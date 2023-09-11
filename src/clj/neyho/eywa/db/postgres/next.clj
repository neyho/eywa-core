(ns neyho.eywa.db.postgres.next
  (:require
    clojure.string
    [camel-snake-kebab.core :as csk]
    [next.jdbc :as jdbc]
    [next.jdbc.result-set :as rs]
    [next.jdbc.sql :as sql]
    [next.jdbc.quoted
     :refer [postgres]]
    [next.jdbc.sql.builder :as build]
    [next.jdbc.date-time]
    [io.pedestal.log :as log]
    [neyho.eywa.db.postgres.util
     :refer [data->json
             json->data]])
  (:import
    [java.sql ResultSet ResultSetMetaData]))

(defn clear-connections [con db-name]
  (jdbc/execute-one!
    con
    [(str "SELECT pg_terminate_backend(pid) FROM pg_stat_activity WHERE datname='" db-name "';")]))

(defn ->timestamp [date] (when date (java.sql.Timestamp. (.getTime date))))

(defn <-timestamp [text] (when text (java.sql.Timestamp/valueOf text)))


(defn current-time [] (->timestamp (java.util.Date.)))


(def defaults
  {:graphql 
   {:builder-fn (rs/as-maps-adapter
                  rs/as-unqualified-modified-maps
                  (fn [^ResultSet rs
                       ^ResultSetMetaData rsmeta
                       ^Integer i]
                    (condp = (.getColumnTypeName rsmeta i)
                      "jsonb" (json->data 
                                (.getObject rs i) 
                                :keyfn identity) 
                      ;;
                      (.getObject rs i))))
    :table-fn postgres
    :label-fn clojure.string/lower-case 
    :qualifier-fn name
    :column-fn postgres}
   :edn {:builder-fn (rs/as-maps-adapter
                       rs/as-unqualified-modified-maps
                       (fn [^ResultSet rs
                            ^ResultSetMetaData rsmeta
                            ^Integer i]
                         (condp = (.getColumnTypeName rsmeta i)
                           "jsonb" (json->data (.getObject rs i)) 
                           ;;
                           (.getObject rs i))))
         :table-fn postgres
         :label-fn (fn [w]
                    (let [special (re-find #"^_+" w)]
                      (keyword (str special (csk/->kebab-case-string w)))))
         :qualifier-fn (comp clojure.string/lower-case name)
         :column-fn postgres}
   :raw {:builder-fn (rs/as-maps-adapter
                       rs/as-unqualified-modified-maps
                       (fn [^ResultSet rs
                            ^ResultSetMetaData rsmeta
                            ^Integer i]
                         (condp = (.getColumnTypeName rsmeta i)
                           "jsonb" (json->data (.getObject rs i) :keyfn identity) 
                           ;;
                           (.getObject rs i))))
         :table-fn postgres
         :label-fn identity
         :qualifier-fn name
         :column-fn postgres}})

(def raw (:raw defaults))


(defn key-map->postgres [m]
  (reduce-kv
    (fn [r k v]
      (assoc 
        r 
        (clojure.string/replace
          (clojure.string/lower-case (name k))
          #"[ |-]"
          "_")
        #_(csk/->snake_case_string k) 
        (if (map? v) (data->json v) v)))
    {}
    m))


(letfn [(adjust-params [params]
          (map #(if (map? %) (data->json %) %) params))
        (normalize-statement [[q & params :as statement]]
          (if (not-empty params)
            (concat [q] (adjust-params params))
            statement))]
  (defn execute! 
    ([con statement] (execute! con statement nil))
    ([con statement return-type]
     (jdbc/execute!
       con 
       (normalize-statement statement)
       (get defaults return-type raw))))

  (defn execute-one! 
    ([con statement] (execute-one! con statement nil))
    ([con statement return-type]
     (jdbc/execute-one! 
       con (normalize-statement statement) 
       (get defaults return-type raw)))))


(comment
  (def v (vura/date 2024 1 21 13 21))
  (def datasource (-> user/system :postgres :datasource))
  (date-time/read-as-default)
  (java.sql.Timestamp/from (.toInstant (java.util.Date.)))
  (do
    (def euuid (java.util.UUID/randomUUID))
    (with-open [connection (jdbc/get-connection datasource)]
      (jdbc/execute-one!
        connection
        ["insert into task (euuid, message, started) values (?, ?, ?)" euuid, "JRIQOJROjiojfi18288282", (vura/date)]))
    (with-open [connection (jdbc/get-connection datasource)]
      (jdbc/execute-one!
        connection
        [(format "select euuid,message,started from task where euuid='%s'" euuid)])))
  (.toInstant (:task/started response))
  (->
    
    vura/<-local
    vura/value->time))

(defn insert! 
    ([con table key-map] (insert! con table key-map nil))
    ([con table key-map return-type]
     (let [data (key-map->postgres key-map)] 
       (sql/insert! con table data 
                    (assoc 
                      (get defaults return-type raw)
                      :return-keys true)))))

(defn update!
  ([con table key-map] (update! con table key-map {:id (:id key-map)}))
  ([con table key-map where] (update! con table key-map where :raw))
  ([con table key-map where return-type]
   (let [data (key-map->postgres key-map)] 
     (sql/update! con table data where 
                  (assoc
                    (get defaults return-type)
                    :return-keys true)))))


(defn for-upsert!
  [table key-map on-params opts]
  (let [entity-fn (:table-fn opts identity)
        column-fn (:column-fn opts identity)
        columns (map (comp column-fn name) (keys key-map)) 
        places (build/as-? key-map opts)
        columns-sql (clojure.string/join ", " columns)
        values (vals key-map)
        on-values (map (comp column-fn name) on-params)
        on-sql (clojure.string/join ", " on-values)
        do-set (clojure.string/join 
                 ", "
                 (map
                   (fn [column]
                     (str column"=excluded." column))
                   columns))]
    (assert (seq key-map) "key-map may not be empty")
    (->
      [(str "INSERT INTO " (entity-fn (name table))
            " (" columns-sql ")"
            " VALUES (" places ") ON CONFLICT (" on-sql 
            ") DO UPDATE SET  " do-set " RETURNING *"
            (when-let [suffix (:suffix opts)]
              (str " " suffix)))]
      (into values))))

(defn upsert!
  ([con table key-map] (upsert! con table key-map [:id]))
  ([con table key-map on] (upsert! con table key-map on nil))
  ([con table key-map on return-type]
   (let [data (key-map->postgres key-map)
         opts (assoc
                (get defaults return-type raw)
                :return-keys true)
         mutation (for-upsert! table data on opts)] 
     (log/trace
       :mutation/type :upsert
       :mutation/payload mutation)
     (execute-one! con mutation return-type))))


(defn for-upsert-multi!
  [table columns rows on-params opts]
  (let [entity-fn (:table-fn opts identity)
        column-fn (:column-fn opts identity)
        columns (map (comp column-fn name) columns) 
        places (str "(" (build/as-? columns opts) ")")
        columns-sql (clojure.string/join ", " columns)
        on-values (map (comp column-fn name) on-params)
        on-sql (clojure.string/join ", " on-values)
        do-set (clojure.string/join 
                 ", "
                 (map
                   (fn [column]
                     (str column"=excluded." column))
                   columns))]
    (assert (seq columns) "columns not provided")
    (into
      [(str "INSERT INTO " (entity-fn (name table))
            " (" columns-sql ")"
            " VALUES " (clojure.string/join ", " (repeat (count rows) places)) 
            " ON CONFLICT (" on-sql ") DO UPDATE SET " do-set " RETURNING *"
            (when-let [suffix (:suffix opts)]
              (str " " suffix)))]
      cat rows)))

(defn upsert-multi!
  ([con table columns rows] (upsert-multi! con table columns rows [:id]))
  ([con table columns rows on] (upsert-multi! con table columns rows on nil))
  ([con table columns rows on return-type]
   (let [data (reduce
                (fn [r row]
                  (conj r (map #(if (or (map? %) (vector? %)) (data->json %) %) row)))
                []
                rows)
         opts (assoc
                (get defaults return-type raw)
                :return-keys true)] 
     (execute!
       con
       (for-upsert-multi! table columns data on opts)
       return-type))))

(defn query
  "Syntactic sugar over `execute!` to provide a query alias.

  Given a connectable object, and a vector of SQL and its parameters,
  returns a vector of hash maps of rows that match."
  ([connectable sql-params]
   (query connectable sql-params nil))
  ([connectable sql-params return-type]
   (execute! connectable sql-params return-type)))
