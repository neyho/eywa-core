(ns neyho.eywa.db.postgres.util
  (:require
    clojure.instant
    clojure.string
   [clojure.data.json :as json])
  (:import 
    [org.postgresql.util PGobject]))


(defn jsonb-field? [data]
  (and 
    (instance? PGobject data) 
    (#{"jsonb" "json"} (.getType data))))

(defn data->json [data]
  (if (jsonb-field? data) 
    data
    (doto (PGobject.)
      (.setType "jsonb")
      (.setValue (json/write-str 
                   data 
                   :key-fn (fn [data]
                             (if (keyword? data) 
                               (if-let [n (namespace data)]
                                 (str n "/" (name data))
                                 (name data))
                               data)))))))


(def uuid-pattern #"[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[34][0-9a-fA-F]{3}-[89ab][0-9a-fA-F]{3}-[0-9a-fA-F]{12}")


(def date-pattern #"\d{4}-(0[1-9]|1[0-2])-[0-3]\dT[0-2]\d:[0-5]\d:[0-5]\d")


(defn pkey-fn [data]
  (if (re-find #"[a-zA-Z]" data)
    (if (re-find uuid-pattern data)
      data
      (let [[keyword-or-namespace _keyword] 
            (clojure.string/split data #"/")] 
        (if _keyword
          (keyword 
            keyword-or-namespace 
            (clojure.string/replace 
              _keyword
              #"[_\s]+" "-"))
          (keyword
            (clojure.string/replace 
              keyword-or-namespace
              #"[_\s]+" "-")))))
    (read-string data)))


(defn eywa-val-fn
  "Helper function for transforming dates and other objects to Clojure data
   objects"
  [_ data]
  (letfn [(cast-date [date]
            (try
              (clojure.instant/read-instant-date date) 
              (catch Exception _ nil)))]
    (cond
      (and (string? data) (re-find date-pattern data)) (cast-date data)
      (and (string? data) (re-find uuid-pattern data)) (try
                                                         (java.util.UUID/fromString data)
                                                         (catch Throwable _ data))
      (vector? data) (mapv #(eywa-val-fn nil %) data)
      (map? data) (reduce
                    (fn [r [k v]] (assoc r k (eywa-val-fn k v)))
                    {}
                    data)
      :else data)))


(defn json->data 
  ([v & {:keys [keyfn valfn]
         :or {keyfn pkey-fn
              valfn eywa-val-fn}}]
   (if (jsonb-field? v) 
     (when-let [v (.getValue v)]
       (json/read-str 
         v
         :key-fn keyfn
         :value-fn valfn))
     v)))

(defn ->timestamp [date] (when date (java.sql.Timestamp. (.getTime date))))

(defn <-timestamp [text] (when text (java.sql.Timestamp/valueOf text)))

(defn current-time [] (->timestamp (java.util.Date.)))


(comment
  (def d (java.util.Date.))
  (type d)
  (clojure.data.json/write-str d)
  (-> (current-time) ->timestamp str <-timestamp))
