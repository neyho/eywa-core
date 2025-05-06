(ns neyho.eywa.dataset.postgres.query
  (:require
   clojure.set
   [clojure.pprint :as pp]
   [clojure.zip :as zip]
   [clojure.walk :as walk]
   [clojure.core.async :as async]
   [clojure.string :as str]
   [clojure.data.json :as json]
   [nano-id.core :refer [nano-id] :as nano]
   [buddy.hashers :as hashers]
   [clojure.tools.logging :as log]
   [camel-snake-kebab.core :as csk]
   [next.jdbc :as jdbc]
   [next.jdbc.prepare :as p]
   [neyho.eywa.transit :refer [<-transit ->transit]]
   [neyho.eywa.lacinia :refer [deep-merge]]
   [neyho.eywa.iam.uuids]
   [neyho.eywa.iam.access :as access]
   [neyho.eywa.iam.access.context
    :refer [*roles*
            *user*]]
   neyho.eywa.dataset.sql.naming
   [neyho.eywa.db :refer [*db*] :as db]
   [neyho.eywa.db.postgres.next :as postgres]
   [neyho.eywa.dataset
    :refer [deployed-relation
            deployed-entity]]
   [neyho.eywa.dataset.encryption
    :refer [encrypt-data
            decrypt-data]]
   [neyho.eywa.dataset.core
    :refer [*return-type*]
    :as core])
  (:import
   [org.postgresql.util PGobject]
   [java.sql PreparedStatement]))

(defonce ^:private _deployed-schema (atom nil))
(defonce ^:dynamic *operation-rules* nil)

(defn pprint
  [data]
  (with-out-str (pp/pprint data)))

(extend-protocol p/SettableParameter
  ;; Java Time type conversion:
  clojure.lang.Keyword
  (set-parameter [^clojure.lang.Keyword v ^PreparedStatement s ^long i]
    (.setString s i (name v))))

; (def known-errors
;   {"23502" :null_constraint
;    "23505" :unique_violation})

(defn freeze [data]
  (if (string? data) data (->transit data)))

(defn- j-and
  "Function joins statements with 'and'"
  [statements]
  (clojure.string/join " and " statements))

; (defn- j-or
;   "Function joins statements with 'or'"
;   [statements]
;   (clojure.string/join " or " statements))

; (defn deployed-schema []
;   (when-let [{schema :dataset/schema} (meta @dataset/*model*)]
;     schema))

(defn deploy-schema [schema] (reset! _deployed-schema schema))

(defn deployed-schema [] @_deployed-schema)

(defn deployed-schema-entity [entity-id]
  (if-some [entity (get (deployed-schema) entity-id)]
    entity
    (throw
     (ex-info
      "Trying to get schema for entity that is not deployed"
      {:exception/type :entity-not-deployed
       :entity entity-id
       :available (keys (deployed-schema))}))))

(defn distribute-fields
  "Given deployed schema fields returns map
  with :field and :reference split."
  [fields]
  (group-by
   (fn [{:keys [type]}]
     (case type
       ("user" "group" "role") :reference
       :field))
   (vals fields)))

;; EXPERIMENTAL = NEW DB INSERTION
(defn tmp-key [] (nano-id 10))

(defn throw-relation [id [from-euuid _]]
  (let [{{from :name} :from
         {to :name} :to
         :keys [from-label to-label] :as relation} (deployed-relation id)
        [from from-label to to-label] (if (= from-euuid (:euuid (:from relation)))
                                        [from to-label to from-label]
                                        [to from-label from to-label])]
    (throw
     (ex-info
      (format
       "You don't have sufficent privilages to access relation [%s]%s -> %s[%s]"
       from from-label to-label to)
      {:type ::enforce-search-access
       :roles *roles*}))))

(defn throw-entity [id]
  (let [{entity-name :name} (deployed-entity id)]
    (throw
     (ex-info
      (format
       "You don't have sufficent privilages to access entity '%s'"
       entity-name)
      {:type ::enforce-search-access
       :roles *roles*}))))

(defn entity-accessible?
  [entity-id scopes]
  (when-not (access/entity-allows? entity-id scopes)
    (throw-entity entity-id))
  true)

(defn relation-accessible?
  [relation direction scope]
  (let [allowed? (access/relation-allows? relation direction scope)]
    (when-not allowed?
      (throw-relation relation direction))
    true))

(defn analyze-data
  ([entity data] (analyze-data entity data true))
  ([entity data stack?]
   (let [schema (deployed-schema)
         find-entity (memoize (fn [entity] (get schema entity)))
         type-mapping (memoize
                       (fn [{:keys [fields]}]
                         (reduce-kv
                          (fn [result _ {:keys [type key]
                                         ptype :postgres/type}]
                            (assoc result key (or ptype type)))
                          {:euuid "uuid"}
                          fields)))
         reference-mapping (memoize
                            (fn [entity]
                              (let [{:keys [fields]} (find-entity entity)]
                                (reduce
                                 (fn [result {k :key r :postgres/reference}]
                                   (if (some? r)
                                     (assoc result k r)
                                     result))
                                 nil
                                 (vals fields)))))
         get-constraints (memoize
                          (fn [entity]
                            (let [{:keys [fields]
                                   {:keys [unique]} :constraints} (find-entity entity)]
                              (if (or (empty? unique) (every? empty? unique))
                                [[:euuid]]
                                (conj
                                 (mapv
                                  (fn [constraints]
                                    (mapv (fn [e] (get-in fields [e :key])) constraints))
                                  unique)
                                 [:euuid])))))
         now (java.util.Date.)]
     (letfn [(get-indexes [data constraints]
               ;; to find out if ID already exists
               ;; first group constraint data
               ;; and remove empty values
               (remove
                empty?
                (map
                 #(select-keys data %)
                 constraints)))
             (get-id [current table indexes]
               ;; then try to find in current
               ;; result if constraint index exists
               ;; for given table
               (or
                (some
                 #(get-in current [:index table %])
                 indexes)
                 ;; If it doesn't than create new temp key
                (tmp-key)))
             (shallow-snake [data]
               (reduce-kv
                (fn [r k v]
                  (if-not k r
                          (assoc r (csk/->snake_case_keyword k :separator #"[\s\-]") v)))
                nil
                data))
             (transform-object
               ([entity-euuid data]
                (transform-object nil entity-euuid data))
               ([result entity-euuid {:keys [tmp/id] :or {id (tmp-key)} :as data}]
                (entity-accessible? entity-euuid #{:write :owns})
                (let [{:keys [relations fields recursions table]
                       modifier :audit/who
                       modified-on :audit/when
                       :as entity} (find-entity entity-euuid)
                      {references true
                       fields false} (try
                                       (group-by
                                        (fn [definition]
                                          (log/trace "Field definition: " definition)
                                          (contains? definition :postgres/reference))
                                           ;; Remove modifier data from input data
                                           ;; this is controled by platform
                                        (vals (dissoc fields modifier modified-on)))
                                       (catch Throwable e
                                         (log/errorf
                                          "Fields:%s\nModifier: %s\nModified on: %s"
                                          (vals (dissoc fields modifier modified-on))
                                          modifier modified-on)
                                         (throw e)))
                        ;;
                      data (shallow-snake (dissoc data :tmp/id))
                        ;;
                      fields-data (select-keys
                                   data
                                   (conj
                                    (map :key fields)
                                    :euuid))
                      type-mapping (type-mapping entity)
                        ;; Cast data to Postgres
                      [fields-data avatars]
                      (reduce
                       (fn [[fd a] k]
                         (let [t (get type-mapping k)]

                           (if (#{"avatar"} t)
                                ;; If there is avatar than remove it from fields-data
                                ;; and put it to avatars
                             [(dissoc fd k) (assoc a k (get fd k))]
                             (letfn [(->postgres [v]
                                       (log/tracef "[%s]Casting %s to Postgres type %s" k v t)
                                       (when v
                                         (doto (PGobject.)
                                           (.setType t)
                                           (.setValue (name v)))))]
                               [(update
                                 fd k
                                 (case t
                                       ;; Shortcircuit defaults
                                   ("boolean" "string" "int" "float" "timestamp" "timeperiod" "currency" "uuid" "avatar" nil) identity
                                   "json" (fn [data]
                                            (doto (PGobject.)
                                              (.setType "jsonb")
                                              (.setValue (json/write-str
                                                          data
                                                          :key-fn (fn [data]
                                                                    (if (keyword? data)
                                                                      (if-let [n (namespace data)]
                                                                        (str n "/" (name data))
                                                                        (name data))
                                                                      data))))))
                                   "encrypted" (fn [data]
                                                 (doto (PGobject.)
                                                   (.setType "jsonb")
                                                   (.setValue (json/write-str (encrypt-data data)))))
                                   "hashed" hashers/derive
                                   "transit" freeze
                                   ->postgres))
                                a]))))
                       [fields-data nil]
                       (keys fields-data))
                      constraints (get-constraints entity-euuid)
                        ;;
                        ;; Check if there are some changes to this record
                        ;; other than constraints
                      indexes (remove empty? (map #(select-keys fields-data %) constraints))
                        ;;
                      id (or
                          (some #(get-in result [:index table %]) indexes)
                          id)
                      constraint-keys (flatten constraints)
                        ;;
                      {:keys [references-data resolved-references]}
                      (reduce-kv
                       (fn [r k v]
                         (if (map? v)
                           (assoc-in r [:references-data k] v)
                           (assoc-in r [:resolved-references k] v)))
                       {:references-data nil
                        :resolved-references nil}
                       (select-keys data (map :key references)))
                      ;; Remove relation data that isn't allowed
                      valid-relation-keys (let [recursions (set recursions)]
                                            (reduce
                                             (fn [result field]
                                               (if (contains? recursions field)
                                                 result
                                                 (let [{:keys [relation to from]} (get relations field)]
                                                   (cond
                                                     ;;
                                                     (not relation) result
                                                     ;;
                                                     (not (contains? data field)) result
                                                     ;;
                                                     (relation-accessible? relation [from to] #{:write :owns})
                                                     (conj result field)))))
                                             []
                                             (keys relations)))
                        ;;
                      relations-data (when (not-empty valid-relation-keys)
                                       (select-keys data valid-relation-keys))
                        ;;
                      recursions-data (select-keys data recursions)
                        ;;
                      [root parents-mapping]
                      (letfn [(normalize-value [v]
                                (select-keys (shallow-snake v) constraint-keys))]
                        (reduce-kv
                         (fn [[r c] k v]
                           (if (nil? v)
                             [(assoc r k nil) c]
                             [r (assoc c k (normalize-value v))]))
                         [nil nil]
                         recursions-data))
                        ;; root elements are elements that have recursive relation
                        ;; set to nil explicitly
                        ;; since there is no reference to parent, add 
                        ;; this data to fields directly
                      fields-data (merge fields-data root resolved-references)
                        ;;
                      fields-data (if (or
                                       (not-empty references-data)
                                          ;; This part is for removing constraint keys, as in
                                          ;; if somebody is linking entities, than entities aren't
                                          ;; actually changed... Version bellow can cause veird behaviour
                                          ;; when someone actually changes unique attribute. It doesn't
                                          ;; recognise that as change that modifies row. This is quickfix
                                          ;; for now
                                       (not-empty (apply dissoc fields-data [:_eid :euuid]))
                                       #_(not-empty (apply dissoc fields-data constraint-keys)))
                                    (assoc fields-data
                                      modifier (if (map? *user*)
                                                 (:_eid *user*)
                                                 *user*)
                                      modified-on now)
                                    fields-data)]
                  (as->
                      ;;
                   (->
                    result
                    (update-in [:entity table id] (if stack? merge (fn [_ v] v)) fields-data)
                    (assoc-in [:entity/mapping table] entity-euuid)
                    (update-in [:index table] merge (zipmap indexes (repeat id)))
                    (assoc-in [:constraint table] constraints))
                   result
                      ;;
                    (if (empty? avatars) result
                        (update-in result [:avatar table id] merge avatars))
                      ;; Add recursions
                      ;; For recursions only save constraint data
                      ;; directly to entity and mark recursion link
                      ;; under :recursion in form [table key parent] #{children}
                    (reduce-kv
                     (fn [result k data]
                       (let [parent-indexes (get-indexes data constraints)
                             pid (get-id result table parent-indexes)]
                         (->
                          result
                          (update-in [:recursion table k pid] (fnil conj #{}) id)
                          (update-in [:index table] merge (zipmap parent-indexes (repeat pid)))
                          (update-in [:entity table pid] merge data))))
                     result
                     parents-mapping)
                      ;; Add references
                    (reduce-kv
                     (fn [result attribute data]
                       (let [reference-entity-euuid (get
                                                     (reference-mapping entity-euuid)
                                                     attribute)
                             reference-entity (find-entity reference-entity-euuid)
                             reference-data (some
                                             (fn [ks]
                                               (when (every? #(contains? data %) ks)
                                                 (select-keys data ks)))
                                             (get-constraints reference-entity-euuid))]
                         (update-in
                          result
                          [:reference
                           (:table reference-entity)
                           reference-data]
                          (fnil conj [])
                          [(:table entity) id attribute])))
                     result
                     references-data)
                      ;; Add relations
                    (reduce-kv
                     (fn [result k data]
                       (let [{{:keys [to]
                               to-table :to/table
                               rtype :type
                               :as relation} k} relations
                             constraints (get-constraints to)]
                         (case rtype
                           :many
                           (if (or (empty? data) (nil? data))
                             (update-in result [:relations/many relation] (fnil conj #{}) [id nil])
                             (reduce
                              (fn [result data]
                                (let [relation-indexes (get-indexes data constraints)
                                      rid (get-id result to-table relation-indexes)]
                                      ;; For found rid that marks 
                                  (transform-object
                                   (->
                                    result
                                    (update-in
                                     [:index to-table] merge
                                     (zipmap relation-indexes (repeat rid)))
                                    (update-in
                                     [:relations/many relation] (fnil conj #{})
                                     [id rid]))
                                   to
                                   (assoc data :tmp/id rid))))
                              result
                              data))
                              ;; If there is nil input don't touch it
                              ;; This will mark deletion
                           :one
                           (if (nil? data)
                             (update-in result [:relations/one relation] (fnil conj #{}) [id nil])
                             (let [relation-indexes (get-indexes data constraints)
                                   rid (get-id result to-table relation-indexes)]
                               (transform-object
                                (->
                                 result
                                 (update-in
                                  [:index to-table] merge
                                  (zipmap relation-indexes (repeat rid)))
                                 (update-in
                                  [:relations/one relation] (fnil conj #{})
                                  [id rid]))
                                to
                                (assoc data :tmp/id rid)))))))
                     result
                     relations-data)))))]
       ;;
       (if (sequential? data)
         (let [data (map #(assoc % :tmp/id (tmp-key)) data)]
           (reduce
            #(transform-object %1 entity %2)
            {:root (mapv :tmp/id data)
             :root/table (:table (find-entity entity))
             :entity/euuid entity}
            data))
         (let [data (assoc data :tmp/id (tmp-key))]
           (transform-object
            {:root (:tmp/id data)
             :root/table (:table (find-entity entity))
             :entity/euuid entity}
            entity data)))))))

(defn pull-references [tx reference-table references]
  (let [table-constraint-mapping
        (reduce-kv
         (fn [result constraints _]
           (update result
                   (set (keys constraints))
                   (fnil conj [])
                   constraints))
         nil
         references)]
    (reduce-kv
     (fn [result constraint-keys values]
       (let [multi? (> (count constraint-keys) 1)
             pattern (if multi?
                       (str \( (clojure.string/join ", " (repeat (count constraint-keys) \?)) \))
                       "?")
              ;; order is not guaranteed
             columns (map name constraint-keys)
             query (str
                    "select " (str/join ", " (conj columns "_eid"))
                    " from " \" reference-table \" " where "
                    \( (clojure.string/join "," columns) \)
                    " in (" (clojure.string/join ", " (repeat (count values) pattern)) ")")
             values' (if multi?
                       (map (apply juxt constraint-keys) values)
                       (map #(get % (first constraint-keys)) values))]
         (log/tracef
          "[%s]Pulling references %s for values %s\nQuery: %s"
          reference-table
          (str/join ", " constraint-keys)
          (str/join ", " values')
          query)
         (let [data (postgres/execute!
                     tx (into [query] values')
                     core/*return-type*)
               data' (reduce
                      (fn [r d]
                        (assoc r (dissoc d :_eid) (:_eid d)))
                      nil
                      data)]
            ; (log/tracef "Normalized reference data\n%s" (pprint data'))
           (reduce
            (fn [result constraint-data]
              (assoc result constraint-data (get data' constraint-data)))
            result
            values))))
     nil
     table-constraint-mapping)))

(defn prepare-references
  [tx {:keys [reference] :as analysis}]
  (reduce
   (fn [analysis [reference-table pulled-references]]
     (log/tracef "Pulled references for table: %s\n%s" reference-table (pprint pulled-references))
     (reduce-kv
      (fn [analysis constraint value]
        (let [rows (get-in analysis [:reference reference-table constraint])]
          (reduce
           (fn [analysis row]
             (log/tracef
              "[%s]Updating row reference %s"
              row value)
             (assoc-in analysis (concat [:entity] row) value))
           analysis
           rows)))
      analysis
      pulled-references))
   analysis
   (mapv
    (fn [[reference-table references]]
      [reference-table (pull-references tx reference-table references)])
    reference)
    ;; FIXME - When below is used future is deadlocking... I suppose because tx is used
    ;; on same table... so postgres doesn't return anything at all
   #_(let [expected-references (mapv
                                (fn [[reference-table references]]
                                  (future [reference-table (pull-references tx reference-table references)]))
                                reference)]
       (mapv deref expected-references))))

(defn group-entity-rows
  [tmp-rows]
  (reduce-kv
   (fn [result tmp-id data]
     (update result (set (keys data)) (fnil conj []) [data tmp-id]))
   nil
   tmp-rows))

(comment
  (search-entity
   #uuid "0757bd93-7abf-45b4-8437-2841283edcba"
   nil
   {:name nil
    :modified_by [{:selections
                   {:name nil}}]
    :modified_on nil})
  #_(time
     (set-entity
      neyho.eywa.iam.uuids/user
      [{:name "test1" :active true
        :type :PERSON
        :roles [neyho.eywa.data/*ROOT*]}
       {:name "test2" :active true
        :type :PERSON
        :roles [neyho.eywa.data/*ROOT*]}
       {:name "test3" :active true
        :type :PERSON
        :roles [neyho.eywa.data/*ROOT*]}])))

(defn store-entity-records
  [tx {:keys [entity constraint] :as analysis}]
  (reduce-kv
   (fn [analysis entity-table rows]
     (reduce-kv
      (fn [analysis ks rows]
        (log/debugf
         "[%s] Storing entity table rows %s\n%s"
         entity-table (str/join ", " ks) (str/join "\n" rows))
        (let [row-data (map
                           ;; select only field keys
                        (apply juxt ks)
                           ;; Get only row without tempids
                        (map first rows))
              tmp-ids (map second rows)
              columns-fn #(str \" (name %) \")
              ks' (map columns-fn ks)
              values-? (str \( (str/join ", " (repeat (count ks) \?)) \))
                ;;
              constraint (if (contains? ks :euuid)
                           [:euuid]
                           (some
                            #(when (every? ks %) %)
                            (get constraint entity-table)))
                ;;
              on-values (map columns-fn constraint)
                ;;
              query
              (str
               "INSERT INTO \"" entity-table "\" ("
               (str/join ", " ks') ") VALUES "
               (str/join ", " (repeat (count row-data) values-?))
               (when (not-empty on-values)
                 (let [on-sql (str/join ", " on-values)
                       do-set (str/join
                               ", "
                               (map
                                (fn [column]
                                  (str column "=excluded." column))
                                ks'))]
                   (str " ON CONFLICT (" on-sql ") DO UPDATE SET " do-set)))
               " RETURNING _eid, euuid")
              _ (log/tracef
                 "[%s]Storing entity group %s\nData:\n%s\nQuery:\n%s"
                 entity-table constraint (pprint row-data) query)
              result (postgres/execute!
                      tx (into [query] (flatten row-data))
                      *return-type*)
              _ (log/tracef
                 "[%s]Stored entity group result:\n%s"
                 entity-table (pprint result))
              mapping (zipmap tmp-ids result)]
          (log/tracef "[%s]Stored entity group %s" entity-table result)
          (reduce-kv
           (fn [analysis tmp-id data]
             (log/tracef "[%s]Merging updated data %s=%s" entity-table tmp-id data)
             (update-in analysis [:entity entity-table tmp-id] merge data))
           analysis
           mapping)))
      analysis
      (group-entity-rows rows)))
   analysis
   entity))

(defn project-saved-entities
  [{:keys [entity :relations/one :relations/many recursion] :as analysis}]
  (as-> analysis analysis
    ;; Project to one relations
    (reduce-kv
     (fn [analysis
          {from-table :from/table
           to-table :to/table
           :as table}
          ks]
       (assoc-in analysis [:relations/one table]
                 (reduce
                  (fn [result [from to]]
                    (conj result
                          [(get-in entity [from-table from :_eid])
                           (get-in entity [to-table to :_eid])]))
                  []
                  ks)))
     analysis
     one)
    ;; Project to many relations
    (reduce-kv
     (fn [analysis
          {from-table :from/table
           to-table :to/table
           :as table}
          ks]
       (assoc-in analysis [:relations/many table]
                 (reduce
                  (fn [result [from to]]
                    (conj result
                          [(get-in entity [from-table from :_eid])
                           (get-in entity [to-table to :_eid])]))
                  []
                  ks)))
     analysis
     many)
    ;; Project to recursions
    (reduce-kv
     (fn [analysis table recursions]
        ;; focus on recursions
       (reduce-kv
          ;; that are distributed as field parent children depth
        (fn [analysis field bindings]
            ;; Replace current temp ids with real :_eids
          (assoc-in analysis [:recursion table field]
                    (reduce-kv
                     (fn [bindings parent children]
                       (assoc bindings
                         (get-in entity [table parent :_eid])
                         (map #(get-in entity [table % :_eid]) children)))
                     nil
                     bindings)))
        analysis
        recursions))
     analysis
     recursion)))

(defn link-relations
  ([tx analysis] (link-relations tx analysis true))
  ([tx analysis stack?]
   (as-> analysis result
     ;; Link recursions
     (let [{:keys [:recursion]} result]
       (reduce-kv
        (fn [result table mapping]
          (reduce-kv
           (fn [result field bindings]
             (reduce-kv
              (fn [result parent children]
                (try
                  (let [sql (str "insert into \"" table "\" (\"_eid\", \"" (name field) "\") values (?,?) on conflict (_eid) do update set \"" (name field) "\"=excluded." (name field))
                        bindings (partition 2 (interleave children (repeat parent)))
                        statement (jdbc/prepare tx [sql])]
                    (log/tracef
                     "[%s]Adding new recursions %s\n%s"
                     table (apply str bindings) sql)
                    (jdbc/execute-batch! statement bindings (get postgres/defaults *return-type*))
                    result)
                  (catch Throwable e
                    (log/error
                     e
                     "[%s]Couldn't set entity %s references %s.\nRecursion map:\n%s"
                     table
                     parent
                     children
                     recursion))))
              result
              bindings))
           result
           mapping))
        result
        recursion))
     ;; Link single relations
     (let [{:keys [:relations/one]} result]
       (reduce-kv
        (fn [result {:keys [table] to :to/field from :from/field} bindings]
          (let [current (set (map first bindings))
                sql-delete (str
                            "delete from \"" table "\" where \"" from "\" in ("
                            (clojure.string/join ", " (repeat (count current) \?))
                            ")")
                sql-add (jdbc/prepare
                         tx
                         [(str "insert into \"" table "\" (\"" from "\", \"" to "\") values (?,?)")])
                new (filter second bindings)
                deleted (jdbc/execute! tx (into [sql-delete] current))]
            (log/tracef
             "[%s]Deleting old relations\n%s\nDeleted:\n%s"
             table sql-delete deleted)
            (log/tracef
             "[%s]Adding new relations\n%s\nIDS:\n%s"
             table sql-add new)
            (jdbc/execute-batch! sql-add new (get postgres/defaults *return-type*))
            result))
        result
        one))
     ;; Link many relations
     (let [{:keys [:relations/many]} result]
       (reduce-kv
        (fn [result {:keys [table] to :to/field from :from/field} bindings]
          (let [current (set (map first bindings))
                sql-add (jdbc/prepare
                         tx
                         [(str "insert into \"" table "\" (\"" from "\", \"" to "\") values (?,?)"
                               (when stack?
                                 " on conflict do nothing"))])
                new (filter second bindings)]
            (when-not stack?
              (let [query (str
                           "delete from " table " where \"" from "\" in ("
                           (clojure.string/join ", " (repeat (count current) \?))
                           ")")
                    deleted (jdbc/execute! tx (into [query] current))]
                (log/debugf
                 "[%s]Deleting relations\n%s\nDeleted:\n%s"
                 table query deleted)))
            (log/tracef
             "[%s]Adding new relations\n%s"
             table sql-add)
            (jdbc/execute-batch! sql-add new (get postgres/defaults *return-type*))
            result))
        result
        many)))
   analysis))

(defn publish-delta
  [{many-relations :relations/many
    one-relations :relations/one
    entities :entity
    entity-mapping :entity/mapping
    :as analysis}]
  ;; (def analysis analysis)
  (doseq [[{:keys [relation]} delta] many-relations]
    (log/debugf "[Datasets] Publishing relation delta: %s" relation)
    (async/put!
     core/delta-client
     {:element relation
      :delta delta}))
  (doseq [[{:keys [relation]} delta] one-relations]
    (log/debugf "[Datasets] Publishing relation delta: %s" relation)
    (async/put!
     core/delta-client
     {:element relation
      :delta delta}))
  (doseq [[table delta] entities]
    (log/debugf "[Datasets] Publishing entity delta: %s" (get entity-mapping table))
    (async/put!
     core/delta-client
     {:element (get entity-mapping table)
      :delta delta}))
  analysis)

(defn set-entity
  ([entity-id data]
   (with-open [connection (jdbc/get-connection (:datasource *db*))]
     (jdbc/with-transaction [tx connection]
       (set-entity tx entity-id data true))))
  ([entity-id data stack?]
   (with-open [connection (jdbc/get-connection (:datasource *db*))]
     (jdbc/with-transaction [tx connection]
       (set-entity tx entity-id data stack?))))
  ([tx entity-id data stack?]
   #_(do
     (def entity-id entity-id)
     (def data data)
     (def stack? stack?)
     (def roles *roles*)
     (def user *user*))
   (letfn [(pull-roots [{:keys [root entity root/table]}]
             (log/tracef
              "[%s]Pulling root(%s) entity after mutation"
              entity-id root)
             (if (sequential? root)
               (mapv #(get-in entity [table %]) root)
               (get-in entity [table root])))]
     (let [analysis (analyze-data entity-id data stack?)]
       (log/tracef "Storing based on analysis\n%s" (pprint analysis))
       (as-> analysis result
         (prepare-references tx result)
         (store-entity-records tx result)
         (project-saved-entities result)
         (link-relations tx result stack?)
         (publish-delta result)
         (pull-roots result))))))

(comment
  (set-entity
   #uuid "ccdab22c-0fd5-49da-b447-72ab55e596a4"
   {:euuid #uuid "2f1338c2-4659-4c96-8b80-15c01a5362f3"
    :name "dijete"
    :mother {:euuid #uuid "e5a6746c-dabe-4810-9fcf-e08dfb381ecd"
             :name "mama"}
    :father {:euuid #uuid "34d00251-cd25-40be-b68f-6755f6ca1bd1"
             :name "tata"}})
  ; (defn test-m []
  ;   (binding [*roles* roles
  ;             *user* user]
  ;     (set-entity entity-id data)))
  ; (binding [*roles* roles
  ;           *user* user]
  ;   (get-entity entity-id {:euuid #uuid "42472014-2995-11f0-a15c-4d664d75367f"} {:euuid nil :name nil}))
  (binding [*roles* #{; (:euuid neyho.eywa.data/*ROOT*)
                      #uuid "97b95ab8-4ca3-498d-b578-b12e6d1a2df8"
                      #uuid "7fc035e2-812e-4861-a25c-eb172b39577f"}
            *user* 100]
    (binding [*user* {:_eid 3,
                      :name "rgersak",
                      :roles #{#uuid "601ee98d-796b-43f3-ac1f-881851407f34"},
                      :settings nil,
                      :active true,
                      :euuid #uuid "4216dce4-7bec-11ef-9009-73dea08e4c4a",
                      :avatar nil,
                      :sessions #{"EkvySotNCwKiLiwSDwAyGoqPDQdcYF"},
                      :groups #{}}]
      (analyze-data
       neyho.eywa.iam.uuids/user
       [{:euuid #uuid "2f1338c2-4659-4c96-8b80-15c01a5362f3"
         :name "test 1"
         :type :person}
        {:euuid #uuid "83c1b3b6-e4e7-4c7c-8673-ef020e6355d5"
         :name "test 2"
         :type :person
         :service_locations [{:euuid #uuid "61468ae5-7c30-41cd-9cfb-7d31eac02d4a"
                              :name "Location1"}
                             {:euuid #uuid "99fca851-69c5-4541-b7e1-3d59bb9e6b8a"
                              :name "Location2"}]}
        {:euuid #uuid "319b4ded-f8fc-4f1b-8718-128050e06912"
         :name "test 3"
         :roles [{:euuid #uuid "601ee98d-796b-43f3-ac1f-881851407f34"}]}]))))

;;
(defn- flatten-selection [s]
  (reduce
   (fn [r [k v]]
     (assoc r
       (-> k name keyword)
       (let [[{:keys [selections]}] v]
         (case selections
           #:Currency{:amount [nil], :currency [nil]} [nil]
           #:CurrencyInput{:amount [nil], :currency [nil]} [nil]
           #:TimePeriod{:start [nil], :end [nil]} [nil]
           #:TimePeriodInput{:start [nil], :end [nil]} [nil]
           v))))
   nil
   s))

(def scalar-types
  #{"boolean" "string" "int" "float" "timestamp" "enum"
    "timeperiod" "currency" "json" "uuid"
    "encrypted" "hashed" "transit" "avatar"})

(defn selection->schema
  ([entity-id selection]
   (selection->schema entity-id selection nil))
  ([entity-id selection args]
   (entity-accessible? entity-id #{:read :owns})
   (let [{relations :relations
          recursions :recursions
          fields :fields
          modifier :audit/who
          modified-on :audit/when
          _agg :_agg
          table :table} (deployed-schema-entity entity-id)
         selection (flatten-selection selection)
         ;;
         {fields :field
          refs :reference} (distribute-fields fields)
         ;;
         valid-fields (cond->
                       (conj
                        (set
                         (keep
                          (fn [{t :type k :key}]
                            (when (scalar-types t)
                              k))
                          fields))
                        :euuid :_eid)
                        modified-on
                        (conj modified-on))
         scalars (reduce-kv
                  (fn [r k [{args :args}]]
                     ; (log/tracef "Checking if key %s is in valid fields." k)
                    (if (valid-fields k)
                      (assoc r k args)
                      r))
                  {:euuid nil}
                  selection)
         args (reduce-kv
               (fn [args k v]
                 (if (some? v)
                   (assoc args k v)
                   args))
               args
               scalars)
         distinct-on (:_distinct args)
         order-by (:_order_by args)
         order-by-relations (reduce-kv
                             (fn [r k v]
                               (if (map? v) (conj r k) r))
                             #{}
                             order-by)
         distinct-on-relations (reduce-kv
                                (fn [r k v]
                                  (if (map? v) (conj r k) r))
                                #{}
                                distinct-on)
         valid-relations (cond-> (set (keys relations))
                           ;; If there is some modifier than add that relation
                           (some? modifier)
                           (conj modifier)
                           ;;
                           (not-empty refs)
                           (clojure.set/union (set (map :key refs)))
                           ;;
                           (not-empty order-by-relations)
                           (clojure.set/union order-by-relations)
                           ;;
                           (not-empty distinct-on-relations)
                           (clojure.set/union distinct-on-relations)
                           ;; If there are some recursions add that relations as well
                           (not-empty recursions)
                           (clojure.set/union (set recursions)))
         type-mapping (zipmap (map :key fields) (map :type fields))
         decoders (reduce
                   (fn [r k]
                      ; (log/tracef "Checking if key %s is in valid fields." k)
                     (if (valid-fields k)
                       (letfn [(shallow-keywords [data]
                                 (reduce
                                  (fn [r [k v]]
                                    (assoc r (keyword k) v))
                                  nil
                                  data))]
                         (if-let [transform (case (get type-mapping k)
                                               ; "enum" keyword
                                              "transit" <-transit
                                              "encrypted" (fn [data]
                                                            (try
                                                              (decrypt-data (walk/keywordize-keys data))
                                                              (catch Throwable _ nil)))
                                              ("currency" "period") shallow-keywords
                                              nil)]
                           (assoc r k transform)
                           r))
                       r))
                   nil
                   (map key scalars))
         field->type (reduce
                      (fn [result {f :key t :type t' :postgres/type}]
                        (assoc result f (or t' t)))
                      nil
                      fields)
         arg-fields (letfn [(join-args
                              ([args] (join-args args #{}))
                              ([args result]
                               (reduce-kv
                                (fn [result k _]
                                  (reduce clojure.set/union
                                          (if (valid-fields k) (conj result k) result)
                                          (map join-args (vals (select-keys args [:_where :_and :_or :_maybe])))))
                                result
                                args)))]
                      (join-args args))
         encoders (reduce
                   (fn [result field]
                     (let [t (get field->type field)]
                       (case t
                          ;; Shortcircuit defaults
                         ("boolean" "string" "int" "float" "json"
                                    "timestamp" "timeperiod" "currency"
                                    "uuid" "avatar" "hashed" nil) result
                          ; "hashed" (update result field hashers/derive)
                         "transit" (update result field freeze)
                         (assoc result field
                                (fn [v]
                                  (doto (PGobject.)
                                    (.setType t)
                                    (.setValue (name v))))))))
                   nil
                   arg-fields)
         objects (apply dissoc selection (keys scalars))
         ;; Filter relations that are in selection from all possible relations
         ; _ (log/trace
         ;     :message "selection->schema collected"
         ;     :args args
         ;     :selection selection
         ;     :scalars scalars
         ;     :objects objects
         ;     :order-by order-by
         ;     :distinct-on distinct-on)
         narrow-relations (reduce-kv
                           (fn [rs rkey rdata]
                             (if (valid-relations rkey)
                               (if (or
                                    (contains? args rkey)
                                    (contains? objects rkey)
                                    (contains? order-by rkey)
                                    (contains? distinct-on rkey)
                                    (contains? _agg rkey))
                                 (reduce
                                  (fn [final {:keys [selections alias]
                                              new-args :args}]
                                    (let [{:keys [relation from to]} rdata]
                                      ; (def relation relation)
                                      ; (def from from)
                                      ; (def to to)
                                      ; (def direction [from to])
                                      (relation-accessible? relation [from to] #{:read})
                                      (assoc final (or alias rkey)
                                             (merge
                                              (clojure.set/rename-keys rdata {:table :relation/table})
                                              {:relation/as (str (gensym "link_"))
                                               :entity/as (str (gensym "data_"))}
                                              (selection->schema
                                               (:to rdata) selections
                                               (cond-> new-args
                                                   ;;
                                                 (and
                                                  (not= (:from rdata) (:to rdata))
                                                  (contains? args rkey))
                                                 (merge (get args rkey))
                                                   ;;
                                                 (contains? order-by rkey)
                                                 (assoc :_order_by (get order-by rkey))
                                                   ;;
                                                 (contains? distinct-on rkey)
                                                 (assoc :_distinct (get distinct-on rkey))))))))
                                  rs
                                  (get objects rkey))
                                 rs)
                               rs))
                           nil
                            ;; Get all possible relations
                           (cond-> relations
                              ;;
                             modifier
                             (assoc modifier
                               (let [user #uuid "edcab1db-ee6f-4744-bfea-447828893223"
                                     {utable :table} (deployed-schema-entity user)]
                                 {:from entity-id
                                  :from/field (name modifier)
                                  :from/table table
                                  :to user
                                  :to/field "_eid"
                                  :to/table utable
                                  :table table
                                  :type :one}))
                              ;;
                             (not-empty refs)
                             (as-> relations
                                   (reduce
                                    (fn [relations' {:keys [postgres/reference] k :key}]
                                      (let [{ttable :table} (deployed-schema-entity reference)
                                            alias-key (get-in objects [k 0 :alias] k)]
                                        (assoc relations' alias-key
                                               {:args (get-in objects [k 0 :args])
                                                :from entity-id
                                                :from/field (name k)
                                                :from/table table
                                                :to reference
                                                :to/field "_eid"
                                                :to/table ttable
                                                :table table
                                                :type :one})))
                                    relations
                                    refs))
                              ;;
                             (not-empty recursions)
                             (as-> relations
                                   (reduce
                                    (fn [relations' recursion]
                                      (assoc relations' (keyword recursion)
                                             {:args (get-in objects [(keyword recursion) 0 :args])
                                              :from entity-id
                                              :from/field (name recursion)
                                              :from/table table
                                              :to entity-id
                                              :to/field "_eid"
                                              :to/table table
                                              :table table
                                              :type :one}))
                                    relations
                                    recursions))))
         aggregate-keys [:_count :_min :_max :_avg :_sum :_agg]]
     (as-> (hash-map
            :entity/as (str (gensym "data_"))
            :entity/table table
            :fields scalars
            :aggregate (reduce
                        (fn [r {k :key}]
                          (let [[{:keys [args selections]}] (get selection k)
                                selection (flatten-selection selections)]
                            (if (not-empty selections)
                              (assoc r k {:operations (vec (map name (keys selection)))
                                          :args args})
                              r)))
                        nil
                        fields)
            :args args
            :decoders decoders
            :encoders encoders
            :relations narrow-relations
            :recursions recursions) schema
       ;;
       (if-not (some #(contains? selection %) aggregate-keys)
         schema
         (reduce-kv
          (fn [schema operation fields]
            (case operation
               ;;
              :_count
              (reduce
               (fn [schema {operations :selections}]
                 (reduce-kv
                  (fn [schema relation specifics]
                    (let [rkey (keyword (name relation))
                          rdata (get relations rkey)
                          relation (->
                                    rdata
                                    (dissoc :_count)
                                    (dissoc :relations)
                                    (clojure.set/rename-keys {:table :relation/table})
                                    (assoc
                                     :pinned true
                                     :entity/table (:to/table rdata)
                                     :relation/as (str (gensym "link_"))
                                     :entity/as (str (gensym "data_"))))]
                      (case specifics
                           ;;
                        (nil [nil]) (assoc-in schema [:_count rkey] relation)
                           ;;
                        (reduce
                         (fn [schema {:keys [alias args]}]
                           (let [akey (or alias rkey)]
                                 ;; Use original relation
                             (assoc-in schema [:_count akey]
                                           ;; and if there are arguments in _counted
                                           ;; than use that arguments, otherwise use
                                           ;; args from narrowed relation select
                                       (cond-> relation
                                         args (assoc :args (clojure.set/rename-keys args {:_where :_maybe}))))))
                         schema
                         specifics))))
                  schema
                  operations))
               schema
               fields)
               ;;
              :_agg
              (reduce
               (fn [schema {operations :selections}]
                 (reduce-kv
                  (fn [schema relation [{specifics :selections}]]
                    (let [rkey (keyword (name relation))
                          rdata (get relations rkey)
                          relation (->
                                    rdata
                                    (dissoc :_agg)
                                    (dissoc :relations)
                                    (clojure.set/rename-keys {:table :relation/table})
                                    (assoc
                                     :pinned true
                                     :entity/table (:to/table rdata)
                                     :relation/as (str (gensym "link_"))
                                     :entity/as (str (gensym "data_"))))
                          schema (assoc-in schema [:_agg rkey] relation)]
                      (reduce-kv
                       (fn [schema operation [{aggregates :selections}]]
                         (let [operation (keyword (name operation))]
                           (reduce-kv
                            (fn [schema fkey selections]
                              (let [fkey (keyword (name fkey))]
                                (case selections
                                       ;;
                                  (nil [nil]) (assoc-in schema [:_agg rkey operation fkey] [fkey nil])
                                       ;;
                                  (reduce
                                   (fn [schema {:keys [alias args]}]
                                     (let [field (or alias fkey)]
                                       (assoc-in schema [:_agg rkey operation field]
                                                 [fkey (when args {:args args})])))
                                   schema
                                   selections))))
                            schema
                            aggregates)))
                       schema
                       specifics)))
                  schema
                  operations))
               schema
               fields)
               ;;
              (reduce
               (fn [schema {:keys [selections]}]
                 (reduce-kv
                  (fn [schema fkey selections]
                    (let [fkey (keyword (name fkey))]
                      (reduce
                       (fn [schema {:keys [alias args]}]
                         (let [field (or alias fkey)]
                           (assoc-in schema [operation field]
                                     [fkey (when args {:args args})])))
                       schema
                       selections)))
                  schema
                  selections))
               schema
               fields)))
          schema
          (select-keys selection aggregate-keys)))))))

(defn relations-cursor [cursor]
  (if (empty? cursor)
    []
    (vec (concat (interleave (repeat :relations) cursor)))))

(defn schema->cursors
  "Given selection schema produces cursors that point
  to all connected entity tables. This is a way point to
  pull linked data from db with single query"
  ([{:keys [relations] :as schema}]
   (schema->cursors
    (when-let [cursors (keys relations)]
      (mapv vector cursors))
    schema))
  ([cursors schema]
   (reduce
    (fn [cursors cursor]
      (let [{:keys [relations fields counted?]} (get-in schema (relations-cursor cursor))]
        (if (not-empty relations)
          (into
           (conj cursors cursor)
           (mapcat #(schema->cursors [(conj cursor %)] schema) (keys relations)))
          (if (or counted? (some? fields))
            (conj cursors cursor)
            cursors))))
    []
    cursors)))

(defn get-cursor-schema
  ([schema cursor]
   (if (empty? cursor) schema
       (get-in schema (relations-cursor cursor))))
  ([schema cursor reference-table]
   (let [{tt :to/table
          tf :to/field
          :as schema} (get-cursor-schema schema cursor)]
     (if (and
          (= reference-table tt)
          ;; Is not direct binding
          (not= tf "_eid"))
       (clojure.set/rename-keys
        schema
        {:to/field :from/field
         :to/table :from/table
         :from/field :to/field
         :from/table :to/table})
       schema))))

(defn wrap-basic-fields
  ([fields] (wrap-basic-fields fields nil))
  ([fields prefix]
   (if (not-empty prefix)
     (map #(str (when prefix (str prefix \.)) (if (= :euuid %) "euuid" (name %))) (conj fields "_eid"))
     (map #(str \" (if (= :euuid %) "euuid" (name %)) \") (conj fields "_eid")))))

(defn extend-fields
  ([fields] (extend-fields fields nil))
  ([fields prefix]
   (clojure.string/join ", " (wrap-basic-fields fields prefix))))

(defn distinct->sql
  ([{{args :_distinct} :args :as schema}]
   (when args
     (str
      "distinct on ("
      (clojure.string/join
       ", "
       (letfn [(process-distinct [{:keys [entity/as] :as schema} {:keys [attributes] :as args}]
                 (reduce-kv
                  (fn [result field distinct-on]
                    (if (empty? distinct-on) result
                        (into
                         result
                         (process-distinct (get-in schema [:relations field]) distinct-on))))
                  (mapv #(vector as %) attributes)
                  (dissoc args :attributes)))]
         (reduce
          (fn [result [table field]]
            (conj result (str (when table (str table \.)) (name field))))
          []
          (process-distinct schema args))))
      \)))))

(defn modifiers-selection->sql
  ([{operators :args :as schema}]
   (let [s (cond-> (list)
             (contains? operators :_offset)
             (conj (str "offset " (get operators :_offset)))
             ;;
             (contains? operators :_limit)
             (conj (str "limit " (get operators :_limit)))
             ;;
             (contains? operators :_order_by)
             (conj
              (str
               "order by "
               (clojure.string/join
                ", "
                (letfn [(process-order-by [{:keys [entity/as] :as schema} order-by]
                          (reduce-kv
                           (fn [result field order-by']
                             (if (keyword? order-by')
                               (do
                                 (log/tracef
                                  "[%s] Modifier %s selection to SQL: %s"
                                  as field order-by')
                                 (conj result [as field order-by']))
                               (into
                                result
                                (process-order-by (get-in schema [:relations field]) order-by'))))
                           []
                           order-by))]
                  (reduce
                   (fn [result [table field order]]
                     (conj result
                           (str (when table (str table \.))
                                (name field)
                                (case order
                                  :desc " desc nulls last"
                                  :asc " asc nulls first"))))
                   []
                   (process-order-by schema (get operators :_order_by))))))))]
     (if (empty? s) "" (clojure.string/join " " s)))))

(def ^:dynamic *ignore-maybe* true)
(def ^:dynamic *deep* true)

;; TODO - IMPORTANT check wyh query-selection->sql is not passing on data
(defn query-selection->sql
  ([schema] (query-selection->sql schema []))
  ([{operators :args encoders :encoders prefix :entity/as relations :relations :as schema} data]
   (let [is-relation? (set (keys relations))]
     (reduce
      (fn [[statements data] [field constraints]]
        (let [field' (if (not-empty prefix) (str prefix \. (name field)) (name field))]
           ;;
          (if (boolean? constraints)
             ;; Check if boolean constraint refers to _distinct
             ;; if it does ignore that operator
            (if (= :_distinct field)
              [statements]
              [(conj statements (str (name field) " = " constraints))])
             ;;
            (if (is-relation? field)
               ;; When specified field is nested relation
              (if-not *deep* [statements data]
                      (let [[statements' data'] (query-selection->sql (get-in schema [:relations field]))]
                        [(into statements statements')
                         (into data data')]))

               ;; Handle fields
              (case field
                ;;
                :_where
                (if-not *deep* [statements data]
                        (let [[statements' data'] (query-selection->sql (assoc schema :args constraints))]
                          [(conj statements [:and statements'])
                           (into data data')]))
                ;; Ignore for now...
                :_maybe
                (if *ignore-maybe* [statements data]
                    (binding [*deep* false]
                      (let [[statements' data'] (query-selection->sql
                                                 (-> schema
                                                     (assoc :args constraints)
                                                     (dissoc :relations)))]
                        [(conj statements [:or statements'])
                         (into data data')])))
                ;; Ignore join
                :_join
                [statements data]
                ;;
                :_and
                (update
                 (reduce
                  (fn [[statements data] [statements' data']]
                    [(into statements statements')
                     (into data data')])
                  [[] data]
                  (map
                   (fn [constraint]
                     (let [schema' (assoc schema :args constraint)]
                       (query-selection->sql schema')))
                   constraints))
                 0
                 (fn [statements']
                   (conj statements
                         (str
                          "("
                          (clojure.string/join
                           " and "
                           statements')
                          ")"))))
                ;;
                :_or
                (update
                 (reduce
                  (fn [[statements data] [statements' data']]
                    [(into statements statements')
                     (into data data')])
                  [[] data]
                  (map
                   (fn [constraint]
                     (let [schema' (assoc schema :args constraint)]
                       (query-selection->sql schema')))
                   constraints))
                 0
                 (fn [statements']
                   (conj statements
                         (str
                          "("
                          (clojure.string/join
                           " or "
                           statements')
                          ")"))))
                 ;; Ignore limit distinct offset
                (:_limit :_offset :_order_by :_distinct)
                [statements data]
                ;; Default handlers
                (if (keyword? constraints)
                  (case constraints
                    :is_null [(conj statements (format "%s is null" field')) data]
                    :is_not_null [(conj statements (format "%s is not null" field')) data])
                  (reduce-kv
                   (fn [[statements' data'] cn cv]
                     (if (or
                          (and
                           (vector? cv)
                           (not-empty cv))
                          (and
                           (not (vector? cv))
                           (some? cv)))
                       (let [statement (case cn
                                         :_in (format "%s in (%s)" field' (clojure.string/join "," (repeat (count cv) \?)))
                                         :_not_in (format "%s not in (%s)" field' (clojure.string/join "," (repeat (count cv) \?)))
                                         :_le (str field' " <= ?")
                                         :_ge (str field' " >= ?")
                                         :_eq (str field' " = ?")
                                         :_neq (str field' " != ?")
                                         :_lt (str field' " < ?")
                                         :_gt (str field' " > ?")
                                         :_like (str field' " like ?")
                                         :_ilike (str field' " ilike ?")
                                         :_limit (str field' " limit ?")
                                         :_offset (str field' " offset ?")
                                         :_boolean (str field' " "
                                                        (case cv
                                                          ("NOT_TRUE" :NOT_TRUE) " is not true"
                                                          ("NOT_FALSE" :NOT_FALSE) " is not false"
                                                          ("TRUE" :TRUE) " is true"
                                                          ("FALSE" :FALSE) " is false"
                                                          ("NULL" :NULL) " is null"))
                                         ;; If nested condition than
                                         (do
                                           (log/errorf
                                            "Nested condition error:\nConstraint: %s\nValue: %s\nSchema:\n%s"
                                            cn
                                            cv
                                            (pprint schema))
                                           (throw (Exception. "Nested problem"))))
                             data (case cn
                                    (:_boolean) data'
                                    (:_in :_not_in) (into data'
                                                          (if-let [e (get encoders field)]
                                                            (map e cv)
                                                            cv))
                                    (conj data' (if-let [e (get encoders field)]
                                                  (e cv)
                                                  cv)))]
                         [(conj statements' statement) data])
                       (case cn
                         :_eq [(conj statements' (format "%s is null" field')) data']
                         :_neq [(conj statements' (format "%s is not null" field')) data']
                         [statements' data'])))
                   [statements data]
                   constraints)))))))
      [[] data]
      operators))))

(defn search-stack-args
  "Function takes table pile and root entity id and produces where statement"
  ([schema] (search-stack-args schema " and "))
  ([schema j]
   (letfn [(args-stack [{:keys [relations] :as schema}]
             (let [[statements data] (query-selection->sql schema)
                   [statements' data']
                   (reduce
                    (fn [[s d] r]
                      (let [[s' d'] (args-stack r)]
                        [(if (not-empty s') (into s s') s)
                         (if (not-empty d') (into d d') d)]))
                    [[] []]
                    (vals relations))]
               [((fnil into []) statements statements')
                ((fnil into []) data data')]))]
     (let [[stack data] (args-stack schema)]
       (log/tracef
        "Computed args stack:\nStack:\n%s\nData:\n%s"
        stack (pprint data))
       (when (not-empty stack)
         [(str/join
           " "
           (map-indexed
            (fn [idx statement]
              (if (vector? statement)
                (let [[j statements] statement
                      op (str/join
                          (case j
                            :or " or "
                            :and " and "))]
                  (str
                   (when-not (zero? idx)
                     (str op \space))
                   (str/join op statements)))
                (str (when-not (zero? idx) j) statement)))
            stack))
          data])))))

;; Maybe add type of join for focused relation in query...
(letfn [(branch? [[_ {:keys [relations]}]] (not-empty relations))
        (get-children [[_ {:keys [relations]}]] relations)
        (make-node [[k node] children]
          [k (update node :relations (fnil conj []) children)])]
  (defn schema-zipper [root] (zip/zipper branch? get-children make-node (clojure.lang.MapEntry. ::ROOT root))))

(defn search-stack-from
  "For given schema function will return FROM statement
  by joining tables in schema based on args available 
  in schema. Intended for locating root records that can
  later be pulled. Returned result is vector of two elements.
  First is sequence of table aliases, and second one is FROM
  statment itself."
  [schema]
  ; (def schema schema)
  (comment
    (println (second *1)))
  (letfn [(targeting-args? [args]
            (when args
              (if (vector? args)
                (some targeting-args? args)
                (let [args' (dissoc args :_offset :_limit)
                      some-constraint? (not-empty (dissoc args' :_and :_or :_where :_maybe :_join))]
                  (if some-constraint?
                    true
                    (some
                     targeting-args?
                     ((juxt :_and :_or :_where :_maybe) args')))))))
          (targeting-schema? [[_ {:keys [args fields pinned]}]]
            (or
             pinned
             (targeting-args? args)
             (some targeting-args? (vals fields))))
          (find-arg-locations
            [zipper]
            (loop [location zipper
                   pinned-locations #{}]
              (if (zip/end? location) pinned-locations
                  (recur
                   (zip/next location)
                   (if (targeting-schema? (zip/node location))
                     (conj pinned-locations location)
                     pinned-locations)))))
          (find-end-locations
            [zipper]
            (let [arg-locations (find-arg-locations zipper)
                  targeted (reduce
                            (fn [locations location]
                              (loop [parent (zip/up location)
                                     locations locations]
                                (if (nil? parent) locations
                                    (recur (zip/up parent) (disj locations parent)))))
                            arg-locations
                            arg-locations)]
              (if-not (empty? targeted) targeted
                      [zipper])))
          (->join [{:keys [args]}]
            (if (:_maybe args) "left"
                (str/lower-case (name (:_join args :INNER)))))]
    (let [zipper (schema-zipper schema)
          {:keys [entity/as entity/table]
           rtable :relation/table
           ras :relation/as
           ttable :to/table
           falias :from/field
           talias :to/field} schema
          ;;
          reference? (= "_eid" talias)
          ;;
          join (->join schema)
          locations (find-end-locations zipper)
          [tables stack] (reduce
                          (fn [[tables stack] location]
                            (loop [[[_ parent] [_ current] :as nodes]
                                   (conj (vec (zip/path location)) (zip/node location))
                                    ;;
                                   tables tables
                                   stack stack]
                              (if (empty? current)
                                 ;; Return final result
                                [(conj tables (:entity/as parent)) stack]
                                 ;; Otherwise recur
                                (let [{:keys [entity/as
                                              entity/table]} parent
                                       ;;
                                      {as-child :entity/as
                                       child-table :entity/table
                                       as-link :relation/as
                                       ff :from/field
                                       tf :to/field
                                       link-table :relation/table} current
                                      join (->join current)
                                      link-sql (if (= table link-table)
                                                 (format
                                                  "%s join \"%s\" %s on %s.%s=%s.%s"
                                                  join child-table as-child as ff as-child "_eid")
                                                 (format
                                                  "%s join \"%s\" %s on %s._eid=%s.%s\n%s join \"%s\" %s on %s.%s=%s.%s"
                                                  join link-table as-link as as-link ff
                                                  join child-table as-child as-link tf as-child "_eid"))
                                      next-tables (conj tables as)
                                      next-stack (conj stack link-sql)]
                                  (recur
                                   (rest nodes)
                                   next-tables
                                   next-stack)))))
                          [[] []]
                          locations)
          stack (distinct stack)]
      [(distinct (mapv keyword (into (cond-> [as] rtable (conj rtable)) tables)))
       (if rtable
         (str
          \" rtable \" " as " ras
          " " join " join " \" ttable \" \space as \space " on "
          ras \. (if reference? falias talias) \= as "._eid"
          \newline (clojure.string/join "\n" stack))
         (str/join "\n" (conj
                          ; stack
                         (distinct stack)
                         (str "\"" table "\" as " as))))])))

(defn focus-order
  "Function will remove nested :_order_by arguments
  and keep only ones defined in schema root entity"
  [{{order-by :_order_by} :args :as schema}]
  (if (some? order-by)
    (reduce
     (fn [s c]
       (if (some? (get-in order-by c)) s
           (update-in s (relations-cursor c) update :args dissoc :_order_by)))
     schema
     (schema->cursors schema))
    schema))

(defn pull-query
  [{:keys [entity/as
           fields]
    talias :to/field
    falias :from/field
    ras :relation/as
    table :entity/table
    :as schema}
   found-records
   parents]
  (log/tracef
   "[%s] Pulling entity for parents\n[%s]\nwith found records [%s]"
   table (str/join ", " parents) (str/join ", " found-records))
  (let [[_ from maybe-data] (search-stack-from schema)
        _ (log/tracef "[%s] Looking for WHERE args" table)
        [where d]  (search-stack-args schema)
        ; _ (log/tracef "[%s] Looking for FOUND args" table)
        ; [found fd] (when-some [found-records (not-empty (keep #(when (some? %) %) found-records))]
        ;              (search-stack-args
        ;               (assoc schema :args
        ;                      ; {:_eid {:_in found-records}})))
        ;                      {:_eid {:_in (long-array found-records)}})))
        _ (log/tracef "[%s] Looking for PARENT args" table)
        [parented pd] (if (= talias "_eid")
                        ;; If direct binding (in entity table)
                        (search-stack-args
                         (assoc schema
                           :args {:_eid {:_in (long-array parents)}}
                                 ; :args {:_eid {:_in parents}}
                           :entity/as ras))
                        ;; Otherwise
                        (search-stack-args
                         (assoc schema
                           :args {(keyword falias) {:_in (long-array parents)}}
                                 ; :args {(keyword falias) {:_in parents}}
                           :entity/as ras)))
        ;; TODO - When using found records it breaks when _limit is set prior in query
        ;; hierarchy... To the point... This will not work if lets say some search query
        ;; is sent that has _limit: 100, because it will return 100 root records with 
        ;; and if there are some _eids in related data it will be limited to 100 in
        ;; found records
        ;; ignore found records so that search is restarted
        ; [where data] [(clojure.string/join " and " (remove nil? [where found parented]))
        ;               (reduce into [] (remove nil? [d fd pd]))]
        [where data] [(clojure.string/join " and " (remove nil? [where parented]))
                      (reduce into [] (remove nil? [d pd]))]
        modifiers (modifiers-selection->sql schema)]
    (into
     [(str "select " (if (= talias "_eid")
                       (str ras "._eid as " falias)
                       (str ras \. falias \, ras \. talias))
           (when-not (empty? fields) (str "," (extend-fields (keys fields) as)))
           \newline "from " from
           (when where (str "\nwhere " where))
           (when modifiers (str \newline modifiers)))]
     ((fnil into []) maybe-data data))))

(defn pull-cursors
  [con {:keys [entity/table] :as schema} found-records]
  (let [zipper (schema-zipper schema)]
    (letfn [(location->cursor [location]
              (let [[field] (zip/node location)]
                (conj (mapv key (zip/path location)) field)))
            (maybe-pull-children [result location]
              (loop [queries []
                     current-location (zip/down location)]
                (if (nil? current-location)
                  (if (empty? queries) result
                      (let [results (map deref queries)]
                        (apply deep-merge results)))
                  (recur
                   (conj queries (future (process-node result current-location)))
                   (zip/right current-location)))))
            (pull-counts [result location]
              (let [[_ {as :entity/as
                        counted :_count
                        :as schema}] (zip/node location)
                    parents (when-let [parent (zip/up location)]
                              (keys (get result (-> parent
                                                    zip/node
                                                    second
                                                    :from/table))))]
                (if-not (contains? schema :_count)
                  nil
                  (future
                    (let [schema (as-> schema schema
                                   (assoc schema :relations
                                          (reduce-kv
                                           (fn [r k _]
                                             (->
                                              r
                                              (assoc-in [k :pinned] true)
                                              (assoc-in [k :args :_join] :LEFT)))
                                           (:_count schema)
                                           (:_count schema)))
                                   (dissoc schema :_count :fields)
                                   (if-not parents schema
                                           (update schema :args assoc-in [:_eid :_in] (long-array parents))))
                          ; _ (def schema schema)
                          [_ from] (search-stack-from schema)
                          ;;
                          [count-selections from-data]
                          (reduce-kv
                           (fn [[statements data] as {etable :entity/as :as schema}]
                             (let [t (name as)
                                   [[[_ [statement]]] statement-data] (binding [*ignore-maybe* false]
                                                                        (-> schema query-selection->sql))]
                               [(conj statements (if (empty? statement)
                                                   (format
                                                    "count(distinct %s._eid) as %s"
                                                    etable t)
                                                   (format
                                                    "count(distinct case when %s then %s._eid end) as %s"
                                                    statement etable t)))
                                (if statement-data (into data statement-data)
                                    data)]))
                           [[] []]
                           counted)
                          ;;
                          ;; [where where-data]  (search-stack-args schema)
                          ;; TODO - ignore where for now, as it should be part of
                          ;; count-selections
                          [where where-data] [nil nil]
                          ;;
                          [query-string :as query]
                          (as->
                           (format
                            "select %s._eid as parent_id, %s\nfrom %s"
                            as (str/join ", " count-selections) from)
                           query
                            ;;
                            (if-not where
                              query
                              (str query \newline "where " where))
                            ;;
                            (str query \newline
                                 (format "group by %s._eid" as))
                            ;;
                            (reduce into [query] (remove nil? [from-data where-data])))
                          ;;
                          _ (log/tracef
                             "[%s] Sending counts aggregate query:\n%s"
                             table query-string)
                          result (postgres/execute! con query *return-type*)]
                      (reduce
                       (fn [r {:keys [parent_id] :as data}]
                         (assoc r parent_id {:_count (dissoc data :parent_id)}))
                       nil
                       result))))))
            (pull-numerics
              [_ location]
              (let [[_ {as :entity/as
                        :as schema}] (zip/node location)
                    schema (dissoc schema :fields)
                    numerics (get schema :_agg)]
                (cond
                  (empty? numerics) nil
                  :else
                  (future
                    (let [aggregate-schema (-> schema
                                               (assoc :relations numerics)
                                               (dissoc :_agg :_count))
                          ;;
                          [numerics-selections numerics-data]
                          (reduce-kv
                           (fn [result rkey {as :entity/as :as rdata}]
                             (reduce-kv
                              (fn [result operation definition]
                                (reduce-kv
                                 (fn [[statements data] target-key [field-key field-args]]
                                   (let [[statement-stack statement-data]
                                         (binding [*ignore-maybe* false]
                                           (-> aggregate-schema
                                               (get-in [rkey operation])
                                               (merge field-args)
                                               query-selection->sql))
                                         j :and
                                         statement (when (not-empty statement-stack)
                                                     (str/join
                                                      " "
                                                      (map-indexed
                                                       (fn [idx statement]
                                                         (if (vector? statement)
                                                           (let [[j statements] statement
                                                                 op (str/join
                                                                     (case j
                                                                       :or " or "
                                                                       :and " and "))]
                                                             (str
                                                              (when-not (zero? idx)
                                                                (str op \space))
                                                              (str/join op statements)))
                                                           (str (when-not (zero? idx) j) statement)))
                                                       statement-stack)))]
                                     [(conj statements
                                            (format
                                             "%s(%s) as %s$%s$%s"
                                             (case operation
                                               :_min "min"
                                               :_max "max"
                                               :_avg "avg"
                                               :_sum "sum")
                                             (if (empty? statement)
                                               (str as "." (name field-key))
                                               (str "case when " statement
                                                    " then " as "." (name field-key)
                                                    " else null end"))
                                             (name rkey) (name operation) (name target-key)))
                                      (if-not statement-data data
                                              (into data statement-data))]))
                                 result
                                 definition))
                              result
                              (select-keys rdata [:_min :_max :_avg :_sum])))
                           [[] []]
                           numerics)
                          [_ from] (search-stack-from aggregate-schema)
                          [where where-data] (search-stack-args aggregate-schema)
                          [query-string :as query]
                          (as->
                           (format
                            "select %s._eid as parent_id, %s\nfrom %s"
                            as (str/join ", " numerics-selections) from)
                           query
                            ;;
                            (if-not where
                              query
                              (str query \newline "where " where))
                            ;;
                            (str query \newline
                                 (format "group by %s._eid" as))
                            ;;
                            (reduce into [query] (remove nil? [numerics-data where-data])))
                          _ (log/tracef
                             "[%s] Sending numerics aggregate query:\n%s"
                             table query-string)
                          result (postgres/execute! con query *return-type*)]
                      (reduce
                       (fn [r {:keys [parent_id] :as data}]
                         (assoc r parent_id
                                (reduce-kv
                                 (fn [r k v]
                                   (let [[rkey operation k] (str/split (name k) #"\$")]
                                     (assoc-in r [(keyword rkey) (keyword operation) (keyword k)] v)))
                                 nil
                                 (dissoc data :parent_id))))
                       nil
                       result))))))
            (process-root [_ location]
              (let [[_ {:keys [entity/table fields
                               decoders recursions entity/as args]}] (zip/node location)
                    expected-start-result (future
                                            {table (apply array-map
                                                          (reduce
                                                           (fn [r d]
                                                             (conj r
                                                                   (:_eid d)
                                                                   (reduce
                                                                    (fn [data [k t]] (update data k t))
                                                                    d
                                                                    decoders)))
                                                           []
                                                           (let [root-query (format
                                                                             "select %s from \"%s\"%s %s"
                                                                             (extend-fields (concat (keys fields) (map name recursions)))
                                                                             table
                                                                             (if-let [records (get found-records (keyword as))]
                                                                               (format
                                                                                " where \"%s\"._eid in (%s) "
                                                                                table
                                                                                (clojure.string/join ", " records))
                                                                               "")
                                                                               ;; TODO - test if this is necessary
                                                                               ;; This maybe obsolete since we already know what recrods
                                                                               ;; to pull and in which order
                                                                             (str
                                                                              (when (= found-records {})
                                                                                (modifiers-selection->sql {:args args}))))]
                                                             (log/tracef "[%s] Root query:\n%s" table root-query)
                                                             (postgres/execute!
                                                              con
                                                              [root-query]
                                                              *return-type*))))})
                    counts (pull-counts nil location)
                    numerics (pull-numerics nil location)]
                (maybe-pull-children
                 (cond->
                  @expected-start-result
                   counts (update-in [::counts [::ROOT]] merge @counts)
                   numerics (update-in [::numerics [::ROOT]] merge @numerics))
                 location)))
            ;;
            (process-related [result location]
              (let [[field {etable :entity/table
                            falias :from/field
                            talias :to/field
                            decoders :decoders
                            args :args
                            as :entity/as
                            ftable :from/table
                            cardinality :type
                            :as schema}] (zip/node location)
                    parents (keys (get result ftable))
                    cursor (location->cursor location)]
                (log/tracef
                 "[%s] Cursor position %s from table %s. Parents:\n%s"
                 table cursor ftable (str/join ", " parents))
                (if (not-empty parents)
                  (let [expected-result
                        (future
                          (let [[_ {ptable :entity/table}] (zip/node (zip/up location))

                                ;;
                                query (pull-query
                                       (update schema :args dissoc :_limit :_offset)
                                       (get found-records (keyword as)) parents)
                                _ (log/tracef
                                   "[%s] Sending pull query:\n%s\n%s"
                                   table (first query) (rest query))
                                relations (cond->
                                           (postgres/execute! con query *return-type*)
                                            ;;
                                            (some #(contains? args %) [:_offset :_limit])
                                            (as-> relations
                                                  (let [grouping (group-by (keyword falias) relations)]
                                                    (vec
                                                     (mapcat
                                                      #(cond->> %
                                                         (:_offset args) (drop (:_offset args))
                                                         (:_limit args) (take (:_limit args)))
                                                      (vals grouping))))))

                                talias' (keyword talias)
                                falias' (keyword falias)
                                data (reduce
                                      (fn [r d]
                                        (assoc r (get d talias')
                                                ;; TODO - Transform data here
                                               (reduce-kv
                                                (fn [data k t] (update data k t))
                                                (dissoc d talias' falias')
                                                decoders)))
                                      nil
                                      relations)
                                result' (update result etable
                                                (fn [table]
                                                  (merge-with merge table data)))]
                            (reduce
                             (fn [r {t talias' f falias'}]
                               (case cardinality
                                 :many
                                 (update-in r [ptable f field] (fnil conj []) [etable t])
                                 :one
                                 (assoc-in r [ptable f field] [etable t])))
                             result'
                             relations)))
                        ;;
                        expected-counts (pull-counts result location)
                        expected-numerics (pull-numerics result location)]
                    (cond->
                     (maybe-pull-children @expected-result location)
                      expected-counts (update-in [::counts cursor] merge @expected-counts)
                      expected-numerics (update-in [::numerics cursor] merge @expected-numerics)))
                  (do
                    (log/tracef
                     "[%s] Couldn't find parents for: %s"
                     etable result)
                    result))))
            ;;
            (process-node [result location]
              (if (= ::ROOT (key (zip/node location)))
                (process-root result location)
                (process-related result location)))]
      ;;
      (let [result (doall (process-node nil zipper))]
        result))))

(defn construct-response
  [{:keys [entity/table recursions] :as schema} {:keys [::counts ::numerics] :as db} found-records]
  (letfn [(reference? [value]
            (vector? value))
          (list-reference? [value]
            (and
             (vector? value)
             (every? vector? value)))
          (narrow [schema]
            (concat
             (keys (:fields schema))
             (keys (:relations schema))
             recursions))
          (get-counts [cursor parent]
            (get-in counts [cursor parent]))
          (get-numerics [cursor parent]
            (when-some [data (get-in numerics [cursor parent])]
              {:_agg data}))
          (pull-reference [[table id] schema cursor]
            (let [data (merge
                        (select-keys (get-in db [table id]) (narrow schema))
                        (get-counts cursor id)
                        (get-numerics cursor id))
                  data' (reduce-kv
                         (fn [data' k v]
                           (cond
                             (list-reference? v)
                             (assoc data' k (mapv
                                             #(pull-reference % (get-in schema [:relations k]) (conj cursor k))
                                             (distinct v)))
                             ;;
                             (and (recursions k) (not= v id) (not= v [table id]))
                             (if (vector? v)
                               (assoc data' k (pull-reference v schema cursor))
                               (assoc data' k (pull-reference [table v] schema cursor)))
                             ;;
                             (and (not (recursions k)) (reference? v))
                             (assoc data' k (pull-reference v (get-in schema [:relations k]) (conj cursor k)))
                             ;;
                             (= v [table id])
                             (assoc data' k ::self)
                             ;;
                             :else data'))
                         data
                         data)]
              (reduce-kv
               (fn [data k v]
                 (if (not= v ::self) data
                     (assoc data k data)))
               data'
               data')))]
    (let [final (mapv
                 (fn [id]
                   (pull-reference [table id] schema [::ROOT]))
                 (reduce
                  (fn [r root]
                    (if (get-in db [table root])
                      (conj r root)
                      r))
                  []
                  (get
                   found-records
                   (keyword ((get-in postgres/defaults [core/*return-type* :label-fn]) (:entity/as schema)))
                   (keys (get db table)))))]
      final)))

(defn pull-roots [con schema found-records]
  ; (log/tracef "[%s] Found records\n%s" "fieoqj" (pprint found-records))
  (binding [*ignore-maybe* false]
    (let [db (pull-cursors con schema found-records)]
      (construct-response schema db found-records))))

(defn schema->aggregate-cursors
  "Given election schema produces cursors that point
  to all connected entity tables. This is a way point to
  pull linked data from db with single query"
  ([{:keys [relations] :as schema}]
   (schema->aggregate-cursors
    (when-let [cursors (keys relations)]
      (mapv vector cursors))
    schema))
  ([cursors schema]
   (reduce
    (fn [cursors cursor]
      (let [{:keys [relations counted? aggregate]} (get-in schema (relations-cursor cursor))]
        (if (not-empty relations)
          (into
           (conj cursors cursor)
           (mapcat #(schema->cursors [(conj cursor %)] schema) (keys relations)))
          (if (or counted? (not-empty aggregate))
            (conj cursors cursor)
            cursors))))
    []
    cursors)))

; (defn shave-schema-relations
;   ([schema]
;    (let [arg-keys (set (keys (:args schema)))
;          relation-keys (set (keys (:relations schema)))
;          valid-keys (clojure.set/intersection arg-keys relation-keys)]
;      (update schema :relations select-keys valid-keys)))
;   ([schema cursor]
;    (letfn [(shave [schema cursor]
;              (if (not-empty cursor)
;                (let [c (butlast cursor)
;                      k [(last cursor)]]
;                  (recur
;                    (->
;                      schema 
;                      (update-in (conj (relations-cursor c) :relations) select-keys k))
;                    c))
;                schema))]
;      (if (not-empty cursor)
;        (shave 
;          (update-in schema (relations-cursor cursor) dissoc :relations)
;          cursor)
;        (dissoc schema :relations)))))

(defn shave-schema-arguments
  ([schema]
   (reduce
    shave-schema-arguments
    schema
    (schema->cursors schema)))
  ([schema cursor]
   (letfn [(shave [schema]
             (->
              schema
              (dissoc :args)
              (update :fields #(zipmap (keys %) (repeat nil)))))]
     (if (some? cursor)
       (update-in schema (relations-cursor cursor) shave)
       (shave schema)))))

(defn search-entity-roots
  ([schema]
   (with-open [connection (jdbc/get-connection (:datasource *db*))]
     (search-entity-roots connection schema)))
  ([connection schema]
   ; (log/tracef "Searching entity roots for schema:\n%s" (pprint schema))
   ;; Prepare tables target table by inner joining all required tables
   ; (def schema schema)
   ; (comment
   ;   (def focused-schema (focus-order schema))
   ;   (search-stack-from focused-schema))
   (let [focused-schema (focus-order schema)
         [[root-table :as tables] from] (search-stack-from focused-schema)
         ;; then prepare where statements and target data
         [where data] (search-stack-args focused-schema)
         ;; select only _eid for each table
         ;; TODO - this is potentially unnecessary... I've thought
         ;; to focus as much as possible roots and pin all records
         ;; at all tables for defined conditions. But this will
         ;; not work on LEFT JOIN, because it will break _limit
         selected-ids (clojure.string/join
                       ", "
                       (map
                        #(str (name %) "._eid as " (name %))
                        tables))
         ; selected-ids (str (name (first tables)) "._eid as " (name (first tables)))
         distinct-on (or (distinct->sql schema)
                         (when (and
                                (contains? (:args focused-schema) :_limit)
                                 ;; BUG - bellow is hotfix for
                                 ;; ERROR: SELECT DISTINCT ON expressions must match initial ORDER BY expressions
                                 ;; Distinct on was inserted so that when left join is used
                                 ;; in combination with _limit, that proper results will
                                 ;; be returned. Left joining with _limit is problematic
                                (not (contains? (:args focused-schema) :_order_by)))
                           (format "distinct on (%s._eid)" (name root-table))))
         ;; Create query
         modifiers (modifiers-selection->sql schema)
         query (as-> (format "select %s %s from %s" (str distinct-on) selected-ids from) query
                 (if (not-empty where) (str query \newline "where " where) query)
                 (if modifiers (str query " " modifiers) query))
         ;; Execute query
         [r :as ids] (if (and
                          (empty? distinct-on)
                          (empty? where)
                          (empty? data)
                          (empty? (get-in schema [:args :_order_by]))
                          (= 1 (count tables)))
                       ;; If schema want's to return whole table
                       ;; return nil to mark that there are no root
                       nil
                       ;; Otherwise try to find roots and if
                       ;; none are found return empty vector
                       (do
                         (log/tracef
                          "Query for roots:\n%s\nData:\n%s"
                          query (pprint data))
                         (postgres/execute!
                          connection (into [query] data)
                          core/*return-type*)))]
     ;; when there are some results
     (if (not-empty r)
       ;; get table keys
       (let [ks (keys r)]
         ;; for given tables take only distinct found keys and
         ;; associate found ids with table key
         (reduce
          (fn [ids' k]
            (assoc ids' k (distinct (map #(get % k) ids))))
          nil
          ks))
       (if (nil? ids) {} nil)))))

(comment
  (def args nil)
  (def entity-id #uuid "edcab1db-ee6f-4744-bfea-447828893223")
  (def schema (time (selection->schema entity-id selection args)))
  (schema->cursors schema)
  (time (search-entity entity-id args selection))

  (def entity-id #uuid "a800516e-9cfa-4414-9874-60f2285ec330")
  (def entity-id #uuid "a800516e-9cfa-4414-9874-60f2285ec330")
  (def relation-id #uuid "cab89384-7b0b-44f0-8d36-fed5bc671ce5")
  ; (binding [*user* user
  ;           *roles* roles]
  ;   #_(access/entity-allows? entity-id #{:read})
  ;   (access/relation-allows? relation-id :chat_message #{:read}))
  ;;
  ; (binding [*operation-rules* #{:read}
  ;           *user* user
  ;           *roles* roles]
  ;   (selection->schema entity-id selection args))
  (core/get-entity (neyho.eywa.dataset/deployed-model) entity-id)
  (time (search-entity entity-id args selection)))

(defn search-entity
  ([entity-id args selection]
   ; (def entity-id entity-id)
   ; (def args args)
   ; (def selection selection)
   ; (def user *user*)
   ; (def roles *roles*)
   (entity-accessible? entity-id #{:read})
   (with-open [connection (jdbc/get-connection (:datasource *db*))]
     (let [schema (binding [*operation-rules* #{:read}]
                    (selection->schema entity-id selection args))
           _ (log/tracef "Searching for entity\n%s" schema)
           roots (search-entity-roots connection schema)]
       (when (some? roots)
         (log/tracef "[%s] Found roots: %s" entity-id (str/join ", " roots))
         (pull-roots connection schema roots))))))

(defn purge-entity
  ([entity-id args selection]
   (with-open [connection (jdbc/get-connection (:datasource *db*))]
     (let [schema (selection->schema entity-id selection args)
           enforced-schema (binding [*operation-rules* #{:owns}]
                             (selection->schema entity-id selection args))]
       ; (log/info
       ;   :entity entity-id
       ;   :args args
       ;   :selection selection
       ;   :schema schema)
       (if (and
            (not= enforced-schema schema)
            (not (access/superuser?)))
         (throw
          (ex-info
           "Purge not allowed. User doesn't own all entites included in purge"
           {:type ::enforce-purge
            :roles *roles*}))
         (let [roots (search-entity-roots connection schema)]
           (if (some? roots)
             (letfn [(construct-statement
                       [table _eids]
                       (log/debugf "[%s]Constructing purge for eids #%d: %s" table (count _eids) (str/join ", " _eids))
                       [(str "delete from \"" table "\" where _eid=any(?)") (long-array _eids)])
                     ; [(str "delete from \"" table "\" where _eid in (select _eid from \"" table "\" where _eid=any(?))") (long-array _eids)])
                     (process-statement [r k v]
                       (conj r (construct-statement k (keys v))))]
               (let [db (pull-cursors connection schema roots)
                     response (construct-response schema db roots)
                     delete-statements (reduce-kv process-statement [] db)]
                 (doseq [query delete-statements]
                   (log/debugf "[%s]Purgin entity rows with %s" entity-id query)
                   (postgres/execute! connection query *return-type*))
                 response))
             [])))))))

(comment
  (def selection
    {:UserGroup/euuid [nil],
     :UserGroup/name [nil],
     :UserGroup/avatar [nil],
     :UserGroup/active [nil],
     :UserGroup/users
     [{:args {:_where {:type {:_eq :PERSON}}},
       :selections
       {:User/euuid [nil],
        :User/name [nil],
        :User/type [nil],
        :User/avatar [nil]}}]})
  (def args {:euuid #uuid "fbcf3bb9-7728-4b80-8b09-b27cca84e663"})
  (def args
    (reduce-kv
     (fn [args k v]
       (assoc args k {:_eq v}))
     nil
     args))
  (def args {:active {:_eq :TRUE}})
  (def selection
    {:euuid nil
     :name nil
     :settings nil
     ;;
     :roles
     [{:selections
       {:euuid nil
        :name nil
        :permissions [{:selections
                       {:euuid nil}
                       :args {:name {:_eq "Delete"}}}]}
       :args {:_where {:name {:_eq "SUPERUSER"}}}}]
     :service_locations [{:selections
                          {:euuid nil
                           :name nil}}]})
  ; (binding [neyho.eywa.iam.access.context/*roles* #{#_(:euuid neyho.eywa.data/*ROOT*)
  ;                                                   #uuid "97b95ab8-4ca3-498d-b578-b12e6d1a2df8"
  ;                                                   ; #uuid "7fc035e2-812e-4861-a25c-eb172b39577f"
  ;                                                   }
  ;           neyho.eywa.dataset.core/*user* 100]
  ;   (time (selection->schema neyho.eywa.iam.uuids/user selection args)))
  (search-stack-from schema)
  ; (search-entity-roots connection schema)
  ; (.close connection)
  )

(defn get-entity
  ([entity-id args selection]
   (assert (some? args) "No arguments to get entity for...")
   (entity-accessible? entity-id #{:read})
   (log/debugf
    "[%s] Getting entity\nArgs:\n%s\nSelection:\n%s"
    entity-id (pprint args) (pprint selection))
   ; (def entity-id entity-id)
   ; (def args args)
   ; (def selection selection)
   (let [args (reduce-kv
               (fn [args k v]
                 (assoc args k {:_eq v}))
               nil
               args)]
     (with-open [connection (jdbc/get-connection (:datasource *db*))]
       (let [schema (binding [*operation-rules* #{:read}]
                      (selection->schema entity-id selection args))
             roots (search-entity-roots connection schema)]
         (when (not-empty roots)
           (let [roots' (pull-roots connection schema roots)
                 response (first roots')]
             (log/tracef
              "[%s] Returning response\n%s"
              entity-id (pprint response))
             response)))))))

(defn get-entity-tree
  [entity-id root on selection]
  (entity-accessible? entity-id #{:read})
  (let [{:keys [entity/table entity/as]
         :as schema} (binding [*operation-rules* #{:read}]
                       (selection->schema entity-id selection))
        on' (name on)
        sql (if (some? root)
              (format
               "with recursive tree as (
                select 
                _eid, euuid, %s
                from %s where euuid='%s'
                union
                select o._eid, o.euuid, o.%s
                from %s o
                inner join tree t on t._eid=o.%s
                ) select * from tree"
               on' table root
               on' table on')
              (format "select _eid, euuid from %s" table))]
    (with-open [connection (jdbc/get-connection (:datasource *db*))]
      (log/tracef
       "[%s] Get entity tree roots\n%s"
       entity-id sql)
      (let [roots (map
                   :_eid
                   (postgres/execute!
                    connection
                    [sql]
                    *return-type*))]
        (when (not-empty roots)
          (pull-roots connection schema {(keyword as) roots}))))))

(defn shave-schema-aggregates
  ([schema]
   (reduce
    shave-schema-aggregates
    schema
    (schema->aggregate-cursors schema)))
  ([schema cursor]
   (if (not-empty cursor)
     (let [c (butlast cursor)]
       (if-not c schema
               (recur
                (update-in schema (relations-cursor c) dissoc :counted? :pinned)
                c)))
     (dissoc schema :counted? :pinned))))

(defn search-entity-tree
  "Function searches entity tree and returns results by requested selection."
  [entity-id on {order-by :_order_by :as args} selection]
  (entity-accessible? entity-id #{:read})
  (let [{:keys [entity/table entity/as]
         :as schema}
        (binding [*operation-rules* #{:read}]
          (selection->schema
           entity-id
           selection
           args))
        ;;
        on' (name on)]
    (with-open [connection (jdbc/get-connection (:datasource *db*))]
      (letfn [(targeting-args? [args]
                (when (or
                       (and args (not (vector? args)))
                       (and args (vector? args) (not-empty args)))
                  (if (vector? args)
                    (some targeting-args? args)
                    (let [args' (dissoc args :_offset :_limit)
                          some-constraint? (not-empty (dissoc args' :_and :_or :_where))]
                      (if some-constraint?
                        true
                        (some
                         targeting-args?
                         ((juxt :_and :_or :_where :_maybe) args')))))))
              (targeting-schema? [{:keys [args fields relations]}]
                (or
                 (targeting-args? args)
                 (some targeting-args? (vals fields))
                 (some targeting-schema? (vals relations))))]
        (let [targeting? (targeting-schema? schema)
              targets (when targeting?
                        (when-let [found-roots
                                   (search-entity-roots
                                    connection
                                    (update schema :args dissoc :_distinct :_limit :_offset))]
                          (not-empty (get found-roots (keyword as)))))]
          (cond
            ;; If there some targets are found with search-entity-roots
            (not-empty targets)
            (let [sql (format
                       "with recursive tree(_eid,link,path,cycle) as (
                        select 
                        g._eid, g.%s, array[g._eid], false
                        from %s g where g._eid in (%s)
                        union all
                        select g._eid, g.%s, g._eid || path, g._eid=any(path)
                        from %s g, tree o
                        where g._eid=o.link and not cycle
                        ) select distinct on (_eid) * from tree"
                       on' table (clojure.string/join ", " targets)
                       on' table)
                  tree (postgres/execute! connection [sql] *return-type*)
                  maybe-roots (set (map :_eid tree))
                  roots (map :_eid (remove (comp maybe-roots :link) tree))
                  ranked-selection (fn [roots]
                                     (let [roots' (clojure.string/join ", " roots)]
                                       (if (not-empty order-by)
                                         (format
                                          "(select _eid, %s, row_number() over (%s) as _rank from %s where _eid in (%s))"
                                          on' (modifiers-selection->sql {:args {:_order_by order-by}}) table roots')
                                         (format "(select _eid, %s, _eid as _rank from %s where _eid in (%s))" on' table roots'))))
                  sql-final [(format
                              "with recursive tree(_eid,link,path,prank,cycle) as (
                               select 
                               g._eid, g.%s, array[g._eid],array[g._rank], false
                               from %s g
                               union all
                               select g._eid, g.%s, path || g._eid, prank || g._rank, g._eid=any(path)
                               from %s g, tree o
                               where g.%s =o._eid and not cycle
                               ) select * from tree order by prank asc %s"
                              on' (ranked-selection roots)
                              on' (ranked-selection maybe-roots) on'
                              (modifiers-selection->sql {:args (dissoc args :_order_by)}))]]
              (log/tracef
               "[%s] Get entity tree roots\n%s"
               entity-id  (first sql-final))
              (when (some? roots)
                (pull-roots
                 connection (shave-schema-arguments schema)
                 {(keyword as)
                  (distinct
                   (map
                    :_eid
                    (postgres/execute! connection sql-final *return-type*)))})))
            ;; If schema is targeted but no results are found
            (and targeting? (empty? targets))
            nil
            :else
            ;; If targets aren't found
            (let [ranked-init-selection (if (not-empty order-by)
                                          (format
                                           "(select _eid, %s, row_number() over (%s) as _rank from %s where %s is null)"
                                           on' (modifiers-selection->sql {:args {:_order_by order-by}}) table on')
                                          (format "(select _eid, %s, _eid as _rank from %s where %s is null)" on' table on'))
                  ranked-selection (if (not-empty order-by)
                                     (format
                                      "(select _eid, %s, row_number() over (%s) as _rank from %s)"
                                      on' (modifiers-selection->sql {:args {:_order_by order-by}}) table)
                                     (format "(select _eid, %s, _eid as _rank from %s)" on' table))
                  sql-final [(format
                              "with recursive tree(_eid,link,path,prank,cycle) as (
                               select 
                               g._eid, g.%s, array[g._eid],array[g._rank], false
                               from %s g
                               union all
                               select g._eid, g.%s, path || g._eid, prank || g._rank, g._eid=any(path)
                               from %s g, tree o
                               where g.%s =o._eid and not cycle
                               ) select * from tree order by prank asc %s"
                              on' ranked-init-selection
                              on' ranked-selection on'
                              (modifiers-selection->sql {:args (dissoc args :_order_by)}))]]
              (log/tracef
               "[%s] Get entity tree roots\n%s"
               entity-id  (first sql-final))
              (pull-roots
               connection (shave-schema-aggregates schema)
               {(keyword as)
                (distinct
                 (map
                  :_eid
                  (postgres/execute! connection sql-final *return-type*)))}))))))))

(defn delete-entity
  [entity-id args]
  (let [{:keys [entity/table]} (binding [*operation-rules* #{:delete}]
                                 (selection->schema entity-id nil nil))]
    (boolean
     (when (and (some? args) table)
       (with-open [connection (jdbc/get-connection (:datasource *db*))]
         (let [[statements data] (reduce-kv
                                  (fn [[statements data] k v]
                                    [(conj statements (str (name k) "=?"))
                                     (conj data v)])
                                  [[] []]
                                  args)
               sql (cond->
                    [(format
                      "delete from \"%s\" where %s"
                      table
                      (j-and statements))]
                     (not-empty data) (into data))
               _ (log/tracef
                  "[%s] Deleting entity\n%s"
                  entity-id sql)]
           (postgres/execute! connection sql *return-type*)
           (async/put! core/delta-client {:element entity-id :delta {:delete args}})
           ; (async/put!
           ;   core/client
           ;   {:type :entity/delete
           ;    :entity entity-id
           ;    :args args})
           true))))))

;; FIXME
(defn slice-entity
  ([entity-id args selection]
   (with-open [connection (jdbc/get-connection (:datasource *db*))]
     (slice-entity connection entity-id args selection)))
  ([tx entity-id args selection]
   (letfn [(targeting-args? [args]
             (when args
               (if (vector? args)
                 (some targeting-args? args)
                 (let [args' (dissoc args :_offset :_limit)
                       some-constraint? (not-empty (dissoc args' :_and :_or :_where :_maybe))]
                   (if some-constraint?
                     true
                     (some
                      targeting-args?
                      ((juxt :_and :_or :_where :_maybe) args')))))))]
     (let [{:keys [relations entity/table entity/as] :as schema}
           (selection->schema entity-id selection args)
           enforced-schema (binding [*operation-rules* #{:delete}]
                             (selection->schema entity-id selection args))]
       (if (and
            (not= enforced-schema schema)
            (not (access/superuser?)))
         (throw
          (ex-info
           "User doesn't have :delete rule for some of sliced relations or entities"
           {:type ::enforce-slice
            :roles *roles*}))
         (let [queries (reduce-kv
                        (fn [r k {tt :to/table tf :to/field
                                  ff :from/field
                                  rt :relation/table
                                  tas :entity/as
                                  args' :args :as schema'}]
                          (let [query  (str "delete from " \" rt \")
                                 ;;
                                [where-from from-data] (search-stack-args (dissoc schema :relations))
                                 ;;
                                select-from
                                (when (targeting-args? args)
                                  (format
                                   "(select _eid from \"%s\" as %s where %s)"
                                   table as where-from))
                                 ;;
                                [where-to to-data] (search-stack-args schema')
                                 ;;
                                select-to
                                (when (targeting-args? args')
                                  (format
                                   "(select _eid from \"%s\" as %s where %s)"
                                   tt tas where-to))
                                 ;;
                                where (when (or select-to select-from)
                                        (j-and
                                         (cond-> []
                                           select-from (conj (str ff " in " select-from))
                                           select-to (conj (str tf " in " select-to)))))]
                            (assoc r k (into
                                        [(str query (when (not-empty where) "\nwhere ") where)]
                                        (into from-data to-data)))))
                        nil
                        relations)
               result (reduce-kv
                       (fn [r k query]
                         (assoc r k
                                (try
                                  (log/debugf
                                   "[%s] slicing query:\n%s"
                                   entity-id query)
                                  (postgres/execute! tx query core/*return-type*)
                                   ;; TODO - Enable this
                                   ; (async/put!
                                   ;   core/client
                                   ;   {:type :entity/slice
                                   ;    :entity entity-id
                                   ;    :args args
                                   ;    :selection selection})
                                  true
                                  (catch Throwable e
                                    (log/errorf e "Couldn't slice entity")
                                    false))))
                       nil
                       queries)]
           ; (def relations relations)
           (doseq [[label {:keys [relation] :as slice}] relations]
             (async/put! core/delta-client
                         {:element relation
                          :delta {:slice {label slice}}}))
           result))))))

(extend-type neyho.eywa.Postgres
  db/ModelQueryProtocol
  (db/sync-entity
    [_ entity-id data]
    (set-entity entity-id data false))
  (db/stack-entity
    [_ entity-id data]
    (set-entity entity-id data true))
  (db/slice-entity
    [_ entity-id args selection]
    (slice-entity entity-id args selection))
  (db/get-entity
    [_ entity-id args selection]
    (get-entity entity-id args selection))
  (db/get-entity-tree
    [_ entity-id root on selection]
    (get-entity-tree entity-id root on selection))
  (db/search-entity
    [_ entity-id args selection]
    (search-entity entity-id args selection))
  (db/search-entity-tree
    [_ entity-id on args selection]
    (search-entity-tree entity-id on args selection))
  (db/purge-entity
    [_ entity-id args selection]
    (purge-entity entity-id args selection))
  (db/delete-entity
    [_ entity-id data]
    (delete-entity entity-id data)))

; (extend-type neyho.eywa.Postgres
;   neyho.eywa.dataset.sql.naming/SQLNameResolution
;   (table [_ value]
;     (let [schema (deployed-schema)]
;       (get-in schema [value :table])))
;   (relation [_ table value]
;     (let [{:keys [relations]} (deployed-schema-entity table)]
;       (if (keyword? value)
;         (get-in relations [value :table])
;         (some
;          (fn [[_ {:keys [relation table]}]]
;            (when (= relation value)
;              table))
;          relations))))
;   (relation-from-field [_ table value]
;     (let [{:keys [relations]} (deployed-schema-entity table)]
;       (if (keyword? value)
;         (get-in relations [value :from/field])
;         (some
;          (fn [[_ {:keys [relation :from/field]}]]
;            (when (= relation value)
;              field))
;          relations))))
;   (relation-to-field [_ table value]
;     (let [{:keys [relations]} (deployed-schema-entity table)]
;       (if (keyword? value)
;         (get-in relations [value :to/field])
;         (some
;          (fn [[_ {:keys [relation :to/field]}]]
;            (when (= relation value)
;              field))
;          relations)))))
