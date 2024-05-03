(ns neyho.eywa.dataset.datalevin
  (:require
    [clojure.walk :as walk]
    [clojure.string :as str]
    [clojure.set :refer [rename-keys]]
    [nano-id.core :refer [nano-id]]
    [camel-snake-kebab.core :as csk]
    [datalevin.core :as d]
    [buddy.hashers :as hashers]
    [neyho.eywa.transit :refer [<-transit ->transit]]
    [neyho.eywa.iam.uuids :as iu]
    [neyho.eywa.dataset :as dataset]
    [neyho.eywa.dataset.core :as core]))



(def nil-value ::nil)


(defonce ^:dynamic *schema* nil)


(defn entity-key [{:keys [euuid]}] (keyword (str euuid)))
(defn attribute-key [{:keys [euuid]}] (keyword (str euuid)))


(defn attribute->schema
  [{:keys [constraint type]}]
  (as-> nil schema
    (case type
      "timestamp" {:db/valueType :db.type/instant}
      "uuid" {:db/valueType :db.type/uuid}
      "int" {:db/valueType :db.type/long}
      "float" {:db/valueType :db.type/float}
      "boolean" {:db/valueType :db.type/boolean}
      ("hashed" "encrypted" "string") {:db/valueType :db.type/string}
      ("user" "group" "role") {:db/valueType :db.type/ref
                               :db/cardinality :db.cardinality/one}
      nil)
    (assoc schema :db/cardinality :db.cardinality/one)
    (if (not= constraint "unique") schema (assoc schema :db/unique :db.unique/identity))))


(defn hex-to-bigint [hex]
  (java.math.BigInteger. hex 16))

(defn bigint-to-base26 [bigint]
  (let [alphabet "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
        base (count alphabet)]
    (loop [num bigint
           acc ""]
      (if (zero? num)
        acc
        (recur (quot num base)
               (str (nth alphabet (rem num base)) acc))))))

(defn uuid-to-base26 [uuid-str]
  (-> uuid-str
      (str/replace "-" "")
      hex-to-bigint
      bigint-to-base26))


(defn relation-key
  ([relation] (relation-key relation false))
  ([{:keys [euuid cardinality]
     {from-euuid :euuid} :from
     {to-euuid :euuid} :to} reverse?]
   (if (#{"tree" "o2o"} cardinality)
     (keyword (str euuid))
     (keyword (str euuid) (str (if reverse? to-euuid from-euuid))))))


(defn relation->schema
  [{:keys [cardinality] :as relation}]
  (let [db-cardinality (case cardinality
                         ("m2o" "o2m" "m2m") :db.cardinality/many
                         :db.cardinality/one)]
    (if (#{"tree" "o2o"} cardinality)
      {(relation-key relation false)
       {:db/valueType :db.type/ref
        :db/cardinality db-cardinality}}
      {(relation-key relation false)
       {:db/valueType :db.type/ref
        :db/cardinality db-cardinality}
       (relation-key relation true)
       {:db/valueType :db.type/ref
        :db/cardinality db-cardinality}})))


(defn entity->schema
  [{:keys [attributes]}]
  (reduce
    (fn [r {:keys [euuid] :as attribute}]
      (let [schema (attribute->schema attribute)]
        (assoc r (keyword (str euuid)) schema)))
    nil
    attributes))


(defn datalevin-schema
  []
  (let [model (dataset/deployed-model)
        entities (core/get-entities model)
        relations (core/get-relations model)]
    (reduce
      merge
      (concat
        (map entity->schema entities)
        (map relation->schema relations)
        [{:euuid {:db/valueType :db.type/uuid
                  :db/unique :db.unique/identity}
          :entity {:db/valueType :db.type/uuid}
          (keyword (str iu/modified-by)) {:db/valueType :db.type/ref
                                          :db/cardinality :db.cardinality/one}
          (keyword (str iu/modified-on)) {:db/valueType :db.type/instant
                                          :db/cardinality :db.cardinality/one}}]))))


(def conn (d/get-conn "/tmp/eywa/datalevin_db" (datalevin-schema)))


(defn build-schema
  ([] (build-schema (dataset/deployed-model)))
  ([model]
   (let [entities (core/get-entities model)
         field-mapping (atom nil)
         schema
         (reduce
           (fn [r {:keys [attributes]
                   entity-euuid :euuid
                   :as entity}]
             (let [attributes (conj attributes
                                    {:euuid "euuid" :name "euuid" :type "uuid"}
                                    {:euuid iu/modified-by :name "modified_by" :type "user"}
                                    {:euuid iu/modified-on :name "modified_on" :type "timestamp"})]
               (swap! field-mapping merge
                      (reduce
                        (fn [c {:keys [euuid name]}]
                          (assoc c (keyword (str euuid)) (csk/->snake_case_keyword name)))
                        nil
                        attributes))
               (assoc
                 r
                 entity-euuid
                 (with-meta
                   (reduce
                     (fn [r {:keys [name type] :as attribute}]
                       (assoc r (csk/->snake_case_keyword name)
                              (case type
                                ;;
                                ("user" "group" "role")
                                {:key (attribute-key attribute)
                                 :type :ref
                                 :ref (case type
                                        "user" iu/user
                                        "group" iu/user-group
                                        "role" iu/user-role)}
                                ("hashed")
                                {:key (attribute-key attribute)
                                 :encoder hashers/derive
                                 :scalar type
                                 :type :field}
                                ;;
                                {:key (attribute-key attribute)
                                 :scalar type
                                 :type :field})))
                     (reduce
                       (fn [r {:keys [from-label to-label cardinality]
                               {from-euuid :euuid} :from {to-euuid :euuid} :to
                               relation-euuid :euuid
                               :as relation}]
                         (let [f (when from-label (csk/->snake_case_keyword from-label))
                               t (when to-label (csk/->snake_case_keyword to-label))
                               cardinality' (case cardinality
                                              ("m2o" "o2m" "m2m") :many
                                              "tree" :recursive
                                              :one)
                               recursive? (or
                                            (= from-euuid to-euuid)
                                            (nil? from-euuid)
                                            (nil? to-euuid))]
                           (swap! field-mapping assoc
                                  (relation-key relation false) f
                                  (relation-key relation true) t)
                           (cond
                             (= cardinality' :recursive)
                             (assoc r t {:key (relation-key relation false)
                                         :euuid relation-euuid
                                         :type :relation
                                         :recursive? true
                                         :reverse? false
                                         :cardinality :one
                                         :ref from-euuid})
                             ;;
                             (and t (= to-euuid entity-euuid))
                             (assoc r f
                                    {:key (relation-key relation false)
                                     :_key (relation-key relation true)
                                     :euuid relation-euuid
                                     :type :relation
                                     :recursive? recursive?
                                     :reverse? false
                                     :cardinality cardinality'
                                     :ref from-euuid})
                             ;;
                             (and f (= from-euuid entity-euuid))
                             (assoc r t
                                    {:key (relation-key relation true) 
                                     :_key (relation-key relation true)
                                     :euuid relation-euuid
                                     :type :relation
                                     :recursive? recursive?
                                     :reverse? true
                                     :cardinality cardinality'
                                     :ref to-euuid}))))
                       nil
                       (core/get-entity-relations model entity))
                     attributes)
                   (let [constraints (conj
                                       (vec
                                         (keep
                                           (fn [constraint]
                                             (when (not-empty constraint)
                                               (mapv
                                                 (fn [euuid]
                                                   (csk/->snake_case_keyword (:name (core/get-attribute entity euuid))))
                                                 constraint)))
                                           (core/get-entity-unique-constraints entity)))
                                       [:euuid])]
                     {::constraints constraints})))))
           nil
           entities)]
     (with-meta
       schema
       {::field-mapping @field-mapping}))))



(defn <-keys
  [data]
  (let [keymap (::field-mapping (meta *schema*))]
    (walk/postwalk
      (fn [x]
        (if (map-entry? x)
          (let [[k v] x]
            [(get keymap k k) v])
          x))
      data)))





(comment
  (::attribute-mapping (meta *schema*))
  (map meta (vals *schema*))
  (alter-var-root #'*schema* (fn [_] (build-schema))))


(defn deploy
  [model]
  (alter-var-root #'*schema* (fn [_] (build-schema model))))


;; What is the challange?
;; I'm starting from some entity and i have to distribute all
;; key values to attributes/relations so that i may store it
;; in datalevin or some other datalog store
;; so some sort of schema
(def ^:dynamic *id* -1)


(defn field->key
  [entity attribute]
  (get-in *schema* [entity attribute :key]))



(defn relation->key
  [entity attribute]
  (get-in *schema* [entity attribute :key]))


(defn relation->entity
  [entity relation]
  (get-in *schema* [entity relation :ref]))


(defn reverse-relation-key
  [entity attribute]
  (get-in *schema* [entity attribute :key/reverse]))


(defn entity-schema [entity]
  (get *schema* entity))


(defn tmp-key [] (nano-id 10))


(defn delta
  "For given entity and data, function will produce
  delta structure that can be used to transact! or
  execute SQL query to insert/delete data"
  ([entity data]
   (let [prepare (memoize
                   (fn [entity]
                     (let [mapping (get *schema* entity)
                           [fields relations recursions refs]
                           (reduce-kv
                             (fn [[fields relations recursions refs] k {t :type :as relation}]
                               (case t
                                 :relation (if (:recursive? relation)
                                             [fields relations ((fnil conj #{}) relations k) refs]
                                             [fields ((fnil conj #{}) relations k) recursions refs])
                                 :ref [fields relations recursions ((fnil conj #{}) refs k)]
                                 :field [((fnil conj #{}) fields k) relations recursions refs]
                                 ))
                             [nil nil nil nil]
                             mapping)]
                       {:fields (conj fields :euuid)
                        :constraints (::constraints (meta mapping))
                        :recursions recursions
                        :relations relations
                        :refs refs})))
         get-constraints (memoize (fn [euuid] (::constraints (meta (get *schema* euuid)))))
         get-field-schema (memoize (fn [euuid k] (get-in *schema* [euuid k])))]
     (letfn [(get-indexes [data constraints]
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
                (let [{:keys [constraints fields relations refs recursions]} (prepare entity-euuid)
                      ;;
                      data (shallow-snake (dissoc data :tmp/id))
                      ;;
                      fields-data (select-keys data fields)
                      ;;
                      ;; Check if there are some changes to this record
                      ;; other than constraints
                      indexes (remove empty? (map #(select-keys fields-data %) constraints))
                      ;;
                      id (or
                           (some #(get-in result [:index entity-euuid %]) indexes)
                           id)
                      ;;
                      [root parents-mapping]
                      (letfn [(normalize-value [v]
                                (select-keys (shallow-snake v) constraints))]
                        (reduce-kv
                          (fn [[r c] k v]
                            (if (nil? v)
                              [(assoc r k nil) c]
                              [r (assoc c k (normalize-value v))]))
                          [nil nil]
                          (select-keys data recursions)))
                      relations-data (select-keys data relations)
                      ;; Check if reference is in form of map (object)
                      ;; or if it is already resolved, that is if db/id
                      ;; is already known
                      {:keys [references-data
                              resolved-references]}
                      (reduce-kv
                        (fn [r k v]
                          (if (map? v)
                            (assoc-in r [:references-data k] v)
                            (assoc-in r [:resolved-references k] v)))
                        {:references-data nil
                         :resolved-references nil}
                        (select-keys data refs))
                      ;; Merge original field data with root recursive relations
                      ;; and resolved references (for which we know db/id)
                      fields-data (merge fields-data root resolved-references)]
                  (as->
                    ;;
                    (->
                      result
                      (update-in [:entity entity-euuid id] merge fields-data)
                      (update-in [:index entity-euuid] merge (zipmap indexes (repeat id))))
                    result
                    ;;
                    ;; Add recursions
                    ;; For recursions only save constraint data
                    ;; directly to entity and mark recursion link
                    ;; under :recursion in form [table key parent] #{children}
                    (reduce-kv
                      (fn [result k data]
                        (let [parent-indexes (get-indexes data constraints)
                              pid (get-id result entity-euuid parent-indexes)]
                          (->
                            result
                            (update-in [:recursion entity-euuid k pid] (fnil conj #{}) id)
                            (update-in [:index entity-euuid] merge (zipmap parent-indexes (repeat pid)))
                            (update-in [:entity entity-euuid pid] merge data))))
                      result
                      parents-mapping)
                    ;; Add references
                    (reduce-kv
                      (fn [result field data]
                        (let [{reference-entity-euuid :ref} (get-field-schema entity-euuid field) 
                              ref-constraints (get-constraints reference-entity-euuid)
                              relation-indexes (get-indexes data ref-constraints)
                              reference-id (get-id result reference-entity-euuid relation-indexes)]
                          (->
                            result
                            (update-in
                              [:index reference-entity-euuid] merge
                              (zipmap relation-indexes (repeat reference-id)))
                            (assoc-in
                              [:reference entity-euuid reference-entity-euuid id]
                              [field reference-id]))))
                      result
                      references-data)
                    ;; Add relations
                    (reduce-kv
                      (fn [result k data]
                        (let [{to :ref
                               :keys [cardinality euuid]} (get-field-schema entity-euuid k)
                              constraints (get-constraints to)]
                          (case cardinality
                            :many
                            (if (or (empty? data) (nil? data))
                              (update-in result [:relations/many euuid {:entity entity-euuid :attribute k}]
                                         (fnil conj #{}) [id nil])
                              (reduce
                                (fn [result data]
                                  (let [relation-indexes (get-indexes data constraints)
                                        rid (get-id result to relation-indexes)]
                                    ;; For found rid that marks 
                                    (transform-object
                                      (->
                                        result
                                        (update-in
                                          [:index to] merge
                                          (zipmap relation-indexes (repeat rid)))
                                        (update-in
                                          [:relations/many euuid {:entity entity-euuid :attribute k}] (fnil conj #{})
                                          [id rid]))
                                      to
                                      (assoc data :tmp/id rid))))
                                result
                                data))
                            ;; If there is nil input don't touch it
                            ;; This will mark deletion
                            :one
                            (if (nil? data)
                              (update-in result [:relations/one euuid {:entity entity-euuid :attribute k}]
                                         (fnil conj #{}) [id nil])
                              (let [relation-indexes (get-indexes data constraints)
                                    rid (get-id result to relation-indexes)]
                                (transform-object
                                  (->
                                    result
                                    (update-in
                                      [:index to] merge
                                      (zipmap relation-indexes (repeat rid)))
                                    (update-in
                                      [:relations/one euuid {:entity entity-euuid :attribute k}] (fnil conj #{})
                                      [id rid]))
                                  to
                                  (assoc data :tmp/id rid)))))))
                      result
                      relations-data)))))]
       (if (sequential? data)
         (let [data (map #(assoc % :tmp/id (tmp-key)) data)]
           (reduce
             #(transform-object %1 entity %2)
             {:root (mapv :tmp/id data)
              :entity/euuid entity}
             data))
         (let [data (assoc data :tmp/id (tmp-key))]
           (transform-object
             {:root (:tmp/id data)
              :entity/euuid entity}
             entity data)))))))


(defn delta->transactions
  ([delta] (delta->transactions (dataset/deployed-model) delta))
  ([model {:keys [entity relations/many relations/one reference]}]
   (let [transformation-keys (memoize
                               (fn [entity]
                                 (reduce-kv
                                   (fn [r k {t :key}]
                                     (assoc r k t))
                                   nil
                                   (get *schema* entity))))
         backreference (memoize
                         (fn [entity field]
                           (get-in *schema* [entity field :_key])))]
     (as-> {:changes []
            :retractions []
            :references []
            :relations []}
       transactions
       ;; First handle entity insertion and retraction parts
       (reduce-kv
         (fn [transactions entity records]
           (reduce-kv
             (fn [transactions id record]
               (let [{changes false
                      retractions true} (group-by (comp nil? val) record)
                     record (into {} changes)]
                 (->
                   transactions 
                   (update :changes
                           (fn [current]
                             (conj current
                                   (assoc (rename-keys record (transformation-keys entity))
                                          :entity entity
                                          :db/id id))))
                   (update :retractions
                           (fn [current]
                             (reduce
                               (fn [current [field]]
                                 (conj current
                                       [:db/retract id (get (transformation-keys entity) field)]))
                               current
                               retractions))))))
             transactions
             records))
         transactions
         entity)
       ;; Then link references and relations
       (reduce-kv
         (fn [transactions entity ref-by-constraints]
           (reduce-kv
             (fn [transactions _ mapping]
               (reduce-kv
                 (fn [transactions record-id [field ref-id]]
                   (update transactions :references
                           (fn [references]
                             (conj references {:db/id record-id (get (transformation-keys entity) field) ref-id}))))
                 transactions
                 mapping))
             transactions
             ref-by-constraints))
         transactions
         reference)
       ;; Relations
       (reduce-kv
         (fn [transactions _ refs-by-attribute]
           (reduce-kv
             (fn [transactions {:keys [entity attribute]} records]
               (let [grouped (group-by first records)]
                 ;; TODO - add retractions here
                 (reduce-kv
                   (fn [transactions from-id records]
                     (let [to-ids (map second records)]
                       (update transactions :relations/many
                               (fn [references]
                                 (concat references
                                         [{:db/id from-id (get (transformation-keys entity) attribute) to-ids}]
                                         (map
                                           (fn [to-id]
                                             {:db/id to-id (backreference entity attribute) from-id})
                                           to-ids))))))
                   transactions
                   grouped)))
             transactions
             refs-by-attribute))
         transactions
         many)
       (reduce-kv
         (fn [transactions _ refs-by-attribute]
           (reduce-kv
             (fn [transactions {:keys [entity attribute]} records]
               ;; TODO - add retractions here
               (reduce
                 (fn [transactions [from-id to-id]]
                   (update transactions :relations/one
                           (fn [references]
                             (let [references (conj
                                                references
                                                {:db/id from-id
                                                 (get (transformation-keys entity) attribute) to-id})])
                             references)))
                 transactions
                 records))
             transactions
             refs-by-attribute))
         transactions
         one)))))


(defn insert-transactions
  [entity data]
  (let [{many-relations :relations/many
         one-relations :relations/one
         :keys [changes retractions references]} (delta->transactions (delta entity data))]
    (as-> changes transactions
      ; concat transactions retractions)
      (concat transactions references)
      (concat transactions many-relations)
      (concat transactions one-relations))))


(defn retraction-transactions
  [entity data]
  )


(comment
  (count users)
  (def entity iu/user)
  (def delta (delta entity users))
  (delta->transactions (delta entity users))
  *schema*
  (delta entity users)
  (insert-transactions entity users)
  (def result (d/transact! conn (insert-transactions entity users)))
  (drop 500 (:tx-data result))
  (d/close conn)
  (get (d/schema conn) (keyword (str iu/modified-by)))

  (def users
    (dataset/search-entity
      neyho.eywa.iam.uuids/user
      nil
      {:euuid nil
       :name nil
       :settings nil
       :password nil
       :avatar nil
       :active nil
       :modified_on nil
       :modified_by [{:selections {:euuid nil :name nil}}]
       :roles [{:selections
                {:euuid nil :name nil :avatar nil}}]}))
  (def roles
    (dataset/search-entity
      neyho.eywa.iam.uuids/user-role
      nil
      {:euuid nil
       :name nil
       :users [{:selections
                {:euuid nil :name nil}}]})))


(defn freeze [data]
  (if (string? data) data (->transit data)))


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


(letfn [(gen-field []
          (symbol (str "?field_" (nano-id 5))))
        (gen-entity []
          (symbol (str "?entity_" (nano-id 5))))]
  (defn selection->schema
    ([entity selection]
     (selection->schema entity nil selection))
    ([entity args selection]
     (let [schema (entity-schema entity)
           selection (flatten-selection selection)
           selection-keys (set (keys selection))
           ;;
           {fields :field
            refs :ref
            relations :relation
            recursions :recursion} (group-by (comp :type val) schema)
           fields (into {} fields)
           refs (into {} refs)
           relations (reduce into {} [relations recursions refs])
           fields (reduce-kv
                    (fn [r _ {k :key}]
                      (assoc r k (gen-field)))
                    nil
                    (select-keys fields selection-keys))
           [arg-fields args] (let [f (volatile! nil)
                                   args' (walk/postwalk
                                           (fn [x]
                                             (if (map-entry? x)
                                               (let [[k v] x]
                                                 (if-some [field (get-in schema [k :key])]
                                                   (do
                                                     (when-not (contains? @f field)
                                                       (vswap! f assoc field (get fields field (gen-field))))
                                                     [field v])
                                                   [k v]))
                                               x))
                                           args)]
                               [@f args'])]
       *schema*
       {:entity/symbol (gen-entity) 
        :entity entity
        :args args
        :args/fields arg-fields
        :selection/fields fields
        ; :refs (reduce-kv
        ;         (fn [r field {k :key
        ;                       entity :ref}]
        ;           (let [{[{:keys [args selections]}] field} selection]
        ;             (assoc r k (selection->schema entity args selections))))
        ;         nil
        ;         (select-keys refs selection-keys))
        :relations (reduce-kv
                     (fn [r field {k :key
                                   entity :ref
                                   c :cardinality
                                   :or {c :one}}]
                       (let [{[{:keys [args selections]}] field} selection]
                         (assoc r k (assoc (selection->schema entity args selections)
                                           :cardinality c))))
                     nil
                     (select-keys relations selection-keys))}))))


(defn schema->filter-clauses
  ([schema] (schema->filter-clauses [] schema))
  ([result {:keys [args/fields relations]
            entity :entity/symbol}]
   (as-> result filters
     (reduce-kv
       (fn [r k s]
         (conj r [entity k s]))
       filters
       fields)
     (reduce-kv
       (fn [r k {relation :entity/as :as schema'}]
         (into
           (conj r [entity k relation])
           (schema->filter-clauses schema')))
       filters
       relations))))


(defn schema->find-mapping
  ([schema] (schema->find-mapping nil schema))
  ([result {:keys [relations]
            entity-uuid :entity
            entity-symbol :entity/as}]
   (as-> result result
     (assoc result entity-symbol entity-uuid)
     (reduce-kv
       (fn [r _ {to-entity-symbol :entity/as
                 to-entity-uuid :entity
                 :as schema'}]
         (merge
           (assoc r to-entity-symbol to-entity-uuid)
           (schema->find-mapping schema')))
       result
       relations))))


(defn find-mapping->clause
  [mapping]
  (into [:find] (keys mapping)))


(defn compose-query
  [{find-mapping :find
    filters :filter}]
  `[~@(find-mapping->clause find-mapping)
    :in ~'$
    :where
    ~@filters])


(defn targeting-args? [args]
  (when args
    (if (vector? args)
      (some targeting-args? args)
      (let [args' (dissoc args :_offset :_limit)
            some-constraint? (not-empty (dissoc args' :_and :_or :_where :_maybe))]
        (if some-constraint?
          true
          (some
            targeting-args?
            ((juxt :_and :_or :_where :_maybe) args')))))))


(defn targeting-schema? [{:keys [args fields relations counted? aggregate]}]
  (or
    counted?
    aggregate
    (targeting-args? args)
    (some targeting-args? (vals fields))
    (some targeting-schema? (vals relations))))


; (defn keyword-field->type 
;   [schema field]
;   (let [keymap (::field-mapping (meta *schema*))
;         k (get keymap field)]
;     (when k
;       (get-in schema []))))


(defn search-entity-roots
  ([{root-symbol :entity/symbol
     root-entity :entity
     :as schema}]
   (letfn [(join-stack
             ([{:keys [relations args] entity-symbol :entity/symbol
                field-symbols :args/fields}]
              (reduce-kv
                (fn [[entities stack entity-cardinality] rel rel-schema]
                  (let [{child-symbol :entity/symbol
                         cardinality :cardinality} rel-schema
                        [entities' stack'] (join-stack rel-schema)
                        args' (dissoc args :_distinct :_offset :_limit :_order_by)]
                    (if (not-empty args')
                      [(into (conj entities child-symbol) entities')
                       (into
                         (conj stack [entity-symbol rel child-symbol])
                         stack')
                       (assoc entity-cardinality child-symbol cardinality)]
                      [(into (conj entities child-symbol) entities')
                       (into
                         (conj stack (list
                                       'or-join [entity-symbol child-symbol]
                                       [entity-symbol rel child-symbol]
                                       (list 'and
                                             [(list 'missing? '$ entity-symbol rel)]
                                             [(list 'ground 0) child-symbol])))
                         stack')
                       (assoc entity-cardinality child-symbol cardinality)])))
                ;; entity list
                [[]
                 ;; statements
                 (letfn [(->datalog [stack args]
                           (reduce-kv
                             (fn [r k conditions]
                               (let [field-symbol (get field-symbols k)]
                                 (case k
                                   ;;
                                   :_where (into r (->datalog stack (:_where args)))
                                   :_limit r
                                   :_order_by r
                                   :_offset r
                                   :_distinct r
                                   (reduce-kv
                                     (fn [r condition v]
                                       (case condition
                                         :_boolean
                                         (conj r
                                               (case v
                                                 ;;
                                                 ("NOT_TRUE" :NOT_TRUE)
                                                 (list 'or
                                                       (list 'not [entity-symbol k true])
                                                       [(list 'missing? '$ entity-symbol k)])
                                                 ;;
                                                 ("NOT_FALSE" :NOT_FALSE)
                                                 [(list 'or
                                                        [entity-symbol k true]
                                                        [(list 'missing? '$ entity-symbol k)])]
                                                 ;;
                                                 ("TRUE" :TRUE)
                                                 [entity-symbol k true]
                                                 ;;
                                                 ("FALSE" :FALSE)
                                                 (list 'not [entity-symbol k true])
                                                 ;;
                                                 ("NULL" :NULL)
                                                 [(list 'missing? '$ entity-symbol k)]))
                                         ;;
                                         :_eq (conj r [entity-symbol k v])
                                         ;;
                                         :_neq (conj r (list 'not [entity-symbol k v]))
                                         ;;
                                         (:_lt :_gt :_le :_ge)
                                         (let [
                                               stack' ((fnil conj []) r
                                                       [entity-symbol k field-symbol]
                                                       [(list (case condition
                                                                :_gt '>
                                                                :_lt '<
                                                                :_ge '>=
                                                                :_le '<=)
                                                              field-symbol v)])]
                                           stack')
                                         ;;
                                         :_in
                                         (conj r
                                               (conj
                                                 (map
                                                   (fn [x]
                                                     [entity-symbol k x])
                                                   v)
                                                 'or))
                                         ;;
                                         :_not_in
                                         (into r
                                               (map
                                                 (fn [x]
                                                   (list 'not [entity-symbol k x]))
                                                 v))
                                         ;; Default
                                         [entity-symbol k :neznam]))
                                     r
                                     conditions))))
                             []
                             args))]
                   (->datalog [] args))
                 ;; entity cardinality
                 nil]
                relations)))]
     (let [[entities statements entity-cardinality] (join-stack schema)
           statements (concat [[root-symbol :entity root-entity]] statements)
           entities (into [(:entity/symbol schema)] entities)
           query `[:find ~@entities
                   :in ~'$
                   :where
                   ~@statements]
           _ (println "QUERY")
           _ (println query)
           roots (d/q query (d/db conn))
           to-pull (reduce merge
                           (map-indexed
                             (fn [idx entity]
                               {entity (distinct (map #(nth % idx) roots))})
                             entities))
           entity-idx (memoize (fn [entity] (.indexOf entities entity)))
           references (letfn [(get-links [entity relation-symbol]
                                (let [eidx (entity-idx entity)
                                      ridx (entity-idx relation-symbol)
                                      cardinality (get entity-cardinality relation-symbol)]
                                  (reduce
                                    (fn [r record]
                                      (let [ev (get record eidx)
                                            rv (get record ridx)]
                                        (if (= :many cardinality)
                                          (update r ev (fnil conj []) rv)
                                          (assoc r ev rv))))
                                    nil
                                    roots)))
                              (group-references
                                ([schema] (group-references nil schema))
                                ([result {entity :entity/symbol
                                          :keys [relations]}]
                                 (reduce-kv
                                   (fn [result _ {relation :entity/symbol :as relation-schema}]
                                     (let [first-level (get-links entity relation)
                                           deeper (group-references relation-schema)]
                                       (as-> result result
                                         (assoc-in result [entity relation] first-level)
                                         (merge-with merge result deeper))))
                                   result
                                   relations)))]
                        (group-references schema))]
       {:link references
        :pull to-pull}))))


(defn pull-roots
  ([{entity :entity/symbol :as schema
     {:keys [_limit _offset]
      :or {_limit 1000
           _offset 0}} :args}
    {data :pull
     references :link}]
   (letfn [(pull [{:keys [selection/fields relations] entity :entity/symbol
                   {:keys [_limit _offset]
                    :or {_limit 1000
                         _offset 0}} :args}]
             (let [entities (d/pull-many
                              (d/db conn)
                              (into [:db/id] (keys fields))
                              ; (get data entity)
                              (take _limit (drop _offset (get data entity))))]
               (reduce-kv
                 (fn [result _ relation-schema]
                   (merge result (pull relation-schema)))
                 {entity (reduce (fn [r d] (assoc r (:db/id d) d)) nil entities)}
                 relations)))]
     (let [db (pull schema)]
       (letfn [(get-record [entity id] (get-in db [entity id]))
               (link [id {:keys [relations] entity :entity/symbol}]
                 (let [record (get-record entity id)]
                   (reduce-kv
                     (fn [record relation {relation-symbol :entity/symbol :as relation-schema
                                           {:keys [_limit _offset]
                                            :or {_limit 1000
                                                 _offset 0}} :args}]
                       (let [to-link-ids (get-in references [entity relation-symbol id])]
                         (cond
                           ;;
                           (or
                             (nil? to-link-ids)
                             (and (number? to-link-ids) (zero? to-link-ids))
                             (and (sequential? to-link-ids) (every? zero? to-link-ids)))
                           (assoc record relation nil)
                           ;;
                           (number? to-link-ids)
                           (assoc record relation (link to-link-ids relation-schema))
                           ;;
                           :else
                           (assoc record relation
                                  (map
                                    #(link % relation-schema)
                                    (to-link-ids))))))
                     record
                     relations)))]
         (reduce
           (fn [result id]
             ((fnil conj []) result (link id schema)))
           nil
           #_(get data entity)
           (take _limit (drop _offset (get data entity)))))))))



(comment
  (time
    (let [schema (selection->schema entity nil selection)
          roots (search-entity-roots schema)]
      (<-keys (pull-roots schema roots))))
  (time
    (do
      (def schema
        (time
          (selection->schema
            neyho.eywa.iam.uuids/user-role
            {:_limit 2}
            {:euuid nil
             :name nil
             :modified_by [{:selections {:name nil}}]
             :users [{:selections {:name nil
                                   :active [{:args {:_ge 0}}]}
                      :args {:_limit 2}}]})))
      (def roots (time (search-entity-roots schema)))
      (time (<-keys (pull-roots schema roots)))))
  (time
    (let [schema (selection->schema
                   neyho.eywa.iam.uuids/user
                   {:euuid nil}
                   {:euuid nil
                    :name nil
                    :active nil
                    :modified_by [{:selections {:name nil}
                                   :args {:_where
                                          {:euuid
                                           {:_in [#uuid "c0e960bc-2515-11ed-8064-02a535895d2d"
                                                      #uuid "fc8716c6-daf1-11ee-a940-02a535895d2d"
                                                      #uuid "7ae30755-f592-4f12-99b0-0ee9fdc88471"]}}}}]})
          roots (search-entity-roots schema)]
      (<-keys (pull-roots schema roots))))
  (<-keys (d/pull (d/db conn) '[*] 115))
  (<-keys (d/pull (d/db conn) '[*] 1))
  ((field->key entity :modified_by) (d/schema conn))
  (def roots (search-entity-roots schema))
  (time (pull-roots schema roots))
  (time (def schema (selection->schema entity {:active {:_eq :FALSE}} selection)))
  (time (def schema (selection->schema #uuid "d304e6d9-07dd-4bc8-9b7f-dc2b289d06a6" selection)))
  (def entity iu/user)
  (concat [1] [2])
  (def selection
    {:euuid nil
     :name nil
     :settings nil
     :active nil
     ;;
     :roles
     [{:selections
       {:euuid nil
        :name nil}
       :args {:_where {:name {:_eq "SUPERUSER"}}}}]})


  schema
  (d/q
    {:query
     '[:find ?entity_YImrn ?entity_mfaGJ ?entity_Eurl8
       :in $
       :where
       [?entity_YImrn :entity #uuid "4778f7b1-f946-4cb2-b356-b9cb336b4087"]
       (or-join [?entity_YImrn ?entity_mfaGJ] [?entity_YImrn :8901e829-7740-4685-90fa-bff807d450e2 ?entity_mfaGJ] (and [(missing? $ ?entity_YImrn :8901e829-7740-4685-90fa-bff807d450e2)] [(ground 0) ?entity_mfaGJ]))
       (or-join [?entity_YImrn ?entity_Eurl8] [?entity_YImrn :466b811e-0ec5-4871-a24d-5b2990e6db3d/edcab1db-ee6f-4744-bfea-447828893223 ?entity_Eurl8] (and [(missing? $ ?entity_YImrn :466b811e-0ec5-4871-a24d-5b2990e6db3d/edcab1db-ee6f-4744-bfea-447828893223)] [(ground 0) ?entity_Eurl8]))
       (not [?entity_Eurl8 :euuid #uuid "c0e960bc-2515-11ed-8064-02a535895d2d"])
       (not [?entity_Eurl8 :euuid #uuid "fc8716c6-daf1-11ee-a940-02a535895d2d"])
       (not [?entity_Eurl8 :euuid #uuid "7ae30755-f592-4f12-99b0-0ee9fdc88471"])]
     :offset 3
     :limit 2
     :args [(d/db conn)]})

  (d/q
    '[:find ?entity_wLJVH ?entity__6NI0 ?entity_FZTrL
      :in $
      :where
      [?entity_wLJVH :entity #uuid "4778f7b1-f946-4cb2-b356-b9cb336b4087"]
      [?entity_wLJVH :8901e829-7740-4685-90fa-bff807d450e2 ?entity__6NI0]
      [?entity_wLJVH :466b811e-0ec5-4871-a24d-5b2990e6db3d/edcab1db-ee6f-4744-bfea-447828893223 ?entity_FZTrL]
      (not [?entity_FZTrL :euuid #uuid "c0e960bc-2515-11ed-8064-02a535895d2d"])
      (not [?entity_FZTrL :euuid #uuid "fc8716c6-daf1-11ee-a940-02a535895d2d"])
      (not [?entity_FZTrL :euuid #uuid "7ae30755-f592-4f12-99b0-0ee9fdc88471"])]
    (d/db conn))
  
  (d/q
    '[:find ?entity
      :in $
      :where
      [?entity :entity #uuid "edcab1db-ee6f-4744-bfea-447828893223"]]
    (d/db conn))
  entity
  (find-mapping->clause find-mapping)
  (schema->filter-clauses schema)
  (schema->find-mapping schema)
  (def selection
    {:euuid nil
     :url nil
     :name nil
     ;;
     :service_locations
     [{:selections
       {:euuid nil
        :name nil}}]
     ;;
     :robots
     [{:selections
       {:_eid nil
        :euuid nil
        :name nil
        :settings nil
        :active nil}
       :args {:_where
              {:_and
               [{:euuid {:_neq nil}
                 :active {:_eq true}}]}}}]}))
