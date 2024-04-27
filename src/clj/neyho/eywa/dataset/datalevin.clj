(ns neyho.eywa.dataset.datalevin
  (:require
    [clojure.walk :as walk]
    [clojure.string :as str]
    [clojure.set :refer [rename-keys]]
    [nano-id.core :refer [nano-id]]
    [camel-snake-kebab.core :as csk]
    [datalevin.core :as d]
    [neyho.eywa.iam.uuids :as iu]
    [neyho.eywa.dataset :as dataset]
    [neyho.eywa.dataset.core :as core]))





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
      ("hashed" "encrypted" "string") {:db/valueType :db.type/string}
      ("user" "group" "role") (:db/valueType :db.type/ref)
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
  ([{:keys [euuid]
     {from-euuid :euuid} :from
     {to-euuid :euuid} :to}
    reverse?]
   (keyword  (str (if reverse? "_" "") euuid))
   #_(keyword (str from-euuid "." to-euuid)  (str (if reverse? "_" "") (uuid-to-base26 euuid)))))



(defn relation->schema
  [{:keys [cardinality] :as relation}]
  {(relation-key relation)
   {:db/valueType :db.type/ref
    :db/cardinality (case cardinality
                      ("m2o" "o2m" "m2m") :db.cardinality/many
                      :db.cardinality/one)}})


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
        (map relation->schema relations)))))


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
                                    {:euuid iu/modified-on :name "modified_by" :type "user"}
                                    {:euuid iu/modified-at :name "modified_on" :type "timestamp"})]
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
                                ("user" "group" "role")
                                {:key (attribute-key attribute)
                                 :type :ref
                                 :ref (case type
                                        "user" iu/user
                                        "group" iu/user-group
                                        "role" iu/user-role)}
                                {:key (attribute-key attribute)
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
                                              :one)
                               recursive? (= from-euuid to-euuid)]
                           (swap! field-mapping assoc
                                  (relation-key relation false) f
                                  (relation-key relation true) t)
                           (cond
                             ;;
                             (and t (= to-euuid entity-euuid))
                             (assoc r f
                                    {:key (relation-key relation false)
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


(defn tmp-key [] (nano-id 10))


(defn analyze
  "For given entity and data, function will produce
  delta structure that can be used to transact! or
  execute SQL query to insert/delete data"
  ([entity data] (analyze entity data true))
  ([entity data stack?]
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
                      (update-in [:entity entity-euuid id] (if stack? merge (fn [_ v] v)) fields-data)
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
                              reference-data (keep
                                               (fn [ks]
                                                 (when (every? #(contains? data %) ks)
                                                   (select-keys data ks)))
                                               (get-constraints reference-entity-euuid))]
                          (reduce
                            (fn [r data]
                              (update-in
                                r
                                [:reference reference-entity-euuid data]
                                (fnil conj [])
                                [reference-entity-euuid id field]))
                            result
                            reference-data)))
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


(defn delta->transaction
  ([delta] (delta->transaction (dataset/deployed-model) delta))
  ([model {:keys [entity relations/many relations/one reference] :as delta}]
   (let [;[_ id-mapping] (reduce-kv
         ;                 (fn [[current-id result] _ records]
         ;                   (let [ks (keys records)
         ;                         next-max-id (- current-id (count ks))
         ;                         next-ids (range current-id next-max-id -1)]
         ;                     [next-max-id
         ;                      (merge result (zipmap ks next-ids))]))
         ;                 [-1 nil]
         ;                 entity)
         transformation-keys (memoize
                               (fn [entity]
                                 (reduce-kv
                                   (fn [r k {t :key}]
                                     (assoc r k t))
                                   nil
                                   (get *schema* entity))))]
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
             (fn [transactions data records]
               (reduce
                 (fn [transactions [_ id field]]
                   (update transactions :references
                           (fn [references]
                             (conj references
                                   (assoc (rename-keys data (transformation-keys entity))
                                          (get (transformation-keys entity) field)
                                          id)))))
                 transactions
                 records))
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
                                 (conj references
                                       {:db/id from-id
                                        (get (transformation-keys entity) attribute)
                                        to-ids})))))
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
                             (conj references
                                   {:db/id from-id
                                    (get (transformation-keys entity) attribute) to-id}))))
                 transactions
                 records))
             transactions
             refs-by-attribute))
         transactions
         many)))))


(defn insert-transactions
  [entity data]
  (let [{many-relations :relations/many
         one-relations :relations/one
         :keys [changes retractions references]} (delta->transaction (analyze entity data))]
    (as-> changes transactions
      ; concat transactions retractions)
      (concat transactions references)
      (concat transactions many-relations)
      (concat transactions one-relations))))


(comment
  (count users)
  (def delta (analyze entity users))
  (data->transactions entity users)
  *schema*
  (data->transactions entity users)
  (d/transact! conn (insert-transactions entity users))
  (d/close conn)
  (d/q
    '[:find [(pull ?e [*]) ...]
      :where
      [?e :b9d88982-7d35-4b26-813a-a8d0365c68d6 ?name]]
    (d/db conn))
  (d/pull
    (d/db conn)
    ['* {:466b811e-0ec5-4871-a24d-5b2990e6db3d '[*]}] 44)
  (time
    (<-keys
      (d/pull
        (d/db conn)
        ['* {:_466b811e-0ec5-4871-a24d-5b2990e6db3d
             '[* {:466b811e-0ec5-4871-a24d-5b2990e6db3d [*]}]}] 106)))



  (def users
    (dataset/search-entity
      neyho.eywa.iam.uuids/user
      nil
      {:euuid nil
       :name nil
       :settings nil
       :password nil
       :avatar nil
       :modified_on nil
       :modified_by [{:selections
                      {:euuid nil :name nil}}]
       :roles [{:selections
                {:euuid nil :name nil :avatar nil}}]}))
  (transform-object neyho.eywa.iam.uuids/user-role (first roles))
  (def roles
    (dataset/search-entity
      neyho.eywa.iam.uuids/user-role
      nil
      {:euuid nil
       :name nil
       :users [{:selections
                {:euuid nil :name nil}}]})))



(comment
  (time
    )
  
  (d/transact! conn [[:db/retract 1 :466b811e-0ec5-4871-a24d-5b2990e6db3d/from]])
  (d/q
    '[:find [(pull ?e [*]) ...]
      :where
      [?e :56a8a49a-4125-4c96-8ab1-49e15c9b6e49 ?name]]
    (d/db conn))
  (get )
  (get (d/schema conn) :466b811e-0ec5-4871-a24d-5b2990e6db3d/to)

  (defn list-all-attributes [conn]
    (d/q '[:find ?attr
           :where
           [?e :db/ident ?attr]
           [:db.part/db :db.install/attribute ?e]]
         conn))
  )
