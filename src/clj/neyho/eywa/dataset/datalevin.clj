(ns neyho.eywa.dataset.datalevin
  (:require
    [clojure.set]
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
      nil)
    (assoc schema :db/cardinality :db.cardinality/one)
    (if (not= constraint "unique") schema (assoc schema :db/unique :db.unique/identity))))


(defn relation->schema
  [{:keys [cardinality]}]
  {:valueType :db.type/ref
   :cardinality (case cardinality
                  ("m2o" "o2m" "m2m") :db.cardinality/many
                  :db.cardinality/one)})


(defn from-relation-key [{:keys [euuid]}] (keyword (str euuid) "from"))
(defn to-relation-key [{:keys [euuid]}] (keyword (str euuid) "to"))


(defn entity->schema
  [model {:keys [attributes] :as entity}]
  (let [relations (core/get-entity-relations model entity)]
    (reduce
      (fn [r {:keys [euuid] :as attribute}]
        (let [schema (attribute->schema attribute)]
          (assoc r (keyword (str euuid)) schema)))
      (reduce
        (fn [r relation]
          (assoc r
                 (from-relation-key relation) (relation->schema relation)
                 (to-relation-key relation) (relation->schema relation)))
        {:euuid {:db/ident :eywa/euuid
                 :db/valueType :db.type/uuid
                 :db/unique :db.unique/identity}}
        relations)
      attributes)))


(defn datalevin-schema
  []
  (let [model (dataset/deployed-model)
        entities (core/get-entities model)]
    (reduce merge (map (partial entity->schema model) entities))))


(def conn (d/get-conn "/tmp/eywa/datalevin_db" (datalevin-schema)))


(defn build-schema
  ([] (build-schema (dataset/deployed-model)))
  ([model]
   (let [entities (core/get-entities model)]
     (reduce
       (fn [r {:keys [attributes]
               entity-euuid :euuid
               :as entity}]
         (let [attributes (conj attributes
                                {:euuid iu/modified-on :name "modified_by" :type "user"}
                                {:euuid iu/modified-at :name "modified_on" :type "timestamp"})]
           (assoc r entity-euuid
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
                                relation-euuid :euuid}]
                          (let [f (when from-label (csk/->snake_case_keyword from-label))
                                t (when to-label (csk/->snake_case_keyword to-label))
                                cardinality' (case cardinality
                                               ("m2o" "o2m" "m2m") :many
                                               :one)
                                recursive? (= from-euuid to-euuid)]
                            (cond
                              ;;
                              (and t (= to-euuid entity-euuid))
                              (assoc r f
                                     {:key (keyword (str from-euuid "." to-euuid) (str relation-euuid))
                                      :euuid relation-euuid
                                      :type :relation
                                      :recursive? recursive?
                                      :reverse? false
                                      :cardinality cardinality'
                                      :ref from-euuid})
                              ;;
                              (and f (= from-euuid entity-euuid))
                              (assoc r t
                                     {:key (keyword (str from-euuid "." to-euuid) (str "_" relation-euuid))
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
       entities))))


(comment
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
  (let []
    (get-in *schema* [entity relation :ref])))


(defn reverse-relation-key
  [entity attribute]
  (get-in *schema* [entity attribute :key/reverse]))






(defn tmp-key [] (nano-id 10))


(defn analzye
  "For given entity and data, function will produce
  delta structure that can be used to transact! or
  execute SQL query to insert/delete data"
  ([entity data] (analzye entity data true))
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
                              (update-in result [:relations/many euuid] (fnil conj #{}) [id nil])
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
                                          [:relations/many euuid] (fnil conj #{})
                                          [id rid]))
                                      to
                                      (assoc data :tmp/id rid))))
                                result
                                data))
                            ;; If there is nil input don't touch it
                            ;; This will mark deletion
                            :one
                            (if (nil? data)
                              (update-in result [:relations/one euuid] (fnil conj #{}) [id nil])
                              (let [relation-indexes (get-indexes data constraints)
                                    rid (get-id result to relation-indexes)]
                                (transform-object
                                  (->
                                    result
                                    (update-in
                                      [:index to] merge
                                      (zipmap relation-indexes (repeat rid)))
                                    (update-in
                                      [:relations/one euuid] (fnil conj #{})
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
  ([model {:keys [entity releation/many relation/one reference] :as delta}]
   (let [[_ id-mapping] (reduce-kv
                          (fn [[current-id result] _ records]
                            (let [ks (keys records)
                                  next-max-id (- current-id (count ks))
                                  next-ids (range current-id next-max-id -1)]
                              [next-max-id
                               (merge result (zipmap ks next-ids))]))
                          [-1 nil]
                          entity)
         transformation-keys (memoize
                               (fn [entity]
                                 (reduce-kv
                                   (fn [r k {t :key}]
                                     (assoc r k t))
                                   nil
                                   (get *schema* entity))))]
     (as-> {:changes []
            :retractions []}
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
                                   (assoc (clojure.set/rename-keys record (transformation-keys entity))
                                          :db/id (get id-mapping id)))))
                   (update :retractions
                           (fn [current]
                             (reduce
                               (fn [current [field]]
                                 (conj current
                                       [:db/retract (get id-mapping id) (get (transformation-keys entity) field)]))
                               current
                               retractions))))))
             transactions
             records))
         transactions
         entity)
       ;; Then link references and relations
       (reduce-kv
         )))))


(comment
  (count users)
  (def delta (analzye entity users))
  (delta->transaction delta)
  (sort (vals *1))
  (def model (dataset/deployed-model))
  (-> data first)
  (def entity neyho.eywa.iam.uuids/user)
  (count users)
  (def entity (core/get-entity model neyho.eywa.iam.uuids/user))
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
  (d/transact!
    conn
    [{:db/id -1
      :euuid #uuid "5476910c-950d-11ee-afdf-02a535895d2d",
      :56a8a49a-4125-4c96-8ab1-49e15c9b6e49 "Manager"}
     {:euuid #uuid "9088ac2d-5d4d-46af-be13-659739026b9d",
      :b9d88982-7d35-4b26-813a-a8d0365c68d6 "Miljenko",
      :6a44cf06-d72c-4930-b2d2-3d607ebf5a04 {"language" "hr"},
      :466b811e-0ec5-4871-a24d-5b2990e6db3d/from [3]
      :466b811e-0ec5-4871-a24d-5b2990e6db3d/to [3]
      :db/id -1}
     {:db/id -2
      :euuid #uuid "c5a67922-351e-4ca3-95c2-fa52a7a3e2b5",
      :name "EYWA"}
     {:db/id -3
      :euuid #uuid "67722bb7-6037-4f1b-aec0-3d0c092b6ba0",
      :b9d88982-7d35-4b26-813a-a8d0365c68d6 "test_mirko",
      :6a44cf06-d72c-4930-b2d2-3d607ebf5a04 {"locale" "en"},
      :2c5684ac-d8e1-40a9-8a4b-db602052907f
      "bcrypt+sha512$5638b71a81b1c5da21d80cf6b1a26b0a$12$d56bc5541dc8a447700c84786841d21400e447cc8e357405",
      :466b811e-0ec5-4871-a24d-5b2990e6db3d/from [-100]}])
  (d/close conn)
  (time
    (d/q
      '[:find [(pull ?e [*]) ...]
        :where
        [?e :b9d88982-7d35-4b26-813a-a8d0365c68d6 ?name]]
      (d/db conn)))
  
  (d/transact! conn [[:db/retract 1 :466b811e-0ec5-4871-a24d-5b2990e6db3d/from]])
  (d/q
    '[:find [(pull ?e [*]) ...]
      :where
      [?e :56a8a49a-4125-4c96-8ab1-49e15c9b6e49 ?name]]
    (d/db conn))
  (get (d/schema conn) :466b811e-0ec5-4871-a24d-5b2990e6db3d/from)
  (get (d/schema conn) :466b811e-0ec5-4871-a24d-5b2990e6db3d/to)

  (defn list-all-attributes [conn]
    (d/q '[:find ?attr
           :where
           [?e :db/ident ?attr]
           [:db.part/db :db.install/attribute ?e]]
         conn))
  )
