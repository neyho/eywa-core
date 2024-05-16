(ns neyho.eywa.dataset.datalevin
  (:require
    [clojure.walk :as walk]
    [clojure.string :as str]
    [clojure.set :refer [rename-keys]]
    [nano-id.core :refer [nano-id]]
    [camel-snake-kebab.core :as csk]
    [datalevin.core :as d]
    [buddy.hashers :as hashers]
    [neyho.eywa.iam.uuids :as iu]
    [neyho.eywa.dataset :as dataset]
    [neyho.eywa.dataset.core :as core]))



(def nil-value ::nil)


(defonce ^:dynamic *schema* nil)
(defonce ^:dynamic syncing? false)


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
     #_(keyword (str euuid) (str (if reverse? to-euuid from-euuid)))
     (keyword (str (if reverse? "_" "") euuid)))))


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
       {:entity/symbol (gen-entity) 
        :entity entity
        :args args
        :args/fields arg-fields
        :selection/fields fields
        :relations (reduce-kv
                     (fn [r field {k :key
                                   entity :ref
                                   :keys [recursive? cardinality]
                                   :or {c :one}}]
                       (let [{[{:keys [args selections]}] field} selection]
                         (assoc r k (assoc (selection->schema entity args selections)
                                           :resursive? recursive?
                                           :cardinality cardinality))))
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


(defn reverse?
             [relation-key]
             (str/starts-with? (name relation-key) "_"))

(defn invert [relation-key]
        (let [k (name relation-key)]
          (if (str/starts-with? k "_")
            (keyword (subs k 1))
            (str "_" k))))


(defn schema->statements
  ([schema] (schema->statements schema false))
  ([{root-symbol :entity/symbol
     root-entity :entity
     :as schema}
    ignore-recursive?]
   (letfn [(join-stack
             ([{:keys [relations args] entity-symbol :entity/symbol
                field-symbols :args/fields}]
              (reduce-kv
                (fn [[entities stack] rel rel-schema]
                  (let [{child-symbol :entity/symbol
                         recursive? :resursive?
                         rel-args :args} rel-schema
                        [entities' stack'] (join-stack rel-schema)
                        args' (dissoc rel-args :_maybe :_distinct :_offset :_limit :_order_by)]
                    (cond
                      (and recursive? ignore-recursive?)
                      [entities stack]
                      ;;
                      (not-empty args')
                      [(into (conj entities child-symbol) entities')
                       (into
                         (conj stack [entity-symbol rel child-symbol])
                         stack')]
                      ;;
                      :else
                      [(into (conj entities child-symbol) entities')
                       (into
                         (conj stack (list
                                       'or-join [entity-symbol child-symbol]
                                       (if (reverse? rel)
                                         [child-symbol (invert rel) entity-symbol]
                                         [entity-symbol rel child-symbol])
                                       (list 'and
                                             [(list 'missing? '$ entity-symbol rel)]
                                             [(list 'ground 0) child-symbol])))
                         stack')])))
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
                                         :_eq (conj r
                                                    [entity-symbol k field-symbol]
                                                    [(list '= field-symbol v)])
                                         ;;
                                         :_neq (conj r
                                                     [entity-symbol k field-symbol]
                                                     [(list 'not= field-symbol v)])
                                         ;;
                                         (:_lt :_gt :_le :_ge)
                                         (let [stack' ((fnil conj []) r
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
     (let [[entities statements entity-cardinality] (join-stack schema)]
       [(into [(:entity/symbol schema)] entities)
        (concat [[root-symbol :entity root-entity]] statements)
        entity-cardinality]))))


(defn search-entity-roots
  ([schema]
   (let [[entities statements entity-cardinality] (schema->statements schema) 
         query `[:find ~@entities
                 :in ~'$
                 :where
                 ~@statements]
         ; _ (println "QUERY: " query)
         roots (d/q query (d/db conn))]
     {:entities entities
      :entity-cardinality entity-cardinality
      :roots roots})))



(defn incremental-pull-roots
  "Function will pull roots by traversing schema and pulling entity attributes
  and entity relations in recursive manner using cache while pulling. This is ok
  when using _limit and _offset arguments, but not ok if there is any sorting included
  since ALL data in DB should be pulled :|"
  [{entity :entity/symbol
    {:keys [_limit _offset]
     :or {_limit 100000
          _offset 0}} :args
    :as schema} {:keys [entities roots]}]
  (let [entity-idx (memoize (fn [entity] (.indexOf entities entity)))]
    (letfn [(limited-result [entity limit]
              (let [idx (entity-idx entity)]
                (loop [[id & ids] (map #(nth % idx) roots)
                       result []
                       elements #{}]
                  (cond
                    ;;
                    (nil? id) result
                    ;;
                    (and (pos? limit) (= (count result) limit)) result
                    ;;
                    (contains? elements id)
                    (recur ids result elements)
                    :else
                    (recur ids (conj result id) (conj elements id))))))
            (limited-relation-result
              [from-entity to-entity from-id limit]
              (let [fidx (entity-idx from-entity)
                    tidx (entity-idx to-entity)]
                (loop [[id & ids] (map #(vector (nth % fidx) (nth % tidx)) roots)
                       result []
                       elements #{}]
                  (cond
                    ;;
                    (nil? id) (map second result)
                    ;;
                    (zero? (first id)) (recur ids result elements)
                    ;;
                    (= (count result) limit) (map second result)
                    ;;
                    (contains? elements id)
                    (recur ids result elements)
                    ;;
                    (= from-id (first id))
                    (recur ids (conj result id) (conj elements id))
                    ;;
                    :else
                    (recur ids result elements)))))
            ;;
            (pull-relations
              [[db
                {from-id :db/id :as record}
                {:keys [relations] :as schema
                 {:keys [_limit _offset]} :args
                 from-entity :entity/symbol}]]
              (reduce-kv
                (fn [[db record schema] k {c :cardinality :as relation-schema
                                           to-entity :entity/symbol}]
                  (case c
                    :many (let [ids (limited-relation-result from-entity to-entity from-id _limit)
                                [db result] (pull-roots [db [] relation-schema] ids)]
                            [db (assoc record k result) schema])
                    :one (let [[id] (limited-relation-result from-entity to-entity from-id 1)
                               [db result] (pull-roots [db [] relation-schema] [id])]
                           [db (assoc record k result) schema])
                    [db record schema]))
                [db record schema]
                relations))
            (pull-roots
              ([[db result {:keys [selection/fields]
                            entity :entity/symbol :as schema}] ids]
               (let [already-pulled (get db entity)
                     to-pull (remove #(or (zero? %) (contains? already-pulled %)) ids)
                     records (d/pull-many (d/db conn) (into [:db/id] (keys fields)) to-pull)]
                 (reduce
                   (fn [[db result schema] record]
                     (let [[db full-record] (pull-relations [db record schema])]
                       [db (conj result full-record) schema]))
                   [db result schema]
                   records))))]
      (let [root-ids (limited-result entity _limit)
            [_ result] (pull-roots [nil [] schema] root-ids)]
        result))))


(defn prepare-roots
  [{:keys [entities roots]} schema]
  (let [to-pull (reduce merge
                        (map-indexed
                          (fn [idx entity]
                            {entity (distinct (map #(nth % idx) roots))})
                          entities))
        entity-idx (memoize (fn [entity] (.indexOf entities entity)))
        references (letfn [(get-links [entity relation-symbol cardinality]
                             (let [eidx (entity-idx entity)
                                   ridx (entity-idx relation-symbol)]
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
                                (fn [result _ {relation :entity/symbol :as relation-schema
                                               cardinality :cardinality}]
                                  (let [first-level (get-links entity relation cardinality)
                                        deeper (group-references relation-schema)]
                                    (as-> result result
                                      (assoc-in result [entity relation] first-level)
                                      (merge-with merge result deeper))))
                                result
                                relations)))]
                     (group-references schema))]
    {:pull to-pull
     :link references}))


(defrecord RString [value]
  Comparable
  (compareTo [this other]
    (.compareTo ^String (.-value other) (.-value this))))


(defn- gen-order-fn
  [order]
  (apply
    juxt
    (reduce-kv
      (fn [r k o]
        (conj 
          r
          (if (= o :asc) k
            (comp
              (fn [v]
                (cond
                  (string? v) (RString. v) 
                  (number? v) (- v)
                  (keyword? v) (RString. (name v))
                  (map? v) nil
                  (vector? v) v
                  (nil? v) nil
                  ;;
                  (instance? java.util.Date v)
                  (- (.getTime v))))
              k))))
      []
      order)))


(defn pull-roots
  ([roots {entity :entity/symbol :as schema
           {order :_order_by
            limit :_limit
            offset :_offset} :args}]
   (let [{to-pull :pull references :link} (prepare-roots roots schema)]
     (letfn [(pull [{:keys [selection/fields relations] entity :entity/symbol}]
               (let [entities (d/pull-many
                                (d/db conn)
                                (into [:db/id] (keys fields))
                                (get to-pull entity))]
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
                                             {order :_order_by
                                              limit :_limit
                                              offset :_offset} :args}]
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
                                    (let [default-result (map
                                                           #(link % relation-schema)
                                                           to-link-ids)
                                          sorted-result (if (nil? order) default-result
                                                          (let [pred (gen-order-fn order)
                                                                result (sort-by pred default-result)]
                                                            result))]
                                      (as-> sorted-result final
                                        (if-not offset final
                                          (drop offset final))
                                        (if-not limit final
                                          (take limit final))))))))
                       record
                       relations)))]
           (let [result (reduce
                          (fn [result id]
                            ((fnil conj []) result (link id schema)))
                          nil
                          (get to-pull entity))
                 sorted-result (if (nil? order) result
                                 (let [pred (gen-order-fn order)
                                       result (sort-by pred result)]
                                   result))]
             (as-> sorted-result final
               (if-not offset final
                 (drop offset final))
               (if-not limit final
                 (take limit final))))))))))


(defn pull-all-roots
  "Pull all roots ignoring limit and offset"
  ([roots {entity :entity/symbol :as schema}]
   (let [{to-pull :pull
          references :link} (prepare-roots roots schema)]
     (letfn [(pull [{:keys [selection/fields relations] entity :entity/symbol}]
               (let [entities (d/pull-many
                                (d/db conn)
                                (into [:db/id] (keys fields))
                                (get to-pull entity))]
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
                       (fn [record relation {relation-symbol :entity/symbol :as relation-schema}]
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
                                      to-link-ids)))))
                       record
                       relations)))]
           (reduce
             (fn [result id]
               ((fnil conj []) result (link id schema)))
             nil
             (get to-pull entity))))))))



(defn search-entity
  [entity-id args selection]
  (let [schema (selection->schema entity-id args selection)
        roots (search-entity-roots schema)]
    (<-keys (pull-roots roots schema))))


(defn get-entity
  [entity-id args selection]
  (let [args' (reduce-kv
                (fn [r k v]
                  (assoc r k {:_eq v}))
                nil
                args)
        schema (selection->schema entity-id args' selection)
        roots (search-entity-roots schema)]
    (first (<-keys (pull-roots roots schema)))))


; (defn get-entity-tree
;   [entity on root selection]
;   (let [schema (selection->schema entity {:euuid {:_eq root}} selection)
;         {roots :roots} (search-entity-roots schema)]
;     (when-let [focused-root (ffirst roots)]
;       (let [rules [[(list 'descendant '?parent '?child)
;                     ['?child on '?parent]]
;                    [(list 'descendant '?parent '?grandchild)
;                     ['?middle on '?parent]
;                     (list 'descendant '?middle '?grandchild)]]
;             roots (d/q
;                     '[:find ?child 
;                       :in $ % ?p
;                       :where
;                       (descendant ?p ?child)]
;                     (d/db conn)
;                     rules
;                     focused-root)]
;         (<-keys (pull-roots {:entities []
;                              :roots roots} schema))))))



(defn get-entity-tree
  [entity on root selection]
  (let [schema (selection->schema entity {:euuid {:_eq root}} selection)
        rules [[(list 'descendant '?parent '?child)
                ['?child on '?parent]]
               [(list 'descendant '?parent '?grandchild)
                ['?middle on '?parent]
                (list 'descendant '?middle '?grandchild)]]
        [entities statements] (schema->statements schema true)
        on-symbol (get-in schema [:relations on :entity/symbol])
        entities (conj entities on-symbol)
        statements (concat statements [(list 'descendant (first entities) on-symbol)])
        query `[:find ~@entities
                 :in ~'$ ~'%
                 :where
                 ~@statements]
        roots (d/q query (d/db conn) rules)
        tree-nodes (concat (distinct (map first roots))
                           (distinct (map last roots)))
        schema' (selection->schema entity selection)
        [entities statements] (schema->statements schema')
        query' `[:find ~@entities
                 :in ~'$ [~(first entities) ...]
                 :where
                 ~@statements]
        roots' (d/q query' (d/db conn) tree-nodes)]
    (pull-roots {:roots roots' :entities entities} schema')))


(comment
  (def entity iu/permission)
  (def selection
    {:euuid nil :name nil
     :roles [{:selections {:name nil}}]
     :parent [{:selections {:euuid nil}}]})
  (def root #uuid "08b9283e-d06c-44a0-84a5-4f9187f27389")
  (def on (field->key entity :parent))
  (def schema (selection->schema entity {:euuid {:_eq root}} selection))
  (schema->statements schema true)
  (get-entity-tree entity on root selection)
  (time (get-entity entity {:euuid root} selection))
  (d/pull (d/db conn) '[*] 225)
  
  (def rules
    [[(list 'descendant '?parent '?child)
      ['?child on '?parent]]
     [(list 'descendant '?parent '?grandchild)
      ['?middle on '?parent]
      (list 'descendant '?middle '?grandchild)]]) 
  
  (time
    (d/q
      '[:find
        ?entity_zQM2F
        ?entity_fO-jV
        ?entity_yOxBL
        :in $ [?entity_zQM2F ...]
        :where
        [?entity_zQM2F :entity #uuid "6f525f5f-0504-498b-8b92-c353a0f9d141"]
        [?entity_zQM2F :euuid ?field_qrjML]
        [(= ?field_qrjML #uuid "08b9283e-d06c-44a0-84a5-4f9187f27389")]
        (or-join
          [?entity_zQM2F ?entity_fO-jV]
          [?entity_zQM2F :247ac70e-460b-40a1-9886-dfc992cfe921 ?entity_fO-jV]
          (and
            [(missing? $ ?entity_zQM2F :247ac70e-460b-40a1-9886-dfc992cfe921)]
            [(ground 0) ?entity_fO-jV]))
        (or-join
          [?entity_zQM2F ?entity_yOxBL]
          [?entity_zQM2F :16ca53f4-0fe3-4122-93dd-1e86fd1b58db ?entity_yOxBL]
          (and
            [(missing? $ ?entity_zQM2F :16ca53f4-0fe3-4122-93dd-1e86fd1b58db)]
            [(ground 0) ?entity_yOxBL]))]
      (d/db conn)
      rules))
  (time
    (d/q
      '[:find ?entity_bCSmN ?entity_uVPEP ?child
        :in $ %
        :where
        [?entity_bCSmN :entity #uuid "6f525f5f-0504-498b-8b92-c353a0f9d141"]
        [?entity_bCSmN :euuid ?field_EFdpb]
        [(= ?field_EFdpb #uuid "08b9283e-d06c-44a0-84a5-4f9187f27389")]
        (or-join
          [?entity_bCSmN ?entity_uVPEP]
          [?entity_bCSmN :16ca53f4-0fe3-4122-93dd-1e86fd1b58db ?entity_uVPEP]
          (and
            [(missing? $ ?entity_bCSmN :16ca53f4-0fe3-4122-93dd-1e86fd1b58db)]
            [(ground 0) ?entity_uVPEP]))
        (descendant ?entity_bCSmN ?child)]
      (d/db conn)
      rules)))


(defn search-entity-tree
  [entity on args selection]
  (let [rules [[(list 'descendant '?parent '?child)
                ['?child on '?parent]]
               [(list 'descendant '?parent '?grandchild)
                ['?middle on '?parent]
                (list 'descendant '?middle '?grandchild)]]
        roots (d/q
                '[:find ?child 
                  :in $ % [?p ...]
                  :where
                  (descendant ?p ?child)]
                (d/db conn)
                rules)]
    ))


(comment
  (time
    (get-entity
      iu/user
      {:name "rgersak"}
      {:euuid nil
       :name nil
       :settings nil
       :modified_on nil
       :modified_by [{:selections
                      {:name nil}}]}))
  (sort
    ["SUPERUSER"
     "Manager"
     "Approval manager"
     "Key account manager"
     "Administrator"])
  (time
    (do
      (def args {:_order_by {:name :asc}})
      (def selection
        {:euuid nil
         :name nil
         :modified_by [{:selections {:name nil}}]
         :users [{:selections {:name nil
                               :active nil}
                  :args {:_order_by {:name :desc}}}]})
      (def schema
        (time
          (selection->schema
            neyho.eywa.iam.uuids/user-role
            args selection)))
      (def roots (time (search-entity-roots schema)))
      (time (<-keys (pull-roots roots schema)))))
  (<-keys (pull-all-roots roots schema))
  (def order {:name :desc})
  (time
    (let [schema (selection->schema
                   neyho.eywa.iam.uuids/user
                   {:_order_by {:name :asc}}
                   {:euuid nil
                    :name nil
                    :active nil
                    :roles [{:selections {:name nil}}]
                    :modified_by [{:selections {:name nil}
                                   :args {:_where
                                          {:euuid
                                           {:_in [#uuid "c0e960bc-2515-11ed-8064-02a535895d2d"
                                                  #uuid "fc8716c6-daf1-11ee-a940-02a535895d2d"
                                                  #uuid "7ae30755-f592-4f12-99b0-0ee9fdc88471"]}}}}]})
          roots (search-entity-roots schema)]
      (def schema schema)
      (<-keys (pull-roots roots schema))))
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


;; Mutations
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
                                (select-keys (shallow-snake v) (flatten constraints)))]
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
                      fields-data (merge fields-data resolved-references)]
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
                            (assoc-in [:recursion entity-euuid k id] pid)
                            (update-in [:index entity-euuid] merge (zipmap parent-indexes (repeat pid)))
                            (update-in [:entity entity-euuid pid] merge (select-keys data fields-data)))))
                      result
                      (select-keys data recursions))
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
                                          [:relations/many euuid {:entity entity-euuid :attribute k}]
                                          (fnil conj #{})
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
  ([{:keys [entity relations/many relations/one reference recursion]}]
   (let [transformation-keys (memoize
                               (fn [entity]
                                 (reduce-kv
                                   (fn [r k {t :key}]
                                     (assoc r k t))
                                   nil
                                   (get *schema* entity))))]
     (as-> {:changes []
            :retractions []
            :references []
            :recursions []
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
                             (let [db-attribute (get (transformation-keys entity) field)]
                               (conj references {:db/id record-id db-attribute ref-id})
                               #_(if syncing?
                                 (conj references
                                       [:db/retract record-id db-attribute]
                                       {:db/id record-id db-attribute ref-id})
                                 (conj references {:db/id record-id db-attribute ref-id}))))))
                 transactions
                 mapping))
             transactions
             ref-by-constraints))
         transactions
         reference)
       (reduce-kv
         (fn [transactions entity ref-by-parent]
           (reduce-kv
             (fn [transactions field mapping]
               (reduce-kv
                 (fn [transactions entity-id parent-id]
                   (update transactions :recursions
                           (fn [references]
                             (let [db-attribute (get (transformation-keys entity) field)]
                               (conj references {:db/id entity-id db-attribute parent-id})))))
                 transactions
                 mapping))
             transactions
             ref-by-parent))
         transactions
         recursion)
       ;; Relations
       (reduce-kv
         (fn [transactions _ refs-by-attribute]
           (reduce-kv
             (fn [transactions {:keys [entity attribute]} records]
               (let [grouped (group-by first records)]
                 (reduce-kv
                   (fn [transactions from-id records]
                     (let [to-ids (map second records)]
                       (update transactions :relations/many
                               (fn [references]
                                 (concat references
                                         [{:db/id from-id (get (transformation-keys entity) attribute) to-ids}])))))
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
                                                 (get (transformation-keys entity) attribute) to-id})]
                               references))))
                 transactions
                 records))
             transactions
             refs-by-attribute))
         transactions
         one)))))


(defn ensure-euuids
  [{:keys [tempids]}]
  (let [inserted (vals tempids)]
    (when-some [missing-euuids (not-empty
                                 (map
                                   (fn [[id]]
                                     {:db/id id :euuid (java.util.UUID/randomUUID)})
                                   (d/q '[:find ?e
                                          :in $ [?e ...]
                                          :where
                                          (not [?e :euuid _])]
                                        (d/db conn)
                                        inserted)))]
      (d/transact! conn missing-euuids))))



(defn delete-entity
  [entity args]
  (when-some [{:keys [db/id]} (get-entity entity args nil)]
    (d/transact! conn [[:db/retractEntity id]])))


(comment
  (def entity iu/user)
  (def args {:_where {:name {:_eq "Terminator"}}})
  (def schema (selection->schema entity args selection))
  )


(defn slice-entity
  [entity args selection]
  (let [{:keys [relations] :as schema
         entity :entity/symbol} (selection->schema entity args selection)
        {:keys [roots entities]} (search-entity-roots schema)
        retractions (reduce-kv
                      (fn [r relation-key {relation-entity :entity/symbol}]
                        (let [idx (.indexOf entities relation-entity)
                              reversed? (reverse? relation-key)
                              k (if reversed? (invert relation-key) relation-key)]
                          (if-some [transactions (not-empty
                                                   (keep
                                                     (fn [[from-id :as row]]
                                                       (when-some [to-id (get row idx)]
                                                         (when-not (zero? to-id)
                                                           (if reversed?
                                                             [:db/retract to-id k from-id]
                                                             [:db/retract from-id k to-id]))))
                                                     roots))]
                            (concat r transactions)
                            r)))
                      []
                      relations)]
    (when (not-empty retractions)
      (d/transact! conn retractions))
    (reduce-kv
      (fn [r k _]
        (assoc r k true))
      nil
      relations)))

;; This works by first finding all roots
;; than pulling all records and composing
;; slice transactions... This is more advanced than currently
;; supported in postgres, so maybe someday
; (defn slice-entity
;   [entity args selection]
;   ;; TODO - result should be map with relation fields
;   ;; marked as bla... look at documentation
;   ;; And selection is different as well
;   (let [schema (selection->schema entity args selection)
;         roots (search-entity-roots schema)
;         result (pull-roots roots schema)]
;     (letfn [(slice-transactions
;               [result {:keys [relations]} {from-id :db/id :as data}]
;               (concat
;                 result
;                 (reduce-kv
;                   (fn [r k schema]
;                     (let [relation-data (get data k)
;                           relation-ids (map :db/id relation-data)
;                           reversed? (reverse? k)
;                           k (if reversed? (invert k) k)]
;                       (concat
;                         (if reversed?
;                           (map #(vector :db/retract % k from-id) relation-ids)
;                           (map #(vector :db/retract from-id k %) relation-ids))
;                         (slice-transactions r schema relation-data))))
;                   []
;                   relations)))]
;       (when-some [transactions (not-empty
;                                  (mapcat
;                                    (fn [data]
;                                      (slice-transactions [] schema data))
;                                    result))]
;         (d/transact! conn transactions))
;       result)))


(defn sync-entity
  [entity data]
  (let [{many-relations :relations/many
         one-relations :relations/one
         :keys [changes retractions references recursions]}
        (delta->transactions (delta entity data))
        ;;
        {:keys [tempids] :as result}
        (d/transact! conn (concat changes references recursions many-relations one-relations))
        ;;
        remove-attributes (map
                            (fn [[o id attribute]]
                              [o (get tempids id) attribute])
                            retractions)]
    (letfn [(sync-slice-transactions
              [transaction]
              (let [[k connections] (first (dissoc transaction :db/id))
                    final-connections (set (map tempids connections))
                    db-id (get tempids (:db/id transaction))
                    reversed? (reverse? k)
                    all-connections (if reversed?
                                      (d/q
                                        '[:find ?entity ?key ?target
                                          :in $ ?key ?target
                                          :where
                                          [?entity ?key ?target]]
                                        (d/db conn)
                                        (invert k)
                                        db-id)
                                      (d/q
                                        '[:find ?target ?key ?entity
                                          :in $ ?key ?target
                                          :where
                                          [?target ?key ?entity]]
                                        (d/db conn)
                                        k
                                        db-id))]
                (reduce
                  (fn [r [from-id link to-id]]
                    (if (contains? final-connections (if reversed? from-id to-id)) r
                      (conj r (vector :db/retract from-id link to-id))))
                  []
                  all-connections)))]
      ; (def retractions retractions)
      ; (def many-relations many-relations)
      ; (def tempids tempids)
      ; (def slice-transactions (mapcat sync-slice-transactions many-relations))
      (d/transact! conn remove-attributes)
      (when-some [slice-transactions (not-empty (mapcat sync-slice-transactions many-relations))]
        (d/transact! conn slice-transactions))
      (ensure-euuids result)))) 



(comment
  (sync-entity
    entity
    {:name "Terminator"
     :roles [{:name "SUPERUSER"}
             {:name "User"}]})
  (time (sync-entity iu/user users))
  (time (slice-entity iu/user-role args selection))
  (do
    (def users
      (dataset/search-entity
        neyho.eywa.iam.uuids/user
        nil
        {;:euuid nil
         :name nil
         :settings nil
         :password nil
         :avatar nil
         :active nil
         :modified_on nil
         :modified_by [{:selections {:euuid nil :name nil}}]
         :roles [{:selections
                  {:euuid nil :name nil :avatar nil}}]}))
    (def data users)
    (def entity iu/user))


  (def roles
    (dataset/search-entity
      neyho.eywa.iam.uuids/user-role
      nil
      {:euuid nil
       :name nil
       :users [{:selections
                {:euuid nil :name nil}}]}))

  (do
    (def data permissions)
    (def entity neyho.eywa.iam.uuids/permission)
    (def permissions
      (dataset/search-entity
        entity
        nil
        {:euuid nil
         :name nil
         :roles [{:selections {:euuid nil}}]})))
  (sync-entity entity permissions)
  (time
    (search-entity
      iu/permission
      {:name {:_eq "Add"}}
      {:euuid nil
       :name nil
       :parent [{:selections {:euuid nil :name nil}}]}))
  

  (d/q
    '[:find ?p
      :in $ % [?p ...]
      :where
      [?p :247ac70e-460b-40a1-9886-dfc992cfe921 ?c]
      (descendant ?p ?c)]
    (d/db conn)
    crules
    [225])

  (search-entity
    iu/permission
    nil
    {:name nil})

  (def crules
    '[[(descendant ?parent ?child)
       [?child :247ac70e-460b-40a1-9886-dfc992cfe921 ?parent]]
      [(descendant ?parent ?grandchild)
       [?middle :247ac70e-460b-40a1-9886-dfc992cfe921 ?parent]
       (descendant ?middle ?grandchild)]])

  (time
    (d/q
      '[:find (pull ?child [*])
        :in $ % [?p ...]
        :where
        (descendant ?p ?child)]
      (d/db conn)
      crules
      192))


  (def rules
    '[[(ancestor ?child ?parent)
       [?child :247ac70e-460b-40a1-9886-dfc992cfe921 ?parent]]
      [(ancestor ?child ?grandparent)
       [?child :247ac70e-460b-40a1-9886-dfc992cfe921 ?parent]
       (ancestor ?parent ?grandparent)]])

  (d/q
    '[:find ?e ?ancestor 
      :in $ % [?e ...]
      :where
      (ancestor ?e ?ancestor)]
    (d/db conn)
    rules
    [135 125]))


(defn stack-entity
    [entity data]
    (let [{many-relations :relations/many
           one-relations :relations/one
           :keys [changes retractions references]}
          (delta->transactions (delta entity data))
          ;;
          {:keys [tempids] :as result}
          (d/transact! conn (concat changes references many-relations one-relations))
          remove-attributes (map
                              (fn [[o id attribute]]
                                [o (get tempids id) attribute])
                              retractions)]
      (d/transact! conn remove-attributes)
      (ensure-euuids result)))
