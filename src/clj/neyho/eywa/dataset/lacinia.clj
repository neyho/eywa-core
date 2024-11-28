(ns neyho.eywa.dataset.lacinia
  (:require
    [camel-snake-kebab.core :as csk]
    [neyho.eywa.dataset :as dataset]
    [neyho.eywa.dataset.core :as core]
    [neyho.eywa.iam.uuids :as iu]
    [neyho.eywa.db
     :refer [*db*
             sync-entity
             stack-entity
             slice-entity
             get-entity
             get-entity-tree
             purge-entity
             search-entity-tree
             search-entity
             aggregate-entity
             aggregate-entity-tree
             delete-entity]]
    [neyho.eywa.lacinia]
    [com.walmartlabs.lacinia.parser :as parser]
    [com.walmartlabs.lacinia.executor :as executor]
    [com.walmartlabs.lacinia.resolve :as lacinia.resolve]
    [clojure.tools.logging :as log]
    [clojure.data.csv :as csv]
    [clojure.java.io :as io]
    [clojure.string]))

(def npattern #"[\s\_\-\.\$\[\]\{\}\#]+")

(def 
  ^{:doc "Name normalization function"} 
  normalize-name 
  (memoize
    (fn
      [n]
      (clojure.string/lower-case 
        (clojure.string/replace n npattern "_")))))


(defn scalar-attribute? [{t :type}]
  (contains? 
    #{"int" "float" "boolean" "string" "avatar" "hashed"
      "encrypted" "timestamp" "timeperiod" "currency"
      "transit" "json" "uuid" "enum"}
    t))

(defn reference-attribute? [{t :type}]
  (contains? #{"user" "group" "role"} t))

(defn attribute->gql-field [n]
  (keyword (normalize-name n)))

(defn entity->gql-object [n]
  (csk/->PascalCaseKeyword n))

(defn entity->gql-input-object [n]
  (csk/->PascalCaseKeyword (str n " input")))

(defn entity-attribute->enum [{ename :name} {aname :name}]
  (csk/->PascalCaseKeyword (str ename \space aname)))

(defn entity-attribute->enum-operator [{ename :name} {aname :name}]
  (csk/->PascalCaseKeyword (str ename \space aname \space "operator")))

(def ignored-field-type? #{"transit" "json" "encrypted" "hashed"})

(defn entity->search-operator [{n :name}]
  (csk/->camelCaseKeyword (str "search " n " operator")))

(defn entity->order-by-operator [{n :name}]
  (csk/->camelCaseKeyword (str "order by " n " operator")))

(defn entity->attributes-enum [{n :name}]
  (csk/->camelCaseKeyword (str n "Attributes")))

(defn entity->distinct-on-operator [{n :name}]
  (csk/->camelCaseKeyword (str "distinct on " n " operator")))

; (defn entity->aggregate-object [{n :name}]
;   (csk/->PascalCaseKeyword (str n " aggregate")))


(defn entity->count-object [{n :name}]
  (csk/->PascalCaseKeyword (str n " count")))

(defn entity->max-object [{n :name}]
  (csk/->PascalCaseKeyword (str n " max")))

(defn entity->min-object [{n :name}]
  (csk/->PascalCaseKeyword (str n " min")))

(defn entity->avg-object [{n :name}]
  (csk/->PascalCaseKeyword (str n " avg")))

(defn entity->sum-object [{n :name}]
  (csk/->PascalCaseKeyword (str n " sum")))

(defn entity->agg-object [{n :name}]
  (csk/->PascalCaseKeyword (str n " aggregate")))

(defn entity->agg-part-object [{n :name}]
  (csk/->PascalCaseKeyword (str n " agg")))


(defn entity->slice-object [{n :name}]
  (csk/->PascalCaseKeyword (str n " slice")))

(defn attribute-type->scalar [entity {t :type :as attribute}]
  (case t
    "int" 'Int
    "float" 'Float
    "boolean" 'Boolean
    "avatar" 'String
    "string" 'String
    "encrypted" 'Encrypted
    "hashed" 'Hash
    "timestamp" 'Timestamp
    "timeperiod" 'TimePeriod
    "currency" 'Currency
    "json" 'JSON
    "transit" 'Transit
    "uuid" 'UUID
    "enum" (entity-attribute->enum entity attribute)))

(defn attribute-type->operator [entity {t :type :as attribute}]
  (case t
    "int" {:type :IntegerQueryOperator}
    "float" {:type :FloatQueryOperator}
    "boolean" {:type :BooleanQueryOperator}
    "string" {:type :StringQueryOperator}
    "avatar" {:type :StringQueryOperator}
    "encrypted" {:type :StringQueryOperator}
    "timestamp" {:type :TimestampQueryOperator}
    "timeperiod" {:type :TimePeriodQueryOperator}
    "currency" {:type :CurrencyQueryOperator}
    "transit" {:type :StringQueryOperator}
    "uuid" {:type :UUIDQueryOperator}
    "enum" {:type (entity-attribute->enum-operator entity attribute)} 
    {:type t}))


(defn numerics? [{as :attributes}]
  (filter 
    (fn [{t :type}]
      (boolean (#{"int" "float"} t)))
    as))

(defn entity->numeric-object [{n :name}]
  (csk/->PascalCaseKeyword (str n " Numerics")))


(defn entity->relation-enum [{n :name}]
  (csk/->PascalCaseKeyword (str n " relations enum")))


(def currency-codes
  (let [table (with-open [reader (io/reader (io/resource "lacinia/currency.csv"))]
                (doall
                  (csv/read-csv reader)))]
    (distinct
      (keep
        (fn [[_ _ code]]
          (when (not-empty code) code))
        (rest table)))))


(defn normalized-enum-value [value]
  (clojure.string/replace value #"-|\s" "_"))


(defn generate-lacinia-enums
  [model]
  (letfn [(model->enums [] 
            (let [entities (core/get-entities model)]
              (reduce
                (fn [enums {:keys [attributes] :as entity}]
                  (as-> enums ens
                    ;; This may be required 
                    (assoc enums (entity->attributes-enum entity)
                           {:values (mapv 
                                      (comp normalize-name :name) 
                                      (filter scalar-attribute? attributes))})
                    (let [le (filter #(= (:type %) "enum") attributes)]
                      (if (not-empty le)
                        (reduce
                          (fn [enums {config :configuration :as attribute}]
                            (assoc enums (entity-attribute->enum entity attribute) 
                                   (update config :values #(mapv (comp normalized-enum-value :name) %))))
                          ens
                          le)
                        ens))))
                {:BooleanCondition {:values ["TRUE" "FALSE" "NOT_TRUE" "NOT_FALSE" "NULL"]}}
                entities)))]
    (merge
      (model->enums)
      {:SQLJoinType {:values [:LEFT :RIGTH :INNER]}
       :currency_enum {:values currency-codes}
       :order_by_enum {:values [:asc :desc]}
       :is_null_enum {:values [:is_null :is_not_null]}})))


(defn generate-lacinia-objects 
  [model]
  (letfn [(reference-object [euuid]
            (let [entity (core/get-entity model euuid)]
              {:type (entity->gql-object (:name entity))
               :args {:_where {:type (entity->search-operator entity)}
                      :_maybe {:type (entity->search-operator entity)}}}))
          (has-numerics? [{:keys [attributes]}]
            (some
              (fn [{t :type}]
                (#{"int" "float"} t))
              attributes))
          (get-numerics [{:keys [attributes]}]
            (reduce
              (fn [fields {atype :type :as attribute}]
                (case atype
                  ("int" "float") (conj fields attribute)
                  fields))
              [] 
              attributes))]
    (let [entities (core/get-entities model)
          _who :modified_by
          _when :modified_on]
      (reduce
        (fn [r {ename :name attributes :attributes :as entity}]
          (if (empty? attributes) r
            (let [numerics (get-numerics entity) 
                  scalars (filter scalar-attribute? attributes)
                  references (filter reference-attribute? attributes)
                  entity-relations (core/focus-entity-relations model entity)
                  to-relations (filter #(not-empty (:to-label %)) entity-relations)]
              (if (nil? ename) r
                (cond->
                  (assoc 
                    r (entity->gql-object ename)
                    {:fields (as-> 
                               (cond->
                                 {:euuid {:type 'UUID}
                                  :modified_by (reference-object iu/user)
                                  :modified_on {:type :Timestamp}})
                               fields 
                               ;; References to well known objects
                               (reduce
                                 (fn [fields {aname :name t :type}]
                                   (case t
                                     "user" (assoc fields (attribute->gql-field aname) (reference-object iu/user))
                                     "group" (assoc fields (attribute->gql-field aname) (reference-object iu/user-group))
                                     "role" (assoc fields (attribute->gql-field aname) (reference-object iu/user-role))))
                                 fields
                                 references)
                               ;; Scalars
                               (reduce
                                 (fn [fields {aname :name atype :type active :active
                                              :as attribute}]
                                   (if-not (or active (= aname "euuid")) fields 
                                     (let [t (attribute-type->scalar entity attribute)]
                                       (assoc fields (attribute->gql-field aname)
                                              (cond-> {:type t}
                                                (= "enum" atype)
                                                (assoc
                                                  :args (zipmap
                                                          [:_eq :_neq :_in :_not_in]
                                                          (concat
                                                            (repeat 2 {:type t})
                                                            (repeat 2 {:type (list 'list t)}))))
                                                (= t 'UUID)
                                                (assoc
                                                  :args (zipmap
                                                          [:_eq :_neq :_in :_not_in]
                                                          (concat
                                                            (repeat 2 {:type 'UUID})
                                                            (repeat 2 {:type (list 'list 'UUID)}))))
                                                (= t 'Boolean)
                                                (assoc
                                                  :args (zipmap
                                                          [:_eq :_neq]
                                                          (repeat {:type 'Boolean})))
                                                (= t 'Int) 
                                                (assoc 
                                                  :args (zipmap
                                                          [:_gt :_lt :_eq :_neq :_ge :_le :_in :_not_in]
                                                          (concat
                                                            (repeat 6 {:type 'Int})
                                                            (repeat 6 {:type (list 'list 'Int)}))))
                                                ;;
                                                (= t 'Float) 
                                                (assoc 
                                                  :args (zipmap
                                                          [:_gt :_lt :_eq :_neq :_ge :_le :_in :_not_in]
                                                          (concat
                                                            (repeat 6 {:type 'Float})
                                                            (repeat 6 {:type (list 'list 'Float)}))))
                                                (= t 'String)
                                                (assoc
                                                  :args (zipmap
                                                          [:_neq :_eq :_like :_ilike :_in :_not_in] 
                                                          (concat
                                                            (repeat 4 {:type 'String})
                                                            (repeat 4 {:type (list 'list 'String)}))))
                                                (= t 'Currency)
                                                (assoc
                                                  :args (assoc
                                                          (zipmap
                                                            [:_gt :_lt :_neq :_eq :_ge :_le]
                                                            (repeat {:type 'Float}))
                                                          :in_currencies {:type (list 'list :currency_enum)}))
                                                (= t 'Timestamp)
                                                (assoc
                                                  :args (zipmap
                                                          [:_gt :_lt :_neq :_eq :_ge :_le]
                                                          (repeat {:type 'Timestamp})))
                                                (= t 'Timeperiod)
                                                (assoc
                                                  :args (merge
                                                          (zipmap
                                                            [:_gt :_lt :_neq :_eq :_ge :_le ]
                                                            (repeat {:type 'TimePeriod}))
                                                          (zipmap
                                                            [:contains :exclude]
                                                            (repeat {:type 'Timestamp})))))))))
                                 fields
                                 (conj scalars {:name "euuid" :type "uuid"}))
                               ;; Outgoing relations
                               (reduce
                                 (fn [fields {:keys [to to-label cardinality]}]
                                   (if (and (not-empty to-label) (not-empty (:name to))) 
                                     (assoc fields (attribute->gql-field to-label) 
                                            (let [t (entity->gql-object (:name to))
                                                  to-search (entity->search-operator to)
                                                  tree-search (entity->search-operator entity)] 
                                              ;; TODO - rethink _maybe and _where
                                              ;; It is essentially opening for _and _or and
                                              ;; Can it be skipped?
                                              ;; I think that it was just convinience to use operator
                                              ;; instead of generating more argument... That is more code
                                              (case cardinality
                                                ("o2m" "m2m") {:type (list 'list t)
                                                               :args {:_offset {:type 'Int}
                                                                      :_limit {:type 'Int}
                                                                      :_where {:type to-search}
                                                                      :_join {:type :SQLJoinType}
                                                                      :_maybe {:type to-search}
                                                                      :_order_by {:type (entity->order-by-operator to)}}}
                                                ("m2o" "o2o") {:type t
                                                               :args {:_where {:type to-search}
                                                                      :_join {:type :SQLJoinType}
                                                                      :_maybe {:type to-search}}}
                                                "tree" {:type t 
                                                        :args {:_where {:type tree-search}
                                                               :_join {:type :SQLJoinType}
                                                               :_maybe {:type tree-search}
                                                               (attribute->gql-field to-label) {:type :is_null_enum}}}
                                                {:type t})))
                                     fields))
                                 fields
                                 to-relations)
                               ;; This was used when _agg wasn't defined
                               (if (empty? to-relations) fields
                                 (cond->
                                   (assoc fields :_count {:type (entity->count-object entity)})
                                   ;; FUTURE Self - This doesn't make sense since other
                                   ;; entities will have _agg option. I can't think of use case
                                   ;; where this is relevant at target entity level
                                   ; (not-empty numerics)
                                   ; (assoc
                                   ;   :_max {:type (entity->numeric-object entity)}
                                   ;   :_min {:type (entity->numeric-object entity)}
                                   ;   :_avg {:type (entity->numeric-object entity)}
                                   ;   :_sum {:type (entity->numeric-object entity)})
                                   ;;
                                   (some has-numerics? (map :to to-relations))
                                   (assoc :_agg {:type (entity->agg-object entity)}))))})
                  ;;
                  (some has-numerics? (map :to to-relations))
                  (as-> objects
                    (reduce
                      (fn [objects {:keys [to to-label]}]
                        (if-not (has-numerics? to) objects
                          (assoc objects
                                 (entity->agg-part-object to)
                                 {:fields
                                  {:_max {:type (entity->numeric-object to)}
                                   :_min {:type (entity->numeric-object to)}
                                   :_avg {:type (entity->numeric-object to)}
                                   :_sum {:type (entity->numeric-object to)}}})))
                      objects
                      to-relations)
                    (assoc objects
                           (entity->agg-object entity)
                           {:fields
                            (reduce
                              (fn [fields {:keys [to to-label]}]
                                (if-not (has-numerics? to) fields
                                  (assoc fields (attribute->gql-field to-label) {:type (entity->agg-part-object to)})))
                              nil
                              to-relations)}))
                  
                  ;;
                  (not-empty to-relations)
                  (assoc 
                    ;;
                    (entity->slice-object entity)
                    {:fields
                     (reduce
                       (fn [fields {:keys [to to-label]}]
                         (if (not-empty to-label) 
                           (assoc fields (attribute->gql-field to-label)
                                  {:type 'Boolean
                                   :args {:_where {:type (entity->search-operator to)}}})
                           fields))
                       nil
                       entity-relations)}
                    ;;
                    (entity->count-object entity)
                    {:fields (reduce
                               (fn [fields {:keys [to to-label]}]
                                 (if (not-empty to-label) 
                                   (assoc fields (attribute->gql-field to-label)
                                          {:type 'Int
                                           :args {:_where {:type (entity->search-operator to)}}})
                                   fields))
                               nil
                               entity-relations)})
                  ;;
                  (not-empty numerics)
                  (assoc (entity->numeric-object entity)
                         {:fields (reduce
                                    (fn [result attribute]
                                      (assoc result (attribute->gql-field (:name attribute))
                                             ; {:type (attribute-type->scalar entity attribute)
                                             {:type 'Float 
                                              :args {:_where {:type (entity->search-operator entity)}}}))
                                    nil
                                    numerics)}))))))
        {:Currency
         {:fields
          {:currency {:type :currency_enum 
                      :args (zipmap
                              [:_neq :_eq :_in :_nin] 
                              (repeat {:type 'String}))}
           :amount {:type 'Float
                    :args (zipmap
                            [:_gt :_lt :_eq :_neq :_ge :_le]
                            (repeat {:type 'Float}))}}}
         ;;
         :TimePeriod
         {:fields
          {:start {:type 'Timestamp}
           :end {:type 'Timestamp}}
          :args (merge
                  (zipmap
                    [:_gt :_lt :_neq :_eq :_ge :_le ]
                    (repeat {:type 'TimePeriod}))
                  (zipmap
                    [:_contains :_exclude]
                    (repeat {:type 'Timestamp})))}}
        entities))))


(defn generate-lacinia-input-objects 
  [model]
  (let [entities (core/get-entities model)
        user (core/get-entity model iu/user)
        group (core/get-entity model iu/user-group)
        role (core/get-entity model iu/user-role)
        user-input (csk/->PascalCaseKeyword (str (:name user) " input"))
        group-input (csk/->PascalCaseKeyword (str (:name group) " input"))
        role-input (csk/->PascalCaseKeyword (str (:name role) " input"))] 
    (letfn [(referenced-entity [{atype :type}]
              (case atype
                "user" user
                "group" group
                "role" role))]
      (reduce
        (fn [r {ename :name attributes :attributes :as entity}]
          (let [i (csk/->PascalCaseKeyword (str ename " input"))
                s (entity->search-operator entity)
                o (entity->order-by-operator entity)
                d (entity->distinct-on-operator entity)
                relations (core/focus-entity-relations model entity)
                recursions (filter 
                             #(= "tree" (:cardinality %)) 
                             relations)
                attributes' (conj attributes
                                  {:name "euuid" :type "uuid" :active true}
                                  {:name "modified_on"
                                   :type "timestamp"
                                   :active true})]
            (assoc 
              (reduce
                (fn [r attribute]
                  (let [t (entity-attribute->enum entity attribute)] 
                    (assoc r (entity-attribute->enum-operator entity attribute)
                           {:fields (zipmap
                                      [:_eq :_neq :_in :_not_in]
                                      (concat
                                        (repeat 2 {:type t})
                                        (repeat 2 {:type (list 'list t)})))})))
                r
                (filter #(= (:type %) "enum") attributes))
              ;; input fields
              i {:fields (as-> {} input
                           ;;
                           (reduce
                             (fn [fields {;constraint :constraint
                                          aname :name atype :type active :active :as attribute}]
                               (if-not (or active (= aname "euuid")) 
                                 fields
                                 (assoc fields 
                                        (keyword (normalize-name aname))
                                        {:type (let [t (case atype 
                                                         "currency"
                                                         'CurrencyInput
                                                         ;;
                                                         "timeperiod"
                                                         'TimePeriodInput
                                                         ;;
                                                         "user"
                                                         user-input
                                                         ;;
                                                         "group"
                                                         group-input
                                                         ;;
                                                         "role"
                                                         role-input
                                                         ;;
                                                         (attribute-type->scalar entity attribute))]
                                                 t
                                                 ;; TODO - To be fully compliant enable this
                                                 ;; For now this is disabled to enable easier
                                                 ;; data storing for eywa.dataset namespace
                                                 ;; that splits data into stack and slice mutations
                                                 #_(if (= constraint "mandatory")
                                                   (list 'non-null t)
                                                   t))})))
                             input
                             (conj attributes {:name "euuid" :type "uuid"}))
                           ;;
                           (reduce
                             (fn [fields {:keys [to to-label cardinality]}]
                               (if (not-empty to-label) 
                                 (assoc fields (keyword (normalize-name to-label))
                                        {:type (case cardinality
                                                 ("o2m" "m2m") (list 'list (csk/->PascalCaseKeyword (str (:name to) " input")))
                                                 (csk/->PascalCaseKeyword (str (:name to) " input")))})
                                 fields))
                             input
                             relations))}
              ;; search/filter fields operator
              s {:fields (as-> 
                           {:_and {:type (list 'list s)}
                            :_or {:type (list 'list s)}
                            :_not {:type (list 'list s)}}
                           operator
                           ;;
                           (reduce
                             (fn [args {:keys [from-label to-label]}]
                               (cond-> args
                                 (not-empty to-label)
                                 (assoc (keyword (normalize-name to-label)) {:type :is_null_enum})
                                 ;;
                                 (not-empty from-label)
                                 (assoc (keyword (normalize-name from-label)) {:type :is_null_enum})))
                             operator
                             recursions)
                           ;;
                           (reduce
                             (fn [fields {aname :name atype :type active :active :as attribute}]
                               (if (or
                                     (not active)
                                     (ignored-field-type? atype)
                                     (reference-attribute? attribute)) 
                                 fields
                                 ;;
                                 (assoc 
                                   fields 
                                   (keyword (normalize-name aname))
                                   (attribute-type->operator entity attribute))))
                             operator
                             (conj
                               attributes'
                               {:name "modified_by"
                                :type (entity->search-operator (core/get-entity model iu/user)) 
                                :active true}
                               {:name "modified_on"
                                :type :TimestampQueryOperator 
                                :active true})))}
              ;; _order_by operator
              o {:fields (reduce
                           (fn [fields {:keys [to to-label cardinality]}]
                             (case cardinality
                               ("m2o" "o2o") 
                               (if (not-empty to-label)
                                 (assoc 
                                   fields
                                   (keyword (normalize-name to-label))
                                   {:type (entity->order-by-operator to)})
                                 fields)
                               fields))
                           (reduce
                             (fn [fields {aname :name atype :type active :active :as attribute}]
                               (if-not (or active (= aname "euuid")) 
                                 fields
                                 (if (#{"json" "encrypted" "hashed"} atype) fields
                                   (if (reference-attribute? attribute)
                                     (assoc 
                                       fields
                                       (keyword (normalize-name aname))
                                       {:type (entity->order-by-operator
                                                (referenced-entity attribute))})
                                     (assoc 
                                       fields 
                                       (keyword (normalize-name aname))
                                       {:type :order_by_enum})))))
                             nil
                             attributes')
                           (conj
                             relations
                             {:to (core/get-entity model iu/user)
                              :to-label "modified_by" 
                              :cardinality "o2o"}))}
              ;;
              d {:fields (reduce
                           (fn [fields {:keys [to to-label cardinality]}]
                             (case cardinality
                               ;;
                               ("m2o" "o2o") 
                               (if (not-empty to-label)
                                 (assoc 
                                   fields
                                   (keyword (normalize-name to-label))
                                   {:type (entity->distinct-on-operator to)})
                                 fields)
                               ;; Default
                               fields))
                           ;;
                           (reduce
                             (fn [fields {aname :name :as attribute}]
                               (assoc fields
                                 (keyword (normalize-name aname))
                                 {:type (entity->distinct-on-operator
                                          (referenced-entity attribute))}))
                             {:attributes
                              {:type (list 'list (entity->attributes-enum entity))}}
                             (filter reference-attribute? attributes))
                           ;; 
                           (conj relations
                                 {:to (core/get-entity model iu/user)
                                  :to-label "modified_on" 
                                  :cardinality "o2o"}))})))
        {:UUIDQueryOperator
         {:fields {:_eq {:type 'UUID} :_neq {:type 'UUID}
                   :_in {:type (list 'list 'UUID)}
                   :_not_in {:type (list 'list 'UUID)}}}
         :BooleanQueryOperator
         ; {:fields {:_eq {:type 'Boolean} :_neq {:type 'Boolean}}}
         {:fields {:_boolean {:type 'BooleanCondition}}}
         :CurrencyInput
         {:fields
          {:amount {:type 'Float}
           :currency {:type :currency_enum}}}
         :TimePeriodInput
         {:fields
          {:start {:type 'Timestamp}
           :end {:type 'Timestamp}}}
         :StringQueryOperator
         {:fields
          (zipmap
            [:_neq :_eq :_like :_ilike :_in :_not_in] 
            (concat
              (repeat 4 {:type 'String})
              (repeat 4 {:type (list 'list 'String)})))}
         :IntegerQueryOperator
         {:fields
          (zipmap
            [:_gt :_lt :_eq :_neq :_ge :_le :_in :_not_in]
            (concat
              (repeat 6 {:type 'Int})
              (repeat 6 {:type (list 'list 'Int)})))}
         :FloatQueryOperator
         {:fields
          (zipmap
            [:_gt :_lt :_eq :_neq :_ge :_le :_in :_not_in]
            (concat
              (repeat 6 {:type 'Float})
              (repeat 6 {:type (list 'list 'Float)})))}
         :CurrencyQueryOperator
         {:fields
          (assoc
            (zipmap
              [:_gt :_lt :_neq :_eq :_ge :_le]
              (repeat {:type 'Float}))
            :in_currencies {:type (list 'list 'String)})}
         :TimestampQueryOperator
         {:fields 
          (zipmap
            [:_gt :_lt :_neq :_eq :_ge :_le]
            (repeat {:type 'Timestamp}))}
         :TimePeriodQueryOperator
         {:fields
          (merge
            (zipmap
              [:_gt :_lt :_neq :_eq :_ge :_le ]
              (repeat {:type 'TimePeriodInput}))
            (zipmap
              [:_contains :_exclude]
              (repeat {:type 'Timestamp})))}}
        entities))))



;;

(defn log-query
  [context]
  (log/debugf
    "Processing query for user %s[%d]:\n%s"
    (:username context)
    (:user context)
    ; (with-out-str (clojure.pprint/pprint context))
    (parser/summarize-query (:com.walmartlabs.lacinia.constants/parsed-query context))))


(defn generate-lacinia-queries [model]
  (let [entities (core/get-entities model)
        user (core/get-entity model iu/user)
        group (core/get-entity model iu/user-group)
        role (core/get-entity model iu/user-role)
        user-operator (entity->search-operator user)
        group-operator (entity->search-operator group)
        role-operator (entity->search-operator role)]
    (letfn [(reference-operator [{atype :type}]
              (case atype
                "user" user-operator
                "group" group-operator
                "role" role-operator))
            (attribute->type [entity attribute]
              (if (reference-attribute? attribute)
                {:type (reference-operator attribute)}
                (attribute-type->operator entity attribute)))]
      (reduce
        (fn [queries {euuid :euuid ename :name 
                      as :attributes 
                      {{uniques :unique} :constraints} :configuration
                      :as entity}]
          (let [relations (core/focus-entity-relations model entity)
                recursions (filter #(= "tree" (:cardinality %)) relations)
                allowed-uniques? (set (map :euuid as))
                uniques (keep
                          (fn [constraints]
                            (when-some [real-ones (filter allowed-uniques? constraints)]
                              (vec real-ones)))
                          uniques)
                get-args (reduce
                           (fn [args ids]
                             (reduce
                               (fn [args id]
                                 (let [{aname :name :as attribute} (core/get-attribute entity id)]
                                   (assoc args (keyword (normalize-name aname)) 
                                          {:type (cond 
                                                   (scalar-attribute? attribute) 
                                                   (attribute-type->scalar entity attribute)
                                                   ;;
                                                   (reference-attribute? attribute)
                                                   (reference-operator attribute))})))
                               args
                               ids))
                           {:euuid {:type 'UUID}}
                           uniques)
                search-arguments (cond->
                                   (conj as
                                         {:name "modified_on" 
                                          :type "timestamp"
                                          :active true}
                                         {:name "modified_by" 
                                          :type "user"
                                          :active true})
                                   (not-empty recursions)
                                   (as-> args
                                     (reduce
                                       (fn [args {:keys [from-label to-label]}]
                                         (cond-> args
                                           (not-empty to-label)
                                           (conj
                                             {:name to-label 
                                              :type :is_null_enum
                                              :active true})
                                           (not-empty from-label)
                                           (conj
                                             {:name from-label 
                                              :type :is_null_enum
                                              :active true})))
                                       args
                                       recursions)))]
            (cond->
              (assoc queries 
                     ;; GETTER
                     (csk/->camelCaseKeyword (str "get " ename))
                     {:type (entity->gql-object ename)
                      :args get-args 
                      :resolve 
                      (fn getter [context data _]
                        (try
                          (get-entity *db* (:euuid entity) data (executor/selections-tree context))
                          (catch Throwable e
                            (log/errorf e  "Couldn't resolve SYNC")
                            (throw e))))}
                     ;; SEARCH
                     (csk/->camelCaseKeyword (str "search " ename))
                     (let [args (reduce
                                  (fn [r {atype :type aname :name :as attribute}]
                                    (if (ignored-field-type? atype) r 
                                      (assoc 
                                        r
                                        (keyword (normalize-name aname))
                                        (attribute->type entity attribute))))
                                  {:_where {:type (entity->search-operator entity)}
                                   :_limit {:type 'Int}
                                   :_offset {:type 'Int}
                                   :_distinct {:type (entity->distinct-on-operator entity)}
                                   :_order_by {:type (entity->order-by-operator entity)}}
                                  search-arguments)]
                       (cond->
                         {:type (list 'list (entity->gql-object ename))
                          :resolve 
                          (fn search [context data _]
                            (try
                              (log-query context)
                              (lacinia.resolve/resolve-as
                                (search-entity *db* euuid data (executor/selections-tree context)))
                              (catch Throwable e
                                (log/error e  "Couldn't search dataset")
                                (throw e))))}
                         args (assoc :args args))))
              ;; Add recursive getters
              (not-empty recursions)
              (as-> qs
                (reduce
                  (fn [qs' {l :to-label}]
                    (assoc qs'
                      ;;
                      (csk/->camelCaseKeyword (str "get " ename " tree" " by" " " l))
                      {:type (list 'list (entity->gql-object ename))
                       :args get-args 
                       :resolve 
                       (fn tree-getter [context data _]
                         (log/debugf
                           "Resolving recursive structure\n%s"
                           {:entity ename
                            :relation l
                            :data data})
                         (try
                           (log-query context)
                           (get-entity-tree 
                             *db*
                             euuid 
                             (:euuid data) 
                             (keyword l) 
                             (executor/selections-tree context))
                           (catch Throwable e
                             (log/error e "Couldn't resolve GET TREE")
                             (throw e))))}
                      ;;
                      (csk/->camelCaseKeyword (str "search " ename " tree by " l))
                      (let [args (reduce
                                   (fn [r {atype :type aname :name :as attribute}]
                                     (if (ignored-field-type? atype) r 
                                       (assoc 
                                         r 
                                         (keyword (normalize-name aname))
                                         (attribute->type entity attribute))))
                                   {:_where {:type (entity->search-operator entity)}
                                    :_limit {:type 'Int}
                                    :_offset {:type 'Int}
                                    :_order_by {:type (entity->order-by-operator entity)}}
                                   search-arguments)]
                        (cond->
                          {:type (list 'list (entity->gql-object ename))
                           :resolve 
                           (fn tree-search [context data _]
                             (try
                               (log-query context)
                               (let [selection (executor/selections-tree context)] 
                                 (log/debugf
                                   "Searching entity tree\n%s"
                                   {:name ename 
                                    :data data 
                                    :selection selection})
                                 (search-entity-tree *db* euuid (keyword (normalize-name l)) data selection))
                               (catch Throwable e
                                 (log/error e "Couldn't resolve SEARCH TREE")
                                 (throw e))))}
                          args (assoc :args args)))))
                  qs
                  recursions)))))
        {}
        entities))))


(defn sync-mutation
  [{{euuid :euuid} :eywa/entity
    :as context} data _]
  (log-query context)
  (let [{row :euuid} (sync-entity *db* euuid (val (first data)))
        selection (executor/selections-tree context)
        ; _ (log/infof
        ;     :message "Getting entity"
        ;     :entity ename :row row :selection selection)
        value (get-entity *db* euuid {:euuid row} selection)]
    value))


(defn sync-list-mutation
  [{:keys [user roles]
    {euuid :euuid} :eywa/entity
    :as context}
   data
   _]
  (log-query context)
  (let [rows (sync-entity *db* euuid (val (first data)))
        rows' (mapv :euuid rows)
        selection (executor/selections-tree context)
        value (search-entity 
                *db* euuid
                {:_where {:euuid {:_in rows'}}}
                selection)]
    value))


(defn stack-mutation
  [{:keys [user roles] :as context
    {euuid :euuid} :eywa/entity}
   data
   _]
  (log-query context)
  (let [{row :euuid} (stack-entity *db* euuid (val (first data)))
        selection (executor/selections-tree context)
        value (get-entity *db* euuid {:euuid row} selection)]
    value))

(defn stack-list-mutation
  [{:keys [user roles]
    {euuid :euuid} :eywa/entity
    :as context}
   data
   _]
  (log-query context)
  (let [rows (stack-entity *db* euuid (val (first data)))
        rows' (mapv :euuid rows)
        selection (executor/selections-tree context)
        value (search-entity 
                *db*  euuid
                {:_where {:euuid {:_in rows'}}}
                selection)]
    value))


(defn slice-mutation
  [{:keys [user roles]
    :as context
    {euuid :euuid} :eywa/entity}
   data
   _]
  (let [args data
        selection (executor/selections-tree context)] 
    ; (log/trace
    ;   :message "Slicing entity" 
    ;   :euuid euuid :args args :selection selection
    ;   (with-out-str (pprint args)) 
    ;   (with-out-str (pprint selection)))
    (slice-entity *db* euuid args selection)))


(defn delete-mutation
  [{{euuid :euuid} :eywa/entity :as context}
   data
   _]
  (try
    (log-query context)
    (delete-entity *db* euuid data)
    true 
    (catch Throwable e
      (log/error
        :exception e 
        :message "Couldn't delete entity")
      false)))


(defn purge-mutation
  [{:keys [user roles] :as context
    {euuid :euuid} :eywa/entity}
   data
   _]
  (log-query context)
  (let [args data
        selection (executor/selections-tree context)] 
    (purge-entity *db* euuid args selection)))


(defn generate-lacinia-mutations [model]
  (let [entities (core/get-entities model)]
    (reduce
      (fn [mutations {ename :name :as entity}]
        (let [t (entity->gql-input-object ename)
              relations (core/focus-entity-relations model entity)
              to-relations (filter #(not-empty (:to-label %)) relations)] 
          (cond->
            (assoc mutations 
                   ;;
                   (csk/->camelCaseKeyword (str "sync " ename))
                   {:type (entity->gql-object ename)
                    :args {(keyword (normalize-name ename)) {:type (list 'non-null t)}}
                    :resolve 
                    (fn sync [context data value]
                      (sync-mutation (assoc context :eywa/entity entity) data value))}
                   ;; SYNC LIST
                   (csk/->camelCaseKeyword (str "sync " ename " List"))
                   {:type (list 'list (entity->gql-object ename))
                    :args {(keyword (normalize-name ename)) {:type (list 'list t)}} 
                    :resolve 
                    (fn syncList [context data value]
                      (try
                        (sync-list-mutation (assoc context :eywa/entity entity) data value)
                        (catch Throwable e
                          (log/error e "Couldn't resolve SYNC list")
                          (throw e))))}
                   ;;
                   (csk/->camelCaseKeyword (str "stack " ename " List"))
                   {:type (list 'list (entity->gql-object ename))
                    :args {(keyword (normalize-name ename)) {:type (list 'list t)}} 
                    :resolve 
                    (fn stackList [context data value]
                      (try
                        (stack-list-mutation (assoc context :eywa/entity entity) data value)
                        (catch Throwable e
                          (log/error e "Couldn't resolve STACK list")
                          (throw e))))}
                   ;;
                   (csk/->camelCaseKeyword (str "stack" ename))
                   {:type (entity->gql-object ename)
                    :args {(keyword (normalize-name ename)) {:type (list 'non-null t)}}
                    :resolve 
                    (fn stack [context data value]
                      (try
                        (stack-mutation (assoc context :eywa/entity entity) data value)
                        (catch Throwable e
                          (log/error e "Couldn't resolve STACK")
                          (throw e))))}
                   ;;
                   (csk/->camelCaseKeyword (str "delete " ename))
                   (let [allowed? (set (map :euuid (:attributes entity)))
                         uniques (keep
                                   (fn [constraints]
                                     (when-some [real-constraints (not-empty (filter allowed? constraints))]
                                       (vec real-constraints)))
                                   (-> entity :configuration :constraints :unique))
                         args (reduce
                                (fn [args ids]
                                  (reduce
                                    (fn [args id]
                                      (let [{aname :name :as attribute} (core/get-attribute entity id)]
                                        (assoc args (keyword (normalize-name aname)) 
                                               {:type (attribute-type->scalar entity attribute)})))
                                    args
                                    ids))
                                {:euuid {:type 'UUID}}
                                uniques)]
                     (comment
                       (csk/->camelCaseKeyword (str "delete " "OAuth Scope")))
                     ; (log/debugf "Adding delete method for %s\n%s" ename args)
                     {:type 'Boolean
                      :args args 
                      :resolve (fn delete [context data value]
                                 (delete-mutation (assoc context :eywa/entity entity) data value))})
                   ;;
                   (csk/->camelCaseKeyword (str "purge " ename))
                   {:type (list 'list (entity->gql-object ename))
                    :args {:_where {:type (entity->search-operator entity)}}
                    :resolve 
                    (fn purge [context  data value]
                      (try
                        (purge-mutation (assoc context :eywa/entity entity) data value)
                        (catch Throwable e
                          (log/error e "Couldn't resolve purge")
                          (throw e))))})
            ;;
            (not-empty to-relations)
            (assoc
              ;; Slicing
              (csk/->camelCaseKeyword (str "slice " ename))
              {:type (entity->slice-object entity)
               :args {:_where {:type (entity->search-operator entity)}}
               :resolve 
               (fn slice [context  data value]
                 (try
                   (slice-mutation (assoc context :eywa/entity entity) data value)
                   (catch Throwable e
                     (log/error e "Couldn't resolve SLICE")
                     (throw e))))}))))
      {}
      entities)))


(comment
  (def model (dataset/deployed-model))
  (do (generate-lacinia-objects model) nil))


(defn generate-lacinia-schema
  ([] (generate-lacinia-schema (dataset/deployed-model)))
  ([model]
   (let [service-definition {:enums (generate-lacinia-enums model) 
                             :objects (assoc
                                        (generate-lacinia-objects model)
                                        :Mutation {:fields (generate-lacinia-mutations model)}
                                        :Query {:fields (generate-lacinia-queries model)})
                             :input-objects (generate-lacinia-input-objects model)}
         schema service-definition]
     (merge schema {:scalars neyho.eywa.lacinia/scalars}))))
