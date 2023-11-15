(ns neyho.eywa.dataset.core
  (:require
    clojure.set
    clojure.data
    ;; DEPRECATED - only for version 1
    #?(:cljs
       [helix.core :refer [create-context]])
    [clojure.core.async :as async]))


;; DEPRECATED - only for version 1
#?(:cljs (defonce ^:dynamic *dataset* (create-context)))


(defn deep-merge
  "Recursively merges maps."
  [& maps]
  (letfn [(m [& xs]
            (if (some #(and (map? %) (not (record? %))) xs)
              (apply merge-with m xs)
              (last xs)))]
    (reduce m maps)))

(defonce ^:dynamic *return-type* :graphql)
(defonce ^:dynamic *user* nil)
(def client (async/chan 1000))
(def publisher
  (async/pub
    client
    (fn [{:keys [type entity]}]
      [entity type])))


(defn generate-uuid []
  #?(:clj (java.util.UUID/randomUUID)
     :cljs (random-uuid)))

(defprotocol EntityConstraintProtocol
  (set-entity-unique-constraints [this constraints])
  (update-entity-unique-constraints [this function])
  (get-entity-unique-constraints [this]))

(defprotocol GraphQLEntityConfigurationProtocol
  (set-gql-interceptors [this type interceptors])
  (get-gql-interceptors [this type])
  (extend-gql-entity-fields [this fields])
  (remove-gql-entity-fields [this fields]))

(defprotocol GraphQLModelConfigurationProtocol
  (set-gql-object [this name object])
  (set-gql-input-object [this name object])
  (set-gql-query [this name query])
  (set-gql-mutation [this name mutation])
  (get-gql-object [this name])
  (get-gql-input-object [this name])
  (get-gql-query [this name])
  (get-gql-mutation [this name])
  (get-gql-objects [this])
  (get-gql-input-objects [this])
  (get-gql-queries [this])
  (get-gql-mutations[this])
  (remove-gql-object [this name])
  (remove-gql-input-object [this name])
  (remove-gql-query [this name])
  (remove-gql-mutation [this name]))

(defprotocol AuditConfigurationProtocol
  (set-who-field [this name])
  (get-who-field [this])
  (set-when-field [this name])
  (get-when-field [this]))


(defprotocol ModuleLifecycleProtocol
  (setup-module [this version])
  (init-module [this version])
  (tear-down-module [this] [this version]))

(defprotocol ModuleConfigurationProtocol
  (set-init-handler [this handler])
  (get-init-handler [this])
  (set-setup-handler [this handler])
  (get-setup-handler [this])
  (set-tear-down-handler [this handler])
  (get-tear-down-handler [this]))


(defprotocol ERDEntityAttributeProtocol
  (generate-attribute-id [this])
  (add-attribute [this attribute])
  (set-attribute [this attribute])
  (get-attribute [this euuid])
  (update-attribute [this euuid f])
  (remove-attribute [this attribute]))

(defrecord ERDRelation [euuid from to from-label to-label cardinality path])
(defrecord NewERDRelation [euuid entity type])
(defrecord ERDEntityAttribute [euuid seq name constraint type configuration active])

(defrecord ERDEntity [euuid position width height name attributes type configuration]
  ;;
  EntityConstraintProtocol
  (set-entity-unique-constraints [this constraints]
    (assoc-in this [:configuration :constraints :unique] constraints))
  (update-entity-unique-constraints [this f]
    (update-in this [:configuration :constraints :unique] f))
  (get-entity-unique-constraints [this]
    (let [active-attributes (set (map :euuid (filter :active (:attributes this))))]
      (reduce
        (fn [r constraint-group]
          (if-some [filtered-group (not-empty (filter active-attributes constraint-group))]
            (conj r (vec filtered-group))
            r))
        []
        (get-in this [:configuration :constraints :unique]))))
  ;;
  ERDEntityAttributeProtocol
  (generate-attribute-id [_]
    (generate-uuid))
  (add-attribute [{:keys [attributes] :as this} {:keys [euuid] :as attribute}]
    {:pre [(instance? ERDEntityAttribute attribute)]}
    (let [entity (if (some? euuid)
                   (update this :attributes (fnil conj []) 
                           (assoc attribute :seq (count attributes)))
                   (update this :attributes (fnil conj []) 
                           (assoc attribute 
                                  :euuid (generate-attribute-id this)
                                  :seq (count attributes))))]
      (if (= "unique" (:constraint attribute))
        (update-entity-unique-constraints 
          entity
          (fnil #(update % 0 conj euuid) [[]]))
        entity)))
  (get-attribute [{:keys [attributes]} euuid]
    (if-let [attribute (some #(when (= euuid (:euuid %)) %) attributes)]
      attribute
      (throw 
        (ex-info
          (str "Couldn't find attribute with euuid " euuid)
          {:euuid euuid
           :euuids (map :euuid attributes)}))))
  (set-attribute [{:keys [attributes] :as this} 
                  {ct :constraint :as attribute
                   euuid :euuid}]
    (let [p (.indexOf (mapv :euuid attributes) (:euuid attribute))] 
      (if (neg? p)
        (throw
          (ex-info 
            "Attribute not found"
            {:attribute attribute
             :attributes attributes}))
        (let [{pt :constraint} (get attributes p)
              entity (assoc-in this [:attributes p] attribute)] 
          (cond
            ;; If once was unique and currently isn't
            (and (= "unique" pt) (not= "unique" ct))
            (update-entity-unique-constraints 
              entity
              (fn [constraints]
                (mapv #(vec (remove #{euuid} %)) constraints))) 
            ;; If now is unique and previously wasn't
            (and (= "unique" ct) (not= "unique" pt))
            (update-entity-unique-constraints 
              entity
              (fnil #(update % 0 conj euuid) [[]]))
            ;; Otherwise return changed entity
            :else entity)))))
  (update-attribute [{:keys [attributes] :as this} euuid f]
    (if-let [{pt :constraint :as attribute} (some #(when (= euuid (:euuid %)) %) attributes)]
      (let [{ct :constraint :as attribute'} (f attribute)
            entity (set-attribute this attribute')]
        (cond
          ;; If once was unique and currently isn't
          (and (= "unique" pt) (not= "unique" ct))
          (update-entity-unique-constraints 
            entity
            (fn [constraints]
              (mapv #(vec (remove #{euuid} %)) constraints))) 
          ;; If now is unique and previously wasn't
          (and (= "unique" ct) (not= "unique" pt))
          (update-entity-unique-constraints 
            entity
            (fnil #(update % 0 conj euuid) [[]]))
          ;; Otherwise return changed entity
          :else entity))
      (throw (ex-info (str "Couldn't find attribute with euuid " euuid) 
                      {:euuid euuid :euuids (map :euuid attributes)}))))
  (remove-attribute [{:keys [attributes] :as this} {euuid :euuid}]
    (->
      this
      (assoc :attributes 
             (vec 
               (keep-indexed
                 (fn [idx a] (assoc a :seq idx))
                 (remove #(= euuid (:euuid %)) attributes))))
      (update update-entity-unique-constraints
              (fn [unique-bindings]
                (reduce
                  (fn [r group]
                    (let [group' (vec 
                                   (remove 
                                     (some-fn 
                                       #{euuid}
                                       string?) 
                                     group))]
                      (if (empty? group') r (conj r group'))))
                  []
                  unique-bindings))))))

(defprotocol ERDModelActions
  (generate-entity-id [this] "Returns unique id")
  (generate-relation-id [this] "Returns unique id")
  (get-entity [this] [this euuid] "Returns node in model with name if provided, otherwise it returns last entity")
  (get-entities [this] "Returns vector of entities")
  (add-entity [this entity] "Adds new entity to model")
  (set-entity [this entity] "Sets entity in model ignoring previous state")
  (update-entity [this euuid function] "Sets entity in model ignoring previous state")
  (remove-entity [this entity] "Removes node from model")
  (replace-entity 
    [this entity replacement] 
    "Repaces entity in model with replacement and reconects all previous connections")
  (get-entity-relations 
    [this entity] 
    "Returns all relations for given entity where relations
    are returned in such maner that input entity is always in :from field")
  (get-relation [this euuid] "Returns relation between entities")
  (get-relations [this] "Returns vector of relations")
  (get-relations-between [this entity1 entity2] "Returns all found relations that exist between entity1 entity2")
  (add-relation [this relation])
  (create-relation [this from to] [this from to type] [this from to type path] [euuid this from to type path] "Creates relation from entity to entity")
  (set-relation [this relation] "Sets relation in model ignoring previous values")
  (update-relation [this euuid function] "Updates relation in model by merging new values upon old ones")
  (remove-relation [this relation] "Removes relation between entities"))


(defprotocol ERDModelReconciliationProtocol
  (reconcile 
    [this model] 
    "Function reconciles this with that. Starting point should be reconcilation
    of some 'this' with ERDModel, and that might lead to reconiliation of relations
    and entities with 'this'. Therefore reconcile this with that"))

(defrecord ERDModel [entities relations configuration]
  ModuleConfigurationProtocol
  (set-init-handler [this h] (assoc-in this [:configuration :init :handler] h))
  (get-init-handler [_] (get-in configuration [:init :handler]))
  (set-setup-handler [this h] (assoc-in this [:configuration :setup :handler] h))
  (get-setup-handler [_] (get-in configuration [:setup :handler]))
  (set-tear-down-handler [this h] (assoc-in this [:configuration :tear-down :handler] h))
  (get-tear-down-handler [_] (get-in configuration [:tear-down :handler]))
  AuditConfigurationProtocol
  (set-who-field
    [this name]
    (assoc-in this [:configuration :audit :who] name))
  (get-who-field [this] 
    (get-in this [:configuration :audit :who]))
  (set-when-field
    [this name]
    (assoc-in this [:configuration :audit :when] name))
  (get-when-field [this] 
    (get-in this [:configuration :audit :when]))
  GraphQLModelConfigurationProtocol
  (get-gql-objects [this] (get-in this [:configuration :graphql :objects]))
  (get-gql-input-objects [this] (get-in this [:configuration :graphql :input-objects]))
  (get-gql-queries [this] (get-in this [:configuration :graphql :queries]))
  (get-gql-mutations [this] (get-in this [:configuration :graphql :mutations]))
  (get-gql-object [this name] (get-in this [:configuration :graphql :objects name]))
  (get-gql-input-object [this name] (get-in this [:configuration :graphql :input-objects name]))
  (get-gql-query [this name] (get-in this [:configuration :graphql :queries name]))
  (get-gql-mutation [this name] (get-in this [:configuration :graphql :mutations name]))
  (set-gql-object 
    [this name object]
    (assoc-in this [:configuration :graphql :objects name] object))
  (set-gql-input-object 
    [this name object]
    (assoc-in this [:configuration :graphql :input-objects name] object))
  (set-gql-query
    [this name query]
    (assoc-in this [:configuration :graphql :queries name] query))
  (set-gql-mutation
    [this name mutation]
    (assoc-in this [:configuration :graphql :mutations name] mutation))
  (remove-gql-object [this name] 
    (update-in this [:configuration :graphql :objects] dissoc name))
  (remove-gql-input-object [this name] 
    (update-in this [:configuration :graphql :input-objects] dissoc name))
  (remove-gql-query [this name] 
    (update-in this [:configuration :graphql :queries] dissoc name))
  (remove-gql-mutation [this name] 
    (update-in this [:configuration :graphql :mutations] dissoc name)))


(extend-type nil
  GraphQLModelConfigurationProtocol
  (get-gql-objects [_] nil)
  (get-gql-input-objects [_] nil)
  (get-gql-queries [_] nil)
  (get-gql-mutations [_] nil)
  (get-gql-object [_ _] nil)
  (get-gql-input-object [_ _] nil)
  (get-gql-query [_ _] nil)
  (get-gql-mutation [_ _] nil)
  (set-gql-object [_ _ _] nil)
  (set-gql-input-object [_ _ _] nil)
  (set-gql-query [_ _ _] nil)
  (set-gql-mutation [_ _ _] nil)
  (remove-gql-object [_ _] nil)
  (remove-gql-input-object [_ _] nil)
  (remove-gql-query [_ _] nil)
  (remove-gql-mutation [_ _] nil))



(extend-protocol ERDModelActions
  nil
  (get-entities [_] nil)
  (get-entity [_ _] nil)
  (get-relations [_] nil)
  (get-relation [_ _] nil)
  (get-entity-relations [_ _] nil)
  (add-entity [_ _] nil)
  (remove-entity [_ _] nil)
  (replace-entity [_ _ _] nil))


(defprotocol DatasetProtocol
  (deploy! 
    [this module]
    "Deploys dataset for given account")
  (recall!
    [this module]
    "Removes current dataset module from account")
  (get-model
    [this]
    "Returns all entities and relations for given account")
  (mount
    [this module]
    "Mounts module in EYWA by storing its dataset and special handlers")
  (reload
    [this]
    [this module]
    "Reloads module. If module is not specified, than whole tenant is reloaded")
  (unmount
    [this module]
    "Removes module from EYWA by removing all data for that module")
  (get-last-deployed 
    [this] [this offset]
    "Returns last deployed model")
  (setup
    [this]
    [this options]
    "Setup dataset for given DB target")
  (tear-down
    [this]
    [this options]
    "Remove dataset from given DB target")
  (backup
    [this options]
    "Backups dataset for given target based on provided options")
  ; (create-db
  ;   [this]
  ;   "Creates database instance")
  ; (drop-db
  ;   [this]
  ;   "Drops database instance")
  ; (backup-db
  ;   [this options]
  ;   "Creates database backup")
  (create-deploy-history
    [this]
    "Prepares db/storage for deploy history")
  (add-to-deploy-history
    [this] [this model]
    "Stacks current global model to deploy history"))


(defn invert-relation [relation]
  (with-meta
    (-> relation
        (clojure.set/rename-keys
          {:from :to
           :from-label :to-label
           :to :from
           :to-label :from-label})
        (assoc :cardinality 
               (case (:cardinality relation)
                 "o2m" "m2o"
                 "o2o" "o2o"
                 "m2m" "m2m"
                 "m2o" "o2m"
                 relation))
        map->ERDRelation)
    (merge
      (meta relation)
      {:dataset.relation/inverted? true})))


(defn inverted-relation? [relation] (:dataset.relation/inverted? (meta relation)))


(defn direct-relation-from
  [{:keys [euuid]} {:keys [from to to-label] :as relation}]
  (if (= from to) 
    (if (not-empty to-label)
      relation
      (invert-relation relation))
    (if (= euuid (:euuid from)) relation
      (invert-relation relation))))


(defn direct-relations-from
  [entity relations]
  (map #(direct-relation-from entity %) relations))



(defn focus-entity-relations 
  "Function returns entity rel focused on entity, inverting
  all relations that are not outgoing from input entity"
  ([model entity]
   (direct-relations-from entity (get-entity-relations model entity)))
  ([model entity entity']
   (direct-relations-from entity (get-relations-between model entity entity'))))



(defn align-relations
  "Function aligns two relations. By comparing source and
  target node. If needed second relation will be inverted"
  [relation1 relation2]
  (if (= (:euuid relation1) (:euuid relation2))
    (if (= (get-in relation1 [:from :euuid])
           (get-in relation2 [:from :euuid]))
      [relation1 relation2]
      (if (= (get-in relation1 [:from :euuid])
             (get-in relation2 [:to :euuid]))
        [relation1 (invert-relation relation2)]
        (throw 
          (ex-info
            "Cannot align relations that connect different entities"
            {:relations [relation1 relation2]}))))
    (throw
      (ex-info 
        "Cannot align different relations"
        {:relations [relation1 relation2]}))))


(defn same-relations? 
  "Function returns true if two relations are the same, by comparing
  relation1 to relation2 and inverted version of relation2"
  [relation1 relation2]
  (if (= (:euuid relation1) (:euuid relation2))
    (let [[relation1' relation2' relation2''] 
          (map 
            #(->
               %
               (select-keys [:to-label :from-label :cardinality :to :from])
               (update :to :euuid)
               (update :from :euuid))
            [relation1 relation2 (invert-relation relation2)])
          same? (boolean
                  (or
                    (= relation1' relation2')
                    (= relation1' relation2'')))]
      same?)
    false))



(defn join-models [model1 model2]
  (->
    model1
    (update :entities deep-merge (:entities model2))
    (update :relations deep-merge (:relations model2))
    (update :configuration deep-merge (:configuration model2))))


(defprotocol ERDModelProjectionProtocol
  (added? [this] "Returns true if this is added or false otherwise")
  (removed? [this] "Returns true if this is removed or false otherwise")
  (diff? [this] "Returns true if this has diff or false otherwise")
  (diff [this] "Returns diff content")
  (mark-added [this] "Marks this ass added")
  (mark-removed [this] "Marks this as removed")
  (mark-diff [this diff] "Adds diff content")
  (suppress [this] "Returns this before projection")
  (project 
    [this that] 
    "Returns projection of this on that updating each value in nested structure with keys:
    * added?
    * removed?
    * diff
    * active")
  (clean-projection-meta [this] "Returns "))


(defn projection-data [x] (:dataset/projection (meta x)))

(defn attribute-has-diff? 
  [attribute]
  (boolean (not-empty (:diff (projection-data attribute)))))

(defn new-attribute? [attribute] (boolean (:added? (projection-data attribute))))

(defn removed-attribute? [attribute] (boolean (:removed? (projection-data attribute))))

(def attribute-changed? (some-fn new-attribute? removed-attribute? attribute-has-diff?))
(def attribute-not-changed? (complement attribute-changed?))

(defn entity-has-diff? 
  [{:keys [attributes] :as entity}]
  (let [{:keys [diff added?]} (projection-data entity)] 
    (and
      (not added?)
      (or
        (not-empty (dissoc diff :width :height))
        (some attribute-changed? attributes)))))

(defn new-entity? [e] (boolean (:added? (projection-data e))))
(defn strong-entity? [{:keys [type]}] (= "STRONG" type))
(defn weak-entity? [{:keys [type]}] (= "WEAK" type))

(def entity-changed? (some-fn new-entity? entity-has-diff?))
(def entity-not-changed? (complement entity-changed?))

(defn new-relation? [r] (boolean (:added? (projection-data r))))
(defn relation-has-diff? [r] (some? (:diff (projection-data r))))

(def relation-changed? (some-fn new-relation? relation-has-diff?))
(def relation-not-changed? (complement relation-changed?))


(defn recursive-relation? [relation]
  (boolean (#{"tree"} (:cardinality relation))))




(extend-protocol ERDModelProjectionProtocol
  ;; ENTITY ATTRIBUTE
  #?(:clj neyho.eywa.dataset.core.ERDEntityAttribute
     :cljs neyho.eywa.dataset.core/ERDEntityAttribute)
  (mark-added [this] (vary-meta (assoc this :active true) assoc-in [:dataset/projection :added?] true))
  (mark-removed [this] (vary-meta (assoc this :active false) assoc-in [:dataset/projection :removed?] true))
  (mark-diff [this diff] 
    (vary-meta this assoc-in [:dataset/projection :diff] diff))
  (added? [this] (boolean (:added? (projection-data this))))
  (removed? [this] (boolean (:removed? (projection-data this))))
  (diff? [this] (boolean (not-empty (:diff (projection-data this)))))
  (diff [this] (:diff (projection-data this)))
  (clean-projection-meta [this] (vary-meta this dissoc :dataset/projection))
  (suppress [this] 
    (when-let [this' (cond
                       (added? this) nil
                       (diff? this) (merge this (diff this))
                       :else this)]
      (with-meta this' nil)))
  (project
    [{this-id :euuid
      :as this} 
     {that-id :euuid
      :as that}]
    {:pre [(or
             (nil? that)
             (and
               (instance? ERDEntityAttribute that)
               (= this-id that-id)))]}
    ;; FIXME configuration should also implement this protocol or
    ;; at least some multimethod that would return configuration diff
    ;; based on attribute type
    (if (some? that)
      (letfn [(focus-attribute [attribute]
                (select-keys attribute [:euuid :name :type :constraint :active :configuration]))]
        (let [[{config :configuration} n _] 
              (clojure.data/diff 
                (focus-attribute that)
                (focus-attribute this))]
          ;; 1. Check configuration has been extended and that contains more
          ;;    information than this
          ;; 2. Check if some existing attribute changes were made
          (if (or (some? n) (not-empty config))
            ;; READ FIXME - this is dirty fix
            (mark-diff that (or n config))
            that)))
      (mark-removed this)))
  ;; ENTITY
  #?(:clj neyho.eywa.dataset.core.ERDEntity
     :cljs  neyho.eywa.dataset.core/ERDEntity)
  (mark-added [this] 
    (vary-meta 
      (update this :attributes #(mapv mark-added %)) 
      assoc-in [:dataset/projection :added?] true))
  (mark-removed [this] 
    (vary-meta 
      (update this :attributes #(mapv mark-removed %)) 
      assoc-in [:dataset/projection :removed?] true))
  (mark-diff [this diff] (vary-meta this assoc-in [:dataset/projection :diff] diff))
  (added? [this] (boolean (:added? (projection-data this))))
  (removed? [this] (boolean (:removed? (projection-data this))))
  (diff? [this]
    (let [{:keys [diff added?]} (projection-data this)] 
      (and
        (not added?)
        (or
          (not-empty (dissoc diff :width :height))
          (some attribute-changed? (:attributes this))))))
  (diff [this] (:diff (projection-data this)))
  (clean-projection-meta [this] (vary-meta this dissoc :dataset/projection))
  (suppress [this]
    (when-let [this' 
               (cond
                 (added? this) nil
                 ;;
                 (diff? this) 
                 (->
                   this
                   (merge this (dissoc (diff this) :attributes))
                   (update :attributes
                           (fn [as] 
                             (vec 
                               (remove nil? (map suppress as))))))
                 #_(let [cso (get-in (diff this) [:configuration :constraints :unique])] 
                   (cond->
                     
                     (some? cso) (assoc-in [:configuration :constraints :unique] cso)))
                 :else this)]
    (with-meta this' nil)))
  (project
    [{this-id :euuid
      :as this} 
     {that-id :euuid
      :as that}]
    {:pre [(or
             (nil? that)
             (and
               (instance? ERDEntity that)
               (= this-id that-id)))]}
    ;; If that exists
    (if (some? that)
      ;; 
      (let [that-ids (set (map :euuid (:attributes that)))
            this-ids (set (map :euuid (:attributes this)))
            ;; Separate new ids from old and same ids
            [oid nid sid] (clojure.data/diff this-ids that-ids)
            removed-attributes (when (not-empty oid) 
                                 (map
                                   mark-removed
                                   ;; Filter from this attributes
                                   ;; all attributes that are not in that model 
                                   (filter 
                                     (every-pred
                                       :active
                                       (comp oid :euuid)) 
                                     (:attributes this))))
            attributes' (into
                          (reduce
                            ;; Reduce attributes
                            (fn [as {:keys [euuid] :as attribute}]
                              (conj
                                as
                                (cond-> attribute
                                  (and 
                                    (not-empty nid) 
                                    (nid euuid))
                                  mark-added
                                  ;;
                                  (and 
                                    (set? sid) 
                                    (sid euuid))
                                  (as-> a
                                    (project (get-attribute this euuid) a)))))
                            []
                            (:attributes that))
                          ;; at last conj removed attributes with marked :removed? keyword 
                          removed-attributes)
            [o _ _] (when (and this that)
                      (clojure.data/diff
                       (select-keys this [:name :width :height])
                       (select-keys that [:name :width :height])))
            cso (get-in this [:configuration :constraints :unique])
            csn (get-in that [:configuration :constraints :unique])
            changed-attributes (vec (filter attribute-changed? attributes'))]
        (cond->
          (assoc that :attributes attributes')
          ;;
          (some? o)
          (vary-meta assoc-in [:dataset/projection :diff] o)
          ;;
          (not= cso csn)
          (vary-meta assoc-in [:dataset/projection :diff :configuration :constraints :unique] cso)
          ;;
          (not-empty changed-attributes)
          (vary-meta assoc-in [:dataset/projection :diff :attributes] changed-attributes)))
      ;; If that doesn't exist return this with :removed? metadata
      (mark-removed this)))
  #?(:clj neyho.eywa.dataset.core.ERDRelation
     :cljs neyho.eywa.dataset.core/ERDRelation)
  (mark-added [this] (vary-meta this assoc-in [:dataset/projection :added?] true))
  (mark-removed [this] (vary-meta this assoc-in [:dataset/projection :removed?] true))
  (mark-diff [this diff] (vary-meta this assoc-in [:dataset/projection :diff] diff))
  (added? [this] (boolean (:added? (projection-data this))))
  (removed? [this] (boolean (:removed? (projection-data this))))
  (diff? [this] (boolean (not-empty (:diff (projection-data this)))))
  (diff [this] (:diff (projection-data this)))
  (clean-projection-meta [this] (vary-meta this dissoc :dataset/projection))
  (suppress [this]
    (when-let [this'
               (cond
                 (added? this) nil
                 (diff? this) (->
                                this
                                (merge (dissoc (diff this) :from :to))
                                (update :from suppress)
                                (update :to suppress)
                                (with-meta nil))
                 :else this)]
      (with-meta this' nil)))
  (project
    [this that]
    {:pre [(or 
             (nil? that)
             (and 
               (instance? ERDRelation that)
               (= (:euuid this) (:euuid that))))]}
    ;; If that exists
    (if (some? that) 
      ;; Check if relations are the same
      (let [ks [:from-label :to-label :cardinality]
            this (if (inverted-relation? this) (invert-relation this) this)
            that (if (inverted-relation? that) (invert-relation that) that)
            ;; Compute difference between this and that
            [o _] (clojure.data/diff 
                    (select-keys this ks) 
                    (select-keys that ks))
            ;; Check only entity names since that might
            ;; affect relation
            from-projection (when (not= 
                                  (:name (:from this))
                                  (:name (:from that)))
                              {:name (:name (:from that))})
            to-projection (when (not= 
                                  (:name (:to this))
                                  (:name (:to that)))
                            {:name (:name (:to that))})
            o' (cond-> o
                 from-projection (assoc :from from-projection)
                 to-projection (assoc :to to-projection))]
        ;; And if there is some difference than
        (if (some? o') 
          ;; return that with projected difference
          (mark-diff that o')
          ;; otherwise return that
          that))
      ;; If that does't exist than return this with projected removed metadata
      (mark-removed this)))
  #?(:clj neyho.eywa.dataset.core.ERDModel
     :cljs neyho.eywa.dataset.core/ERDModel)
  (clean-projection-meta [this] (vary-meta this dissoc :dataset/projection))
  (suppress [this]
    (with-meta
      (reduce
        (fn [m r]
          (->
            m
            (set-relation (suppress r))
            (with-meta nil)))
        (reduce
          (fn [m e]
            (->
              m
              (set-entity (suppress e))
              (with-meta nil)))
          this
          (get-entities this))
        (get-relations this))
      nil))
  (project
    [this that]
    (reduce
      (fn [m {id :euuid :as r}]
        (set-relation m (project (get-relation this id) r)))
      (reduce
        (fn [m {id :euuid :as e}]
          (set-entity m (project (get-entity this id) e)))
        that
        (get-entities that))
      (get-relations that)))
  nil
  (mark-removed [_] nil)
  (mark-added [_] nil)
  (mark-diff [_ _] nil)
  (project [_ that] (when that (mark-added that))))


;; DEPRECATED
(let [queries #{:sync :tree :get :search :aggregate :delete :stack :slice}] 
  (extend-protocol GraphQLEntityConfigurationProtocol
    ERDEntity
    (set-gql-interceptors [this t interceptors]
      {:pre (queries t)}
      (assoc-in this [:configuration :graphql :interceptors t] (vec interceptors)))
    (get-gql-interceptors [this t]
      (get-in this [:configuration :graphql :interceptors t] []))
    (extend-gql-entity-fields [this fields]
      (update-in this [:configuration :graphql :extend] merge fields))
    (remove-gql-entity-fields [this fields]
      (update-in this [:configuration :graphql :extend] dissoc fields))))
