(ns neyho.eywa.dataset.default-model
  (:use
   [neyho.eywa.dataset.core :as dataset])
  (:require
   clojure.set
   clojure.data))

(extend-type
 #?(:clj neyho.eywa.dataset.core.ERDModel
    :cljs neyho.eywa.dataset.core/ERDModel)
  neyho.eywa.dataset.core/ERDModelActions
  (generate-entity-id [_]
    (dataset/generate-uuid))
  (generate-relation-id [_]
    (dataset/generate-uuid))
  (get-entity [{:keys [entities]} euuid] (get entities euuid))
  (add-entity [this {:keys [euuid] :as entity}]
    (assert (not-any? #{euuid} (map :euuid (dataset/get-entities this))) (str "Model already contains entity " euuid ":" (:name entity)))
    (let [euuid (if (nil? euuid) (dataset/generate-entity-id this) euuid)]
      (assoc-in this [:entities euuid] (assoc entity :euuid euuid))))
  (remove-entity [{:keys [entities] :as this} {:keys [euuid] :as entity}]
    (let [relations' (map :euuid (dataset/get-entity-relations this entity))]
      (->
       this
       (update :entities dissoc entities euuid)
       (update :relations #(reduce dissoc % relations')))))
  (replace-entity
    [this {:keys [euuid position] :as entity} {euuid' :euuid :as replacement}]
    (assert (some #{euuid} (map :euuid (dataset/get-entities this))) (str "Model doesn't contain entity " euuid ":" (:name entity)))
    (assert (not-any? #{euuid'} (map :euuid (dataset/get-entities this))) (str "Model already contains entity " euuid' ":" (:name replacement)))
    (let [old-relations (dataset/get-entity-relations this entity)]
      (reduce
       (fn [model {:keys [from to] :as relation}]
          ;; Add relation by checking which direction to change
         (dataset/add-relation
          model
          (cond-> relation
            (= euuid (:euuid from))
            (-> (assoc :from euuid') (update :to :euuid))
              ;;
            (= euuid (:euuid to))
            (-> (assoc :to euuid') (update :from :euuid)))))
       (->
        this
          ;; Remove entity removes all old connections
        (dataset/remove-entity entity)
          ;; Add new entity as replacement at the same position
        (dataset/add-entity (assoc replacement :position position)))
        ;; reduce all old connections and reconnect
       old-relations)))
  (set-entity [this {:keys [euuid] :as entity}]
    (update this :entities assoc euuid entity))
  (update-entity [this euuid f]
    (dataset/set-entity this (f (dataset/get-entity this euuid))))
  (get-relation [{:keys [relations] :as this} euuid]
    (some-> (get relations euuid)
            (update :from (partial dataset/get-entity this))
            (update :to (partial dataset/get-entity this))))
  (add-relation [this {:keys [euuid] :as relation}]
    (update this :relations assoc euuid relation))
  (create-relation
    ([this from to cardinality path]
     (let [euuid (dataset/generate-relation-id this)]
       (assoc-in this [:relations euuid]
                 (dataset/map->ERDRelation
                  {:euuid euuid
                   :from (:euuid from)
                   :to (:euuid to)
                   :cardinality cardinality
                   :path path}))))

    ([this from to cardinality]
     (dataset/create-relation this from to cardinality nil))
    ([this from to]
     (dataset/create-relation this from to "o2o")))
  (set-relation [this relation]
    (assoc-in this [:relations (:euuid relation)]
              (->
               relation
               (update :from :euuid)
               (update :to :euuid))))
  (update-relation [this euuid f]
    (dataset/set-relation this (f (dataset/get-relation this euuid))))
  (remove-relation [this relation]
    (update this :relations dissoc (:euuid relation)))
  (get-entities [{:keys [entities]}]
    (vec (mapv val (sort-by :name entities))))
  (get-relations [{:keys [relations] :as this}]
    (reduce
     (fn [relations relation]
       (conj relations
             (-> relation
                 (update :from (partial dataset/get-entity this))
                 (update :to (partial dataset/get-entity this)))))
     []
     (mapv val relations)))
  (get-relations-between
    [this {e1 :euuid} {e2 :euuid}]
    (let [valid? #{e1 e2}]
      (filter
       (fn [{{t1 :euuid} :from {t2 :euuid} :to}]
         (= #{t1 t2} valid?))
       (dataset/get-relations this))))
  (get-entity-relations [{:keys [relations] :as this} {:keys [euuid]}]
    (let [looking-for #{euuid}]
      (reduce
       (fn [r {:keys [from to] :as relation}]
         (let [relation' (->
                          relation
                          (update :from (partial dataset/get-entity this))
                          (update :to (partial dataset/get-entity this)))]
           (cond-> r
             (looking-for from) (conj relation')
             (looking-for to) (conj relation'))))
       []
       (vals relations)))))
