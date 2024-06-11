(ns neyho.eywa.iam.access
  (:require
    [neyho.eywa.dataset :as dataset]
    [neyho.eywa.iam.uuids :as iu]))


(defonce ^:dynamic *rules* nil)


(defn get-roles-access-data
  []
  (dataset/search-entity
    iu/user-role
    nil
    {:euuid nil
     :name nil
     ;; Entities
     :write_entites [{:selections {:euuid nil}}]
     :read_entities [{:selections {:euuid nil}}]
     :delete_entities [{:selections {:euuid nil}}]
     :owned_entities [{:selections {:euuid nil}}]
     ;; Relations
     :to_read_relations [{:selections {:euuid nil}}]
     :to_write_relations [{:selections {:euuid nil}}]
     :to_delete_relations [{:selections {:euuid nil}}]
     :from_read_relations [{:selections {:euuid nil}}]
     :from_write_relations [{:selections {:euuid nil}}]
     :from_delete_relations [{:selections {:euuid nil}}]}))


(defn transform-roles-data
  [data]
  (letfn [(x-entity [result role rule entities]
            (reduce
              (fn [r entity]
                (update-in r [:entity entity rule] (fnil conj #{}) role))
              result
              entities))
          (x-relation [result role direction rule relations]
            (reduce
              (fn [r relation]
                (update-in r [:relation relation direction rule] (fnil conj #{}) role))
              result
              relations))]
    (reduce
      (fn [r {:keys [euuid
                     write_entites read_entities delete_entities owned_entites
                     to_read_relations to_write_relations to_delete_relations
                     from_read_relations from_write_relations from_delete_relations]}]
        (-> r
            (x-entity euuid :read (map :euuid read_entities))
            (x-entity euuid :write (map :euuid write_entites))
            (x-entity euuid :delete (map :euuid delete_entities))
            (x-entity euuid :owners (map :euuid owned_entites))
            (x-relation euuid :from :read (map :euuid from_read_relations))
            (x-relation euuid :from :write (map :euuid from_write_relations))
            (x-relation euuid :from :delete (map :euuid from_delete_relations))
            (x-relation euuid :to :read (map :euuid to_read_relations))
            (x-relation euuid :to :write (map :euuid to_write_relations))
            (x-relation euuid :to :delete (map :euuid to_delete_relations))))
      nil
      data)))



(defn load-rules
  []
  (alter-var-root #'*rules* (fn [_] (transform-roles-data (get-roles-access-data)))))
