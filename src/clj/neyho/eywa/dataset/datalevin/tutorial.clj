(ns neyho.eywa.dataset.datalevin.tutorial
  (:require
    [nano-id.core :refer [nano-id]]
    [camel-snake-kebab.core :as csk]
    [datalevin.core :as d]
    [neyho.eywa.dataset :as dataset]
    [neyho.eywa.dataset.core :as core]))


(defn add-users-and-roles [conn]
  (let [role-id1 (nano-id)
        role-id2 (nano-id)]
    (d/tempid
      (d/transact! conn
                   [{:db/id role-id1 
                     :role/name "Administrator"}
                    {:db/id role-id2 
                     :role/name "Editor"}
                    {:db/id "3i1X6ak_KHhJ-zelnDCZ3"
                     :user/name "Alice"
                     :user/roles [role-id1]}
                    {:db/id "2VV1jvKejr2SweZZdpDma"
                     :user/name "Bob"
                     :user/roles [role-id1 role-id2]}]))))

(def conn
  (d/get-conn
    "/tmp/eywa/datalevin_tutorial"
    {:user/name {:db/valueType :db.type/string
                 :db/unique :db.unique/identity}
     :user/roles {:db/valueType :db.type/ref
                  :db/cardinality :db.cardinality/many}
     :role/name {:db/valueType :db.type/string
                 :db/unique :db.unique/identity}}))


(defn fetch-all-entities [conn]
  (d/q
    '[:find ?e ?attr ?value
      :where [?e ?attr ?value]]
    (d/db conn)))




(comment
  (d/close conn)
  (d/pull
    (d/db conn)
    [:db/id :user/name {:user/roles [:db/id :role/name]}] 1)
  (d/pull
    (d/db conn)
    [:db/id :role/name {:user/_roles [:db/id :user/name]}] 2)
  (d/q
    '[:find [(pull ?e [*]) ...]
      :where
      [?e :user/name ?name]]
    (d/db conn))
  ;;
  (d/q
    '[:find [(pull ?e [*]) ...]
      :where
      [?e :role/name ?name]]
    (d/db conn)))
