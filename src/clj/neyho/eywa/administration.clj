(ns neyho.eywa.administration
  (:require 
    clojure.string
    [clojure.java.io :as io]
    [clojure.tools.logging :as log]
    [neyho.eywa.data
     :refer [*EYWA*
             *ROOT*]]
    [neyho.eywa.lacinia :as lacinia]
    [neyho.eywa.authorization.components :as c]
    [neyho.eywa.administration.uuids :as au]
    [neyho.eywa.iam.access.context :refer [*user*]]
    [neyho.eywa.dataset :as dataset]
    [neyho.eywa.dataset.core :as core])
  (:import
    [java.lang StringBuilder]))


(def alphabet (.toCharArray "0123456789abcdefghijklmnopqrstuvwxyz"))
(def MASK 35)


(defn hash-uuid [uuid]
  (let [^long lo (.getLeastSignificantBits uuid)
        ^long hi (.getMostSignificantBits uuid)
        uuid-bytes (-> (java.nio.ByteBuffer/allocate 16)
                       (.putLong hi)
                       (.putLong lo)
                       (.array))
        builder (StringBuilder.)]
    (.toString
      (reduce
        (fn [b by]
          (.append b (get alphabet (bit-and by MASK))))
        builder
        uuid-bytes))))



(defn get-permissions []
  (letfn [(get-uuids [col]
            (set (map (fn [{e :euuid}] e) col)))] 
    (let [permissions 
          (map 
            #(->
               %
               (update :roles get-uuids)
               (update :parent :euuid)) 
            (dataset/get-entity-tree
              au/permission
              c/eywa
              :parent
              {:euuid nil
               :roles [{:selections {:euuid nil :name nil}}]
               :name nil}))]
      permissions)))


(defn root?
  [roles]
  (contains? (set roles) (:euuid *ROOT*)))


(defn user-uuid [args]
  (:euuid (dataset/get-entity au/user args {:euuid nil})))


;; @hook
(defn reload-schema
  [ctx args v]
  (log/info "Reloading access schema")
  (dataset/load-role-schema)
  [ctx args v])


(defn init
  []
  (log/info "Initializing Administration...")
  (try
    (dataset/load-role-schema)
    (log/info "Administration initialized")
    (lacinia/add-shard ::administration (slurp (io/resource "administration.graphql")))
    (catch Throwable e
      (log/error e "Couldn't load role schema"))))


(def permissions
  [{:euuid c/eywa
    :name "EYWA"}
   {:euuid c/administration
    :name "Administration"
    :roles [*ROOT*]
    :parent {:euuid c/eywa}}
   {:euuid c/users
    :name "Users"
    :roles [*ROOT*]
    :parent {:euuid c/administration}}
   {:euuid c/user-add
    :name "User Add"
    :roles [*ROOT*]
    :parent {:euuid c/users}}
   {:euuid c/user-modify
    :name "User Edit"
    :roles [*ROOT*]
    :parent {:euuid c/users}}
   {:euuid c/user-delete
    :name "User Delete"
    :roles [*ROOT*]
    :parent {:euuid c/users}}
   {:euuid c/groups
    :name "Groups"
    :roles [*ROOT*]
    :parent {:euuid c/administration}}
   {:euuid c/group-add
    :name "Group Add"
    :roles [*ROOT*]
    :parent {:euuid c/groups}}
   {:euuid c/group-modify
    :name "Group Edit"
    :roles [*ROOT*]
    :parent {:euuid c/groups}}
   {:euuid c/group-delete
    :name "Group Delete"
    :roles [*ROOT*]
    :parent {:euuid c/groups}}
   {:euuid c/group-members
    :name "Group Members"
    :roles [*ROOT*]
    :parent {:euuid c/groups}}
   {:euuid c/roles
    :name "Roles"
    :roles [*ROOT*]
    :parent {:euuid c/administration}}
   {:euuid c/role-add
    :name "Role Add"
    :roles [*ROOT*]
    :parent {:euuid c/roles}}
   {:euuid c/role-modify
    :name "Role Edit"
    :roles [*ROOT*]
    :parent {:euuid c/roles}}
   {:euuid c/role-delete
    :name "Role Delete"
    :roles [*ROOT*]
    :parent {:euuid c/roles}}
   {:euuid c/role-members
    :name "Role Members"
    :roles [*ROOT*]
    :parent {:euuid c/roles}}
   {:euuid c/role-permissions
    :name "Role Members"
    :roles [*ROOT*]
    :parent {:euuid c/roles}}])


(defn setup
  [{:keys [users groups roles services]}] 
  (binding [core/*return-type* :edn
            *user* (:_eid *EYWA*)]
    (log/info  "Creating ROOT user role")
    (dataset/sync-entity au/user-role *ROOT*)
    (log/info "ROOT user role created")
    (doseq [user users]
      (log/infof "Adding user %s" (dissoc user :password))
      (dataset/sync-entity 
        au/user 
        (assoc user :avatar nil :type :PERSON)))
    (doseq [group groups] 
      (log/infof "Adding user group %s" group)
      (dataset/sync-entity 
        au/user-group
        (assoc group :avatar nil)))
    (doseq [role roles]
      (log/infof "Adding user role %s" role)
      (dataset/sync-entity 
        au/user-role
        (assoc role :avatar nil)))
    (doseq [service services
            :let [euuid (java.util.UUID/randomUUID)]]
      (log/infof "Adding service %s" service)
      (dataset/sync-entity 
        au/user 
        (assoc service 
               :euuid euuid
               :type :SERVICE
               :avatar nil)))))


(comment
  (setup
    {:users
     [{:name "rgersak" :password "change-me" :active true :roles [*ROOT*]}]}))
