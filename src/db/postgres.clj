(require '[neyho.eywa.properties])
(neyho.eywa.properties/load-properties)

;;
(require '[clojure.string :as str])
(require 'neyho.eywa.transit)
(require 'neyho.eywa)
(require 'neyho.eywa.lacinia)
(require 'neyho.eywa.server)
(require 'neyho.eywa.server.jetty)
(require '[neyho.eywa.data :refer [*ROOT*]])
(require 'neyho.eywa.db.postgres)
(require 'neyho.eywa.avatars.postgres)
(require 'neyho.eywa.authorization)
(require '[neyho.eywa.administration :as iam])
(require 'neyho.eywa.dataset)
(require 'neyho.eywa.dataset.core)
(require 'neyho.eywa.dataset.default-model)
(require 'neyho.eywa.dataset.postgres)
(require 'neyho.eywa.dataset.postgres.query)
(require '[neyho.eywa.administration.uuids :as au])


#_(ns postgres
  (:require
    [clojure.string :as str]
    [neyho.eywa.data :refer [*ROOT*]]
    [neyho.eywa.administration :as iam]
    [neyho.eywa.administration.uuids :as au]
    neyho.eywa.transit
    neyho.eywa
    neyho.eywa.lacinia
    neyho.eywa.server
    neyho.eywa.server.jetty
    neyho.eywa.db.postgres
    neyho.eywa.avatars.postgres
    neyho.eywa.authorization
    [neyho.eywa.dataset :as dataset]
    neyho.eywa.dataset.core
    neyho.eywa.dataset.default-model
    neyho.eywa.dataset.postgres
    neyho.eywa.dataset.postgres.query))

(defn bootstrap
  [{:keys [admin password]}]
  (neyho.eywa.transit/init)
  (neyho.eywa.transit/init)
  (let [db (neyho.eywa.db.postgres/from-env)]
    (neyho.eywa.dataset.core/setup db))
  (when (and admin password)
    (iam/setup
      {:users
       [{:name (str admin) :password (str password) :active true
         :type :PERSON
         :roles [*ROOT*]}]
       :roles [*ROOT*]})))

(defn tear-down
  [_]
  (let [db (neyho.eywa.db.postgres/from-env)]
    (neyho.eywa.dataset.core/tear-down db)))


(defn init-eywa
  []
  (neyho.eywa.transit/init)
  (neyho.eywa.db.postgres/init)
  (neyho.eywa.dataset/init))


(defn add-user
  [{:syms [user password type active]
    :or {type "PERSON"
         active true}}]
  (init-eywa)
  (dataset/sync-entity
    au/user
    {:name (str user)
     :password (str password)
     :type (str/upper-case (name type))
     :active active}))
