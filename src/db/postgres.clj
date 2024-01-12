(ns postgres
  (:require
    [neyho.eywa.data :refer [*ROOT*]]
    [neyho.eywa.administration :as iam]
    neyho.eywa.transit
    neyho.eywa
    neyho.eywa.lacinia
    neyho.eywa.server
    neyho.eywa.server.jetty
    neyho.eywa.db.postgres
    neyho.eywa.avatars.postgres
    neyho.eywa.authorization
    neyho.eywa.dataset
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
  (iam/setup
    {:users
     [{:name (str admin) :password (str password) :active true
       :roles [*ROOT*]}]
     :roles [*ROOT*]}))

(defn tear-down
  [_]
  (let [db (neyho.eywa.db.postgres/from-env)]
    (neyho.eywa.dataset.core/tear-down db)))
