(ns user
  (:require 
    neyho.eywa.transit
    neyho.eywa
    neyho.eywa.properties
    neyho.eywa.lacinia
    neyho.eywa.server
    neyho.eywa.server.jetty
    neyho.eywa.data
    neyho.eywa.db.postgres
    neyho.eywa.avatars.postgres
    neyho.eywa.authorization
    neyho.eywa.administration
    neyho.eywa.dataset
    neyho.eywa.dataset.core
    neyho.eywa.dataset.default-model
    neyho.eywa.dataset.postgres
    neyho.eywa.dataset.postgres.query
    [neyho.eywa.server.interceptors.authentication :refer [init-default-encryption]])
  (:gen-class :main true))


(neyho.eywa.properties/load-properties)


(defn setup
  []
  (neyho.eywa.transit/init)
  (let [db (neyho.eywa.db.postgres/from-env)]
    (neyho.eywa.dataset.core/setup db)))


(defn tear-down
  []
  (let [db (neyho.eywa.db.postgres/from-env)]
    (neyho.eywa.dataset.core/tear-down db)))


(defn -main
  []
  (neyho.eywa.transit/init)
  (init-default-encryption)
  (neyho.eywa.db.postgres/init)
  (neyho.eywa.dataset/init)
  (neyho.eywa.avatars.postgres/init)
  (neyho.eywa.administration/init)
  (neyho.eywa.server/start
    {:context-configurator neyho.eywa.server.jetty/context-configuration}))


; (-main)
