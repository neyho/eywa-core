(ns neyho.eywa.core
  (:require
    [clojure.string :as str]
    [environ.core :refer [env]]
    [clojure.tools.logging :as log]
    neyho.eywa.transit
    neyho.eywa
    [neyho.eywa.env :as env]
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
    neyho.eywa.iam
    [neyho.eywa.iam.oauth2 :as oauth2]
    [neyho.eywa.server.interceptors.authentication :refer [init-default-encryption]])
  (:gen-class :main true))


(defn setup
  []
  (neyho.eywa.transit/init)
  (let [db (neyho.eywa.db.postgres/from-env)]
    (neyho.eywa.dataset.core/setup db)))


(defn tear-down
  []
  (let [db (neyho.eywa.db.postgres/from-env)]
    (neyho.eywa.dataset.core/tear-down db)))


(defn set-superuser
  []
  (let [user (env :eywa-user)
        password (env :eywa-password)]
    (try
      (when (and user password)
        (println "Initializing user: " user)
        (neyho.eywa.administration/setup
          {:users
           [{:name user :password password :active true
             :roles [neyho.eywa.data/*ROOT*]}]
           :roles [neyho.eywa.data/*ROOT*]}))
      (catch Throwable ex
        (log/errorf ex "Couldn't finish EYWA setup.")
        (println
          (str/join
            "\n"
            ["Couldn't finish EYWA initialization for more."
             "For more info check \"" env/log-dir "\" files"]))
        (System/exit 1)))))


(defn initialize
  []
  (neyho.eywa.transit/init)
  (println "Setting up DB")
  (let [db (neyho.eywa.db.postgres/from-env)]
    (neyho.eywa.dataset.core/setup db))
  (set-superuser)
  (System/exit 0))


(defn -main
  [& args]
  (let [[command] args]
    (case command
      "init" (initialize)
      "super" (do
                (set-superuser)
                (System/exit 0))
      (do
        (neyho.eywa.transit/init)
        (neyho.eywa.iam/init-default-encryption)
        (oauth2/start-maintenance)
        (init-default-encryption)
        (neyho.eywa.db.postgres/init)
        (neyho.eywa.dataset/init)
        (neyho.eywa.avatars.postgres/init)
        (neyho.eywa.administration/init)
        (neyho.eywa.server/start
          {:port (env :eywa-server-port 8080)
           :host (env :eywa-server-host "localhost")
           :context-configurator neyho.eywa.server.jetty/context-configuration})))))
