;; First load properties from eywa.properties file
;; if any so that environ can pick up those entries
(require '[neyho.eywa.properties])
(neyho.eywa.properties/load-properties)

;;
(require 'neyho.eywa.transit)
(require 'neyho.eywa)
(require 'neyho.eywa.lacinia)
(require 'neyho.eywa.server)
(require 'neyho.eywa.server.jetty)
(require 'neyho.eywa.data)
(require 'neyho.eywa.db.postgres)
(require 'neyho.eywa.avatars.postgres)
(require 'neyho.eywa.authorization)
(require 'neyho.eywa.administration)
(require 'neyho.eywa.dataset)
(require 'neyho.eywa.dataset.core)
(require 'neyho.eywa.dataset.default-model)
(require 'neyho.eywa.dataset.postgres)
(require 'neyho.eywa.dataset.postgres.query)
(require '[neyho.eywa.server.interceptors.authentication :refer [init-default-encryption]])



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


(comment
  (setup)
  (-main)
  (do
    (neyho.eywa.administration/setup
      {:users
       [{:name "test" :password "test" :active true
         :roles [neyho.eywa.data/*ROOT*]}]
       :roles [neyho.eywa.data/*ROOT*]})
    (neyho.eywa.dataset/load-role-schema)))
