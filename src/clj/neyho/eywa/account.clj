(ns neyho.eywa.account
  (:require
    [clojure.string :as str]
    [neyho.eywa.setup :as s]
    [next.jdbc :as jdbc]
    [neyho.eywa :as eywa]
    [neyho.config :refer [load-config]]
    [com.stuartsierra.component :as component]))


(defn new-account
  ([account-name] (new-account account-name nil))
  ([account-name {:keys [s3 postgres]}]
   (let [defaults (->
                    (load-config)
                    (assoc-in [:s3 :bucket] (str "eywa" \. (str/replace account-name #"[\s_]+" "."))))
         ;; TODO this will override default config data and set DB alwasys to eywa
         postgres (assoc postgres :db "eywa")]
     (->
       defaults
       (assoc :account account-name)
       (update :postgres merge postgres)
       (update :s3 merge s3)))))


(defn backup-account [{:keys [account postgres]} backup-name]
  (let [{:keys [datasource] :as db} (component/start (eywa/map->Postgres (assoc postgres :db "eywa")))]
    (with-open [con (jdbc/get-connection datasource)]
      (jdbc/execute-one!
        con
        [(format "SELECT pg_terminate_backend(pid) FROM pg_stat_activity WHERE datname='%s';create database %s with template '%s'" account backup-name account)]))
    (component/stop db)
    true))


(defn eywa
  ([{:keys [account s3 postgres users groups roles]}]
   (component/system-map
     :postgres (eywa/map->Postgres (assoc postgres :account account))
     :s3 (eywa/map->S3 (assoc s3 :account account))
     ;;
     :lacinia (eywa/->Lacinia (ref nil) (ref nil))
     ;;
     :datasets
     (component/using
       (eywa/map->Datasets nil)
       {:db :postgres
        :storage :s3
        :lacinia :lacinia})

     ;;
     :administration 
     (component/using
       (eywa/map->Administration
         {:users users 
          :groups groups 
          :roles roles
          :account account})
       [:datasets :lacinia :eywa]))))


(defn setup [config]
  (-> config eywa s/setup))


(defn tear-down [config]
  (-> config eywa s/tear-down))
