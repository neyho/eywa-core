(ns neyho.eywa.db.postgres
  (:require
    [neyho.eywa :as eywa]
    [neyho.eywa.db]
    [clojure.tools.logging :as log]
    [next.jdbc :as jdbc]
    [environ.core :refer [env]]
    [neyho.eywa.db.postgres.next
     :refer [execute-one! clear-connections]])
  (:import 
    [com.zaxxer.hikari HikariDataSource]))


(defn postgres-connected? [datasource] (not (.isClosed datasource)))


(defn connect
  "Connects neyho.eywa.Postgres to server and returns HikariDataSource instance"
  [^neyho.eywa.Postgres {:keys [host port user db password max-connections]
                         :or {max-connections 2}
                         :as data}]
  (let [url (str "jdbc:postgresql://" host \: port \/ db)
        datasource (doto
                     (HikariDataSource.)
                     (.setDriverClassName "org.postgresql.Driver")
                     (.setJdbcUrl url)
                     (.setUsername user)
                     (.setPassword password)
                     (.setLeakDetectionThreshold 2000)
                     (.setInitializationFailTimeout 0)
                     (.addDataSourceProperty "connectionInitSql" "SET TIME ZONE 'UTC'")
                     (.setMaximumPoolSize max-connections)
                     (.setConnectionTestQuery "select 1")
                     (.setKeepaliveTime 5000)
                     (.setConnectionTimeout 30000)
                     (.setIdleTimeout 30000)
                     (.setValidationTimeout 5000))]
    (when-not (postgres-connected? datasource)
      (throw (ex-info "Couldn't connect to Postgres" data)))
    (log/infof "[%s]Connected to %s PostgresDB" user url)
    (assoc data :datasource datasource)))


(comment
  (def db (connect (from-env)))
  (def db
    (connect
      (neyho.eywa/map->Postgres
        {:host "eywa-development.caffmhmswqhy.eu-west-1.rds.amazonaws.com"
         :port "5432" 
         :user "eywa"
         :password "wZV2F9LZ8jeMmq7v"
         :max-connections 2
         :db "kb_dev"})))
  (.close (-> db :datasource))
  (def db
    (connect
      (neyho.eywa/map->Postgres
        {:host "drka-zbrka-10115.7tc.aws-eu-central-1.cockroachlabs.cloud"
         :port "26257"
         :user "robert"
         :max-connections 2
         :password "mNg5OTMVEakGL9hpXDFslA"
         :db "defaultdb"}))))


(defn check-connection-params
  [{:keys [host db user password] :as data}]
  (letfn [(check [x message]
            (when-not x (throw (ex-info message data))))]
    (check host "POSGRES_HOST not specified")
    (check db "POSTGRES_DB not specified")
    (check user "POSTGRES_USER not specified")
    (check password "POSTGRES_PASSWORD not specified")))


(defn from-env
  "Builds neyho.eywa.Postgres instance from environment variables"
  []
  (let [host (env :postgres-host "localhost")
        port (env :postgres-port 8080)
        db (env :postgres-db)
        password (env :postgres-password)
        user (env :postgres-user)
        data (hash-map :host host 
                       :port port 
                       :db db 
                       :password password 
                       :user user 
                       :max-connections (Integer/parseInt (env :hikari-max-pool-size "2")))]
    (check-connection-params data)
    (eywa/map->Postgres data)))


(defn admin-from-env
  "Builds neyho.eywa.Postgres instance fron environment variables. Admin db should be
  used to create new db for new tenants"
  []
  (let [host (env :postgres-host)
        port (env :postgres-port 5432)
        admin-db (env :postgres-admin-db (env :postgres-db))
        password (env :postgres-admin-password (env :postgres-password))
        user (env :postgres-admin-user (env :postgres-user))
        data (hash-map :host host
                       :port port
                       :db admin-db
                       :password password 
                       :user user 
                       :max-connections (Integer/parseInt (env :hikari-max-pool-size "2")))]
    (check-connection-params data)
    (eywa/map->Postgres data)))


(defn create-db
  "Function will setup create new database using admin account. When new database
  is created function will return HikariDataSource connection to that new database"
  [^neyho.eywa.Postgres {:keys [host] :as admin} database-name]
  (let [admin-db (connect admin)]
    (log/infof "Creating database %s at %s" database-name host)
    ;; Admin DB
    (try
      (with-open [connection (jdbc/get-connection (:datasource admin-db))]
        (execute-one!
          connection
          [(format "create database %s" database-name)]))
      (let [db (connect (assoc admin :db database-name))]
        (try
          (with-open [connection (jdbc/get-connection (:datasource db))]
            (execute-one!
              connection
              ["create extension \"uuid-ossp\""]))
          (catch Throwable ex
            (println "Couldn't create uuid-ossp extension")
            (throw ex)))
        (log/infof "Database %s created at %s" database-name host)
        db)
      (catch Throwable ex
        (println "Couldn't create Database: \"" database-name\")
        (throw ex))
      ;; New DB
      (finally
        (.close (:datasource admin-db))))))


(defn drop-db
  "Removes DB from Postgres server"
  [{:keys [host] :as admin} database]
  (log/infof "Droping DB %s at %s" database host)
  (let [admin (connect admin)]
    (try
      (with-open [con (:datasource admin)]
        (clear-connections con database)
        (execute-one!
          con
          [(format "drop database if exists %s" database)]))
      (finally
        (.close (:datasource admin)))))
  nil)


(defn backup
  "Function will connect to admin db and backup 'database' under 'backup' name"
  [^neyho.eywa.Postgres admin database backup]
  (let [db (connect admin)]
    (with-open [con (jdbc/get-connection (:datasource db))]
      (jdbc/execute!
        con
        [(format "SELECT pg_terminate_backend(pid) FROM pg_stat_activity WHERE datname='%s'" database)])
      (jdbc/execute!
        con
        [(format "create database %s with template '%s'" backup database)]))
    true))


(defonce connection-agent (agent {:running? true}))


(defn monitor-connection
  [{:keys [running? period]
    :or {running? true
         period 10000}
    :as data} database]
  (if-not running? data
    (do
      (when-not (postgres-connected? (:datasource neyho.eywa.db/*db*))
        (try
          (when (nil? database)
            (throw
              (ex-info
                "Database not specified"
                data)))
          (when-let [db (connect database)]
            (alter-var-root #'neyho.eywa.db/*db* (constantly db))
            nil)
          (catch Throwable e
            (log/errorf e "Couldn't connect to DB"))))
      (send-off *agent* monitor-connection database)
      (Thread/sleep period))))


(comment
  (.getActiveConnections (.getHikariPoolMXBean (:datasource neyho.eywa.db/*db*)))
  (.getIdleConnections (.getHikariPoolMXBean (:datasource neyho.eywa.db/*db*)))
  (postgres-connected? (:datasource neyho.eywa.db/*db*))
  )


(defn start-connection-monitor
  [database]
  (send-off connection-agent (fn [_] {:running? true :period 10000}))
  (send-off connection-agent monitor-connection database))


(defn stop-connection-monitor
  []
  (send-off connection-agent (fn [x] (assoc x :running? false)))
  (when-some [db neyho.eywa.db/*db*]
    (when (postgres-connected? (:datasource db))
      (.close (:datasource db))
      (alter-var-root #'neyho.eywa.db/*db* (constantly nil)))))


(defn init
  "Initializes database connection and returns HikariDataSource instance"
  ([] (init (from-env)))
  ([database]
   (when-let [db (connect database)]
     (alter-var-root #'neyho.eywa.db/*db* (constantly db))
     nil)
   (start-connection-monitor database)))
