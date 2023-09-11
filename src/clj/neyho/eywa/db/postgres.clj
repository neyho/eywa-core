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
  [^neyho.eywa.Postgres {:keys [host port user db password max-connections] :as data}]
  (let [url (str "jdbc:postgresql://" host \: port \/ db)
        datasource (doto
                     (HikariDataSource.)
                     (.setDriverClassName "org.postgresql.Driver")
                     (.setJdbcUrl url)
                     (.setUsername user)
                     (.setPassword password)
                     (.addDataSourceProperty "connectionInitSql" "SET TIME ZONE 'UTC'")
                     (.setMaximumPoolSize max-connections))]
    (when-not (postgres-connected? datasource)
      (throw (ex-info "Couldn't connect to Postgres" data)))
    (log/infof "[%s]Connected to %s PostgresDB" user url)
    (assoc data :datasource datasource)))


(defn from-env
  "Builds neyho.eywa.Postgres instance from environment variables"
  []
  (eywa/map->Postgres
    (hash-map :host (env :postgres-host)
              :port (env :postgres-port)
              :db (env :postgres-db)
              :password (env :postgres-password)
              :user (env :postgres-user)
              :max-connections (Integer/parseInt (env :hikari-max-pool-size "2")))))


(defn admin-from-env
  "Builds neyho.eywa.Postgres instance fron environment variables. Admin db should be
  used to create new db for new tenants"
  []
  (eywa/map->Postgres
    (hash-map :host (env :postgres-host)
              :port (env :postgres-port)
              :db (env :postgres-admin-db (env :postgres-db))
              :password (env :postgres-admin-password (env :postgres-password))
              :user (env :postgres-admin-user (env :postgres-user))
              :max-connections (Integer/parseInt (env :hikari-max-pool-size "2")))))


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
        (with-open [connection (jdbc/get-connection (:datasource db))]
          (execute-one!
            connection
            ["create extension \"uuid-ossp\""]))
        (log/infof "Database %s created at %s" database-name host)
        db)
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
      (jdbc/execute-one!
        con
        [(format "SELECT pg_terminate_backend(pid) FROM pg_stat_activity WHERE datname='%s';create database %s with template '%s'" database backup database)]))
    true))


(comment
  (def admin (assoc (admin-from-env) :db "eywa"))
  (def admin (connect admin))
  (with-open [con (jdbc/get-connection (:datasource db))])
  (def database "a1_mk")
  (backup admin "a1_mk" "a1_mk_25082023")
  )


(defn init
  "Initializes database connection and returns HikariDataSource instance"
  ([] (init (from-env)))
  ([database]
   (when-let [db (connect database)]
     (alter-var-root #'neyho.eywa.db/*db* (constantly db))
     db)))
