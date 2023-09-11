(ns neyho.eywa.avatars.postgres
  (:require
    neyho.eywa
    neyho.eywa.avatars
    [next.jdbc :as jdbc]
    [taoensso.nippy :as nippy]
    [clojure.data.codec.base64 :as b64]
    [clojure.tools.logging :as log]
    [neyho.eywa.db :refer [*db*]]
    [neyho.eywa.db.postgres.next :as n]))


(defn- table-exists?
  [{:keys [datasource]}]
  (boolean
    (with-open [connection (jdbc/get-connection datasource)]
      (n/execute-one!
        connection
        ["select tablename from pg_tables where schemaname='public' and tablename='__avatars'"]))))


(defn- create-avatars-table
  [{:keys [datasource]}]
  (with-open [connection (jdbc/get-connection datasource)]
    (n/execute-one!
      connection
      ["create table __avatars (
       \"entity\" uuid not null,
       \"record\" uuid not null,
       \"attribute\" uuid not null,
       \"data\" BYTEA,
       constraint \"unique_avatar\" unique(\"entity\",\"record\",\"attribute\")
       )"])))


(extend-type neyho.eywa.Postgres
  neyho.eywa.avatars/AvatarStoreProtocol
  (-get [{:keys [datasource]} hex]
    (let [{:keys [entity record attribute]} (select-keys
                                              (nippy/thaw (b64/decode (.getBytes hex)))
                                              [:entity :attribute :record])]
      (with-open [connection (jdbc/get-connection datasource)]
        (:data
          (n/execute-one!
            connection
            ["select data from __avatars where entity=? and record=? and attribute=?"
             entity record attribute])))))
  (-set [{:keys [datasource]} {:keys [entity record attribute]} payload]
    (with-open [connection (jdbc/get-connection datasource)]
      (n/execute-one!
        connection
        ["insert into __avatars (entity,record,attribute,data)
         values (?,?,?,?)
         on conflict (entity,record,attribute)
         do update set data = excluded.data"
         entity record attribute payload]))))


(comment
  (neyho.eywa.avatars/-get *db* "TlBZAHAEagZlbnRpdHlbChC68j7ZTlOFen4M1Yz2+GoJYXR0cmlidXRlW7KIZWhd4UmjmSqH+2DtGXFqBnJlY29yZFupDBpWzuQR7YmwAqU1iV0tagRzYWx0aQROSXNp"))

(defn init
  []
  (when-not (instance? neyho.eywa.Postgres *db*)
    (throw
      (ex-info
        "Postgres DB is not available"
        {:db *db*})))
  (when-not (table-exists? *db*)
    (log/info "Creating avatars table")
    (create-avatars-table *db*))
  (log/infof "Postgres avatars initialized!")
  (alter-var-root #'neyho.eywa.avatars/*store* (constantly *db*)))
