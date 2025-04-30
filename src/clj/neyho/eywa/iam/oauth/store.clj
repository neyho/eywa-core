(ns neyho.eywa.iam.oauth.store
  (:require
   [clojure.string :as str]
   [neyho.eywa.dataset.sql.compose :as compose]))

(def -client- #uuid "0757bd93-7abf-45b4-8437-2841283edcba")
(def -session- #uuid "b2562198-0817-4508-a941-d898373298e5")
(def -access- #uuid "405d7201-a74a-490d-a5f7-669701d1a735")
(def -refresh- #uuid "6a6511b9-0616-4eee-99e0-729c6058985c")

(defmethod compose/prepare [::create-consent-table neyho.eywa.Postgres]
  [_]
  #_(let [ddl (compose/mlf
               "create table __oidc_user_consent ("
               "   \"user\" bigint not null references \"%s\"(_eid) on delete cascade," (compose/table -oidc-user-)
               "   \"client\" bigint not null references \"%s\"(_eid) on delete cascade," (compose/table -oidc-client-)
               "   \"scope\" bigint not null references \"%s\"(_eid) on delete cascade," (compose/table -oidc-scope-)
               "   \"consent_at\" TIMESTAMP default now(),"
               "   unique (\"user\", \"client\", \"scope\")"
               ")")]
      [ddl]))

(defmethod compose/prepare [::delete-consent-table neyho.eywa.Postgres]
  [_]
  ["drop table __oidc_user_consent"])

(defmethod compose/prepare [::exists? neyho.eywa.Postgres]
  [_]
  ["SELECT to_regclass('public.__oidc_user_consent')"])

(defn create-consent-table
  []
  (compose/execute! (compose/prepare ::create-consent-table)))

(defn delete-consent-table
  []
  (compose/execute! (compose/prepare ::delete-consent-table)))

(defn consent-table-exists?
  []
  (let [[{result :to_regclass}] (compose/execute! (compose/prepare ::exists?))]
    (some? result)))

(comment
  (create-consent-table)
  (delete-consent-table)
  (consent-table-exists?))

(defmethod compose/prepare [::provide-consent neyho.eywa.Postgres]
  [_ {:keys [username client-id scopes]}]
  #_(into
     [(compose/ml
       (format "WITH data (username, clientid, scopename) AS (VALUES %s" (str/join ", " (repeat (count scopes) "(?,?,?)")))
       "), ids AS ("
       "SELECT "
       (format "(SELECT _eid FROM \"%s\" WHERE name = d.username) AS user, " (compose/table -oidc-user-))
       (format "(SELECT _eid FROM \"%s\" WHERE client_id = d.clientid) AS client, " (compose/table -oidc-client-))
       (format "(SELECT _eid FROM \"%s\" WHERE name = d.scopename) AS scope" (compose/table -oidc-scope-))
       "FROM data d"
       ") "
       "INSERT INTO __oidc_user_consent (user, client, scope)"
       "SELECT user, client, scope FROM ids")]
     (map (fn [scope] (vector username client-id scope)) scopes)))

(comment
  (def scopes [100 200 300 400]))

(defn user-provided-consent [user client scopes]
  (compose/execute!
   (compose/prepare
    ::provide-consent
    {:user user :client client :scopes scopes})))
