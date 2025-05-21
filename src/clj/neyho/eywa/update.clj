(ns neyho.eywa.update
  (:refer-clojure :exclude [sync])
  (:require
   [neyho.eywa.dataset.sql.compose :as compose]))

(defmethod compose/prepare [::create-table neyho.eywa.Postgres]
  [_]
  (let [ddl (compose/mlf
             "CREATE TABLE __version_history ("
             " id SERIAL PRIMARY KEY,"
             " version TEXT NOT NULL,"
             " patched_at TIMESTAMP NOT NULL DEFAULT now()"
             ")")]
    [ddl]))

(defmethod compose/prepare [::insert-version neyho.eywa.Postgres]
  [_ {:keys [version]}]
  ["INSERT INTO __version_history (version) VALUES (?)"
   version])

(defmethod compose/prepare [::get-latest-version neyho.eywa.Postgres]
  [_]
  ["SELECT version, patched_at FROM __version_history ORDER BY patched_at DESC limit 1"])

(defmethod compose/prepare [::get-history neyho.eywa.Postgres]
  [_]
  ["SELECT version, patched_at FROM __version_history ORDER BY patched_at DESC"])

(defmethod compose/prepare [::exists? neyho.eywa.Postgres]
  [_]
  ["SELECT to_regclass('public.__version_history')"])

(defmethod compose/prepare [::delete-table neyho.eywa.Postgres]
  [_]
  ["drop table __version_history"])

(defn table-exists?
  []
  (let [[{result :to_regclass}] (compose/execute! (compose/prepare ::exists?))]
    (some? result)))

(defn create-table
  []
  (when-not (table-exists?)
    (compose/execute! (compose/prepare ::create-table))))

(defn delete-table
  []
  (compose/execute! (compose/prepare ::delete-table)))

(defn history
  []
  (compose/execute! (compose/prepare ::get-history)))

(defn last-version
  []
  (create-table)
  (:version (first (compose/execute! (compose/prepare ::get-latest-version)))))

(defn add-version
  ([version]
   (compose/execute! (compose/prepare ::insert-version {:version version}))))

(defn sync
  [version]
  (create-table)
  (when-not (= (last-version) version)
    (add-version version)))

(comment
  (create-table)
  (delete-table)
  (table-exists?)
  (history)
  (sync "0.1.0")
  (sync "0.2.0")
  (last-version))
