(ns neyho.eywa.update
  (:refer-clojure :exclude [sync])
  (:require
   [patcho.patch :as patch]
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

(defmethod compose/prepare [::create-table neyho.eywa.Postgres]
  [_]
  (let [ddl (compose/mlf
             "CREATE TABLE __version_history ("
             " id SERIAL PRIMARY KEY,"
             " version TEXT NOT NULL,"
             " target TEXT NOT NULL,"
             " patched_at TIMESTAMP NOT NULL DEFAULT now()"
             ")")]
    [ddl]))

(defmethod compose/prepare [::insert-version neyho.eywa.Postgres]
  [_ {:keys [version target]}]
  ["INSERT INTO __version_history (version, target) VALUES (?, ?)"
   version target])

(defmethod compose/prepare [::get-latest-version neyho.eywa.Postgres]
  [_ target]
  ["SELECT version, patched_at FROM __version_history where target=? ORDER BY patched_at DESC limit 1" target])

(defmethod compose/prepare [::get-history neyho.eywa.Postgres]
  [_ target]
  (if target
    ["SELECT version, target, patched_at FROM __version_history where target=? ORDER BY patched_at DESC" target]
    ["SELECT version, target, patched_at FROM __version_history ORDER BY patched_at DESC"]))

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
  ([] (history nil))
  ([target]
   (compose/execute! (compose/prepare ::get-history target))))

(defn last-version
  [target]
  (create-table)
  (:version (first (compose/execute! (compose/prepare ::get-latest-version target)))))

(defn add-version
  ([target version]
   (compose/execute! (compose/prepare ::insert-version {:version version :target target}))))

(defn sync
  [target version]
  (create-table)
  (when-not (= (last-version target) version)
    (add-version target version)))

(patch/upgrade
 ::db
 "0.4.0"
 (try
   (last-version "core")
   (catch Throwable _
     (when (table-exists?)
       (delete-table)
       (create-table)))))

(comment
  (compose/execute!
   (let [ddl (compose/mlf
              "CREATE TABLE __version_history ("
              " id SERIAL PRIMARY KEY,"
              " version TEXT NOT NULL,"
              " patched_at TIMESTAMP NOT NULL DEFAULT now()"
              ")")]
     [ddl]))
  (create-table)
  (delete-table)
  (table-exists?)
  (history)
  (history "robotics")
  (history "core")
  (sync "robotics" "0.1.0")
  (sync "core" "0.4.0")
  (sync "core" "0.4.1")
  (sync "core" "0.4.4")
  (sync "core" "0.3.9")
  (sync "0.2.0")
  (last-version))
