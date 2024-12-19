(ns neyho.eywa.core
  (:require
   [babashka.fs :as fs]
   [clojure.string :as str]
   [clojure.tools.logging :as log]
   [clojure.java.shell :refer [sh]]
   [environ.core :refer [env]]
   neyho.eywa.transit
   neyho.eywa
   [neyho.eywa.env :as env]
   neyho.eywa.lacinia
   neyho.eywa.server
   neyho.eywa.data
   neyho.eywa.db.postgres
   neyho.eywa.dataset
   neyho.eywa.dataset.core
   neyho.eywa.dataset.encryption
   neyho.eywa.dataset.default-model
   neyho.eywa.dataset.postgres
   neyho.eywa.dataset.postgres.query
   neyho.eywa.iam
   neyho.eywa.iam.uuids
   neyho.eywa.iam.access
   [neyho.eywa.iam.oauth :as oauth])
  (:gen-class :main true))

(def version "0.3.3-SNAPSHOT")

(defn setup
  ([] (setup (neyho.eywa.db.postgres/from-env)))
  ([db]
   (neyho.eywa.transit/init)
   (neyho.eywa.dataset.core/setup db)))

(defn warmup
  ([] (warmup (neyho.eywa.db.postgres/from-env)))
  ([db]
   (neyho.eywa.transit/init)
   (neyho.eywa.db.postgres/start db)
   (neyho.eywa.dataset/start)))

(comment
  (set-superuser
   {:username "admin"
    :password "admin"}))

(defn set-superuser
  ([] (set-superuser {:username  (env :eywa-user)
                      :password (env :eywa-password)}))
  ([user]
   (set-superuser (neyho.eywa.db.postgres/from-env) user))
  ([db {:keys [username password]}]
   (when (and username password)
     (println "Initializing user: " username)
     (warmup db)
     (neyho.eywa.iam/setup
      {:users
       [{:name username :password password :active true
         :roles [neyho.eywa.data/*ROOT*]}]
       :roles [neyho.eywa.data/*ROOT*]}))))

(defn delete-superuser
  ([] (delete-superuser (neyho.eywa.db.postgres/from-env)))
  ([db] (delete-superuser db (env :eywa-user)))
  ([db {:keys [username]}]
   (warmup db)
   (let [{:keys [euuid]} (neyho.eywa.dataset/get-entity
                          neyho.eywa.iam.uuids/user
                          {:name username}
                          {:euuid nil})]
     (when euuid
       (neyho.eywa.dataset/delete-entity
        neyho.eywa.iam.uuids/user
        {:euuid euuid})))))

(defn list-superusers
  ([] (list-superusers (neyho.eywa.db.postgres/from-env)))
  ([db]
   (warmup db)
   (let [{users :users} (neyho.eywa.dataset/get-entity
                         neyho.eywa.iam.uuids/user-role
                         {:euuid (:euuid neyho.eywa.data/*ROOT*)}
                         {:euuid nil
                          :users [{:selections
                                   {:name nil}}]})]
     (println (str/join "\n" (map :name users))))))

(defn initialize
  ([]
   (initialize (neyho.eywa.db.postgres/from-env)))
  ([db]
   (neyho.eywa.transit/init)
   (println "Setting up DB")
   (neyho.eywa.dataset.core/setup db)
   (set-superuser)))

(let [padding-left "    "
      table-length 70
      hline (str \+ (apply str (repeat (- table-length 2) \-)) \+)]
  (defn doctor-table
    [lines]
    (letfn [(row [text]
              (str "|" text (apply str (repeat (- table-length 2 (count text)) " ")) "|"))]
      (str/join
       "\n"
       (map
        #(str padding-left %)
        (concat
         [hline
          (row "")]
         (map row lines)
         [(row "")
          hline]))))))

(defn java-info
  []
  (let [{:keys [exit out err]} (try
                                 (sh "java" "-version")
                                 (catch Throwable ex
                                   (log/error ex "Invalid JAVA scan")))]
    (when (and exit (zero? exit))
      (let [output (some #(when (not-empty %) %) [out err])
            [_ _ version build-time] (re-find #"(java\s+version\s+\")([\d\.]+)\"\s*([\d\-]+)" output)
            [_ build :as all] (re-find #"build(.*?)\)" output)]
        {:version version
         :build  build
         :build-time build-time}))))

(defn valid-java?
  [info]
  (if-some [{:keys [version]} info]
    (condp #(.startsWith %2 %1) version
      "17." true
      "11." true
      false)
    false))

(defn java-doctor
  [lines]
  (let [{java-version :version :as jinfo} (java-info)]
    (as-> (or lines []) lines
      (if (valid-java? jinfo)
        (conj lines (str "  JAVA     " (str "OK: version '" java-version "' is supported")))
        (conj lines
              (str "  JAVA     " (str "ERROR: current version '" java-version "' is not supported"))
              (str "         Use JAVA versions 11,17"))))))

(defn is-initialized
  []
  (let [postgres-error (try
                         (neyho.eywa.db.postgres/start)
                         nil
                         (catch Throwable ex (ex-message ex)))]
    (when-not postgres-error
      (neyho.eywa.dataset.core/get-last-deployed neyho.eywa.db/*db*)
      (println "EYWA is initialized"))))

(defn dataset-doctor
  [lines]
  (neyho.eywa.transit/init)
  (neyho.eywa.iam/init-default-encryption)
  (let [postgres-error (try
                         (neyho.eywa.db.postgres/start)
                         nil
                         (catch Throwable ex (ex-message ex)))
        dataset-error (when-not postgres-error
                        (try
                          (neyho.eywa.dataset.core/get-last-deployed neyho.eywa.db/*db*)
                          nil
                          (catch Throwable ex
                            (log/error ex "Doctor failed to check datasets")
                            "EYWA not initialized")))]
    (as-> lines lines
      ;; Check postgres
      (if postgres-error
        (conj lines (str "  POSTGRES " (str "ERROR: " postgres-error)))
        (conj lines (str "  POSTGRES " (str "OK"))))
      ;; Check datasets
      (if dataset-error
        (conj lines (str "  DATASETS " (str "ERROR: " dataset-error)))
        (if-not postgres-error
          (conj lines (str "  DATASETS OK"))
          (conj lines (str "  DATASETS " (str "ERROR: Postgres not available"))))))))

(defn doctor []
  (->
   []
   java-doctor
   dataset-doctor
   doctor-table
   println))

(defn spit-pid
  []
  (let [target (env :eywa-pid (fs/expand-home "~/.eywa/pid"))
        pid (let [process-handle (java.lang.ProcessHandle/current)]
              (.pid process-handle))]
    (spit (str target) (str pid))))

(defn stop
  []
  (oauth/stop)
  (neyho.eywa.db.postgres/stop)
  (neyho.eywa.dataset/stop)
  (neyho.eywa.iam/stop)
  (neyho.eywa.dataset.encryption/stop)
  (neyho.eywa.server/stop)
  nil)

(defn start
  ([] (start (neyho.eywa.db.postgres/from-env)))
  ([db] (start
         db
         {:port (when-some [port (env :eywa-server-port "8080")] (if (number? port) port (Integer/parseInt port)))
          :host (env :eywa-server-host "0.0.0.0")
          :info {:version version
                 :release-type "core"}}))
  ([db options]
   (stop)
   (neyho.eywa.transit/init)
   (neyho.eywa.iam/init-default-encryption)
   (oauth/start)
   (neyho.eywa.db.postgres/start db)
   (neyho.eywa.dataset/start)
   (neyho.eywa.iam/start)
   (neyho.eywa.dataset.encryption/start)
   (when (#{"true" "TRUE" "YES" "yes" "y"} (env :eywa-iam-enforce-access))
     (neyho.eywa.iam.access/start))
   (neyho.eywa.server/start options)))

(defn tear-down
  ([] (tear-down (neyho.eywa.db.postgres/from-env)))
  ([db]
   (stop)
   (neyho.eywa.dataset.core/tear-down db)))

(defn -main
  [& args]
  (let [[command subcommand] args]
    (when (= command "version")
      (println version)
      (System/exit 0))
    (try
      (case command
        "init" (do
                 (initialize)
                 (System/exit 0))
        "is-initialized" (try
                           (is-initialized)
                           (System/exit 0)
                           (catch Throwable ex
                             (println "EYWA not initialized")
                             (System/exit 1)))
        "super" (case subcommand
                  "add"
                  (try
                    (set-superuser)
                    (System/exit 0)
                    (catch Throwable ex
                      (log/errorf ex "Couldn't finish EYWA setup.")
                      (.println System/err
                                (str/join
                                 "\n"
                                 ["Couldn't finish EYWA initialization"
                                  (ex-message ex)
                                  (str "For more info check \"" env/log-dir "\" files")]))
                      (System/exit 1)))
                  "delete"
                  (do
                    (delete-superuser)
                    (System/exit 0))
                  "list"
                  (do
                    (list-superusers)
                    (System/exit 0)))
        "doctor" (do
                   (doctor)
                   (System/exit 0))
        "start" (start)
        (do
          (.print System/err (str "Unknown args: " args))
          (System/exit 1)))
      (catch Throwable ex
        (.printStackTrace ex)
        (System/exit 1))
      (finally (spit-pid)))))

(comment
  (str/replace "09jfiqo-123 39:foiq" #"[^\w^\d^\-^\.^_:]" "")
  (re-find #"^[\w\d\-\._]" "09jfiqo-123 39")
  (start)
  (stop))
