(ns neyho.eywa.core
  (:require
   [babashka.fs :as fs]
   [clojure.string :as str]
   [clojure.tools.logging :as log]
   [clojure.java.shell :refer [sh]]
   [environ.core :refer [env]]
   [patcho.patch :as patch]
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
   neyho.eywa.dataset.graphql
   neyho.eywa.dataset.postgres
   neyho.eywa.dataset.postgres.query
   neyho.eywa.iam
   neyho.eywa.iam.uuids
   neyho.eywa.iam.access
   neyho.eywa.iam.oauth.store
   [neyho.eywa.update :as update]
   [neyho.eywa.health :as health]
   [neyho.eywa.iam.oauth :as oauth])
  (:gen-class :main true))

(patch/current-version :eywa/core "0.5.1")

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

(defn add-eywa-client-redirects
  ([]
   (add-eywa-client-redirects
    {:redirections (when-some [redirects (not-empty (env :eywa-iam-allowed-redirections
                                                         (env :eywa-oauth-allowed-redirections)))]
                     (let [redirects (str/split redirects #"\s*,\s*")]
                       (map str/trim redirects)))
     :logout (when-some [redirects (not-empty (env :eywa-iam-allowed-logouts
                                                   (env :eywa-oauth-allowed-logouts)))]
               (let [redirects (str/split redirects #"\s*,\s*")]
                 (map str/trim redirects)))}))
  ([{:keys [redirections logout]}]
   (when (some not-empty [redirections logout])
     (let [client (neyho.eywa.dataset/get-entity
                   neyho.eywa.iam.uuids/app
                   {:id "MUMADPADAKQHSDFDGFAEJZJXUSFJGFOOYTWVAUDEFVPURUOP"}
                   {:euuid nil
                    :settings nil})
           updated-client (update client :settings
                                  (fn [current]
                                    (->
                                     current
                                     (update "redirections"
                                             (fn [_redirections]
                                               (distinct
                                                (into _redirections redirections))))
                                     (update "logout-redirections"
                                             (fn [_redirections]
                                               (into _redirections logout))))))]
       (neyho.eywa.dataset/stack-entity
        neyho.eywa.iam.uuids/app
        updated-client)))))

(comment
  (add-eywa-client-redirects
   {:redirections ["http://localhost:8000/eywa/callback"
                   "http://localhost:8000/eywa/silent-callback"]
    :logout ["http://localhost:8000/eywa"]}))

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
  ([db username]
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
   (neyho.eywa.dataset/init-delta-pipe)
   (neyho.eywa.dataset.core/setup db)
   (set-superuser)))

(let [padding-left "    "
      table-length 70
      hline (str \+ (apply str (repeat (- table-length 2) \-)) \+)]
  (defn doctor-table
    [lines]
    (letfn [(row [text]
              text
              #_(str "|" text)
              #_(str "|" text (apply str (repeat (- table-length 2 (count text)) " ")) "|"))]
      (str/join
       "\n"
       (map
        #(str padding-left %)
        (concat
         [(row "")]
         #_[hline
            (row "")]
         (map row lines)
         [(row "")]))))))

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
  (if-some [version (:version info)]
    (condp #(.startsWith %2 %1) version
      "17." true
      "11." true
      false)
    false))

(defmethod health/doctor "JAVA"
  [_]
  (let [{java-version :version :as jinfo} (java-info)]
    (as-> [] lines
      (if (valid-java? jinfo)
        (conj lines (str "✅ JAVA" (str ": version '" java-version "' is supported")))
        (conj lines
              (str "❌ JAVA" (str ": current version '" java-version "' is not supported"))
              (str "         Use JAVA versions 11+"))))))

(defn is-initialized
  []
  (let [postgres-error (try
                         (neyho.eywa.db.postgres/start)
                         nil
                         (catch Throwable ex (ex-message ex)))]
    (when-not postgres-error
      (neyho.eywa.dataset.core/get-last-deployed neyho.eywa.db/*db*)
      (println "EYWA is initialized"))))

(defmethod health/doctor "DATASETS"
  [_]
  (neyho.eywa.transit/init)
  (neyho.eywa.iam/init-default-encryption)
  (let [postgres-error (try
                         (neyho.eywa.db.postgres/start)
                         nil
                         (catch Throwable ex (ex-message ex)))
        dataset-error (when-not postgres-error
                        (try
                          (neyho.eywa.dataset/start)
                          (neyho.eywa.dataset.core/get-last-deployed neyho.eywa.db/*db*)
                          nil
                          (catch Throwable ex
                            (log/error ex "Doctor failed to check datasets")
                            "EYWA not initialized")))]
    (as-> [] lines
      ;; Check postgres
      (if postgres-error
        (conj lines (str "❌ POSTGRES" (str ": " postgres-error)))
        (conj lines (str "✅ POSTGRES ")))
      ;; Check datasets
      (if dataset-error
        (conj lines (str "❌ DATASETS " (str ": " dataset-error)))
        (if-not postgres-error
          (conj lines (str "✅ DATASETS"))
          (conj lines (str "❌ DATASETS " (str ": Postgres not available"))))))))

(defn doctor []
  (let [lines (reduce
               (fn [lines organ]
                 (if-some [new-lines (health/doctor organ)]
                   (into lines new-lines)
                   lines))
               []
               (keys (methods health/doctor)))]
    (println (doctor-table lines))))

(defn spit-pid
  []
  (let [target (env :eywa-pid (fs/expand-home "~/.eywa/pid"))
        pid (let [process-handle (java.lang.ProcessHandle/current)]
              (.pid process-handle))]
    (spit (str target) (str pid))))

(patch/upgrade
 :eywa/core
 "0.4.0"
 (try
   (update/last-version "core")
   (catch Throwable _
     (when (update/table-exists?)
       (update/delete-table)
       (update/create-table)))))

(patch/upgrade
 :eywa/core
 "0.5.0"
 (neyho.eywa.dataset.postgres/fix-int-types)
 (neyho.eywa.dataset.postgres/fix-mandatory-constraints))

(defn stop
  []
  (oauth/stop)
  (neyho.eywa.db.postgres/stop)
  (neyho.eywa.dataset/stop)
  (neyho.eywa.iam/stop)
  (neyho.eywa.dataset.encryption/stop)
  (neyho.eywa.server/stop)
  nil)

(def default-options
  {:port (when-some [port (env :eywa-server-port "8080")]
           (if (number? port) port (Integer/parseInt port)))
   :host (env :eywa-server-host "0.0.0.0")
   :info (patch/available-versions :eywa/core)})

(defn start
  ([] (start (neyho.eywa.db.postgres/from-env)))
  ([db]
   (start db default-options))
  ([db options]
   (comment
     (def db (neyho.eywa.db.postgres/from-env)))
   (stop)
   (neyho.eywa.transit/init)
   (oauth/start)
   (neyho.eywa.db.postgres/start db)
   (neyho.eywa.dataset/start)
   (neyho.eywa.iam/start)
   (neyho.eywa.dataset.encryption/start)
   (neyho.eywa.iam.oauth.store/start)
   (neyho.eywa.iam.access/start)
   (add-eywa-client-redirects)
   (neyho.eywa.server/start options)
   (patch/apply
    :eywa/core
    (try
      (update/last-version "core")
      (catch Throwable _ nil)))
   (neyho.eywa.update/sync "core" (patch/version :eywa/core))))

(defn tear-down
  ([] (tear-down (neyho.eywa.db.postgres/from-env)))
  ([db]
   (stop)
   (neyho.eywa.dataset.core/tear-down db)))

(defn -main
  [& args]
  (let [[command subcommand] args]
    (when (= command "version")
      (let [eywa-versions (filter
                           (fn [[k]]
                             (when (keyword? k)
                               (namespace k)
                               (= "eywa" (namespace k))))
                           (patch/available-versions))
            sorted (sort-by key eywa-versions)
            to-print (map (fn [[k v]] (str (namespace k) \. (name k) "=" v)) sorted)]
        (println (str/join "\n" to-print)))
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
