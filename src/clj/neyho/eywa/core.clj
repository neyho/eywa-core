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
    ; neyho.eywa.server.jetty
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


(def version "0.3.2")


(defn setup
  []
  (neyho.eywa.transit/init)
  (let [db (neyho.eywa.db.postgres/from-env)]
    (neyho.eywa.dataset.core/setup db)))


(defn tear-down
  []
  (let [db (neyho.eywa.db.postgres/from-env)]
    (neyho.eywa.dataset.core/tear-down db)))


(defn warmup
  []
  (neyho.eywa.transit/init)
  (neyho.eywa.db.postgres/init)
  (neyho.eywa.dataset/init))


(defn set-superuser
  []
  (let [user (env :eywa-user)
        password (env :eywa-password)]
    (try
      (comment
        (def user "admin")
        (def password "password"))
      (when (and user password)
        (println "Initializing user: " user)
        (warmup)
        (neyho.eywa.iam/setup
          {:users
           [{:name user :password password :active true
             :roles [neyho.eywa.data/*ROOT*]}]
           :roles [neyho.eywa.data/*ROOT*]}))
      (catch Throwable ex
        (log/errorf ex "Couldn't finish EYWA setup.")
        (.println System/err
                  (str/join
                    "\n"
                    ["Couldn't finish EYWA initialization"
                     (ex-message ex)
                     (str "For more info check \"" env/log-dir "\" files")]))
        (System/exit 1)))))


(defn delete-superuser
  []
  (warmup)
  (let [user (env :eywa-user)
        {:keys [euuid]} (neyho.eywa.dataset/get-entity
                          neyho.eywa.iam.uuids/user
                          {:name user}
                          {:euuid nil})]
    (when euuid
      (neyho.eywa.dataset/delete-entity
        neyho.eywa.iam.uuids/user
        {:euuid euuid}))))


(defn list-superusers
  []
  (warmup)
  (let [{users :users} (neyho.eywa.dataset/get-entity
                         neyho.eywa.iam.uuids/user-role
                         {:euuid (:euuid neyho.eywa.data/*ROOT*)}
                         {:euuid nil
                          :users [{:selections
                                   {:name nil}}]})]
    (println (str/join "\n" (map :name users)))))


(defn initialize
  []
  (neyho.eywa.transit/init)
  (println "Setting up DB")
  (let [db (neyho.eywa.db.postgres/from-env)]
    (neyho.eywa.dataset.core/setup db))
  (set-superuser)
  (System/exit 0))


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
                         (neyho.eywa.db.postgres/init)
                         nil
                         (catch Throwable ex (ex-message ex)))]
    (when-not postgres-error
      (try
        (neyho.eywa.dataset.core/get-last-deployed neyho.eywa.db/*db*)
        (println "EYWA is initialized")
        (System/exit 0)
        (catch Throwable ex
          (println "EYWA not initialized")
          (System/exit 1))))))


(defn dataset-doctor
  [lines]
  (neyho.eywa.transit/init)
  (neyho.eywa.iam/init-default-encryption)
  (let [postgres-error (try
                         (neyho.eywa.db.postgres/init)
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


(defn -main
  [& args]
  (let [[command subcommand] args]
    (when (= command "version")
      (println version)
      (System/exit 0))
    (try
      (case command
        "init" (initialize)
        "is-initialized" (is-initialized)
        "super" (case subcommand
                  "add"
                  (do
                    (set-superuser)
                    (System/exit 0))
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
        "start" (do
                  (neyho.eywa.transit/init)
                  (neyho.eywa.iam/init-default-encryption)
                  (oauth/start-maintenance)
                  (neyho.eywa.db.postgres/init)
                  (neyho.eywa.dataset/init)
                  (neyho.eywa.iam/init)
                  (neyho.eywa.dataset.encryption/init)
                  (when (#{"true" "TRUE" "YES" "yes" "y"} (env :eywa-iam-enforce-access))
                    (neyho.eywa.iam.access/start-enforcing))
                  (neyho.eywa.server/start
                    {:port (when-some [port (env :eywa-server-port "8080")] (if (number? port) port (Integer/parseInt port)))
                     :host (env :eywa-server-host "0.0.0.0")
                     ; :context-configurator neyho.eywa.server.jetty/context-configuration
                     }))
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
  )
