(ns neyho.eywa.core
  (:require
    [clojure.string :as str]
    [clojure.tools.logging :as log]
    [clojure.java.shell :refer [sh]]
    [environ.core :refer [env]]
    neyho.eywa.transit
    neyho.eywa
    [neyho.eywa.env :as env]
    neyho.eywa.lacinia
    neyho.eywa.server
    neyho.eywa.server.jetty
    neyho.eywa.data
    neyho.eywa.db.postgres
    neyho.eywa.avatars.postgres
    neyho.eywa.authorization
    neyho.eywa.administration
    neyho.eywa.dataset
    neyho.eywa.dataset.core
    neyho.eywa.dataset.default-model
    neyho.eywa.dataset.postgres
    neyho.eywa.dataset.postgres.query
    neyho.eywa.iam
    neyho.eywa.iam.uuids
    [neyho.eywa.iam.oauth2 :as oauth2]
    [neyho.eywa.server.interceptors.authentication :refer [init-default-encryption]])
  (:gen-class :main true))


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
  (init-default-encryption)
  (neyho.eywa.db.postgres/init)
  (neyho.eywa.dataset/init))


(defn set-superuser
  []
  (let [user (env :eywa-user)
        password (env :eywa-password)]
    (try
      (when (and user password)
        (println "Initializing user: " user)
        (warmup)
        (neyho.eywa.administration/setup
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


(defn doctor []
  (let [{java-version :version :as jinfo} (java-info)
        info-lines (as-> [] lines
                     (if (valid-java? jinfo)
                       (conj lines (str "  JAVA - " (str "OK: current version '" java-version "' is supported")))
                       (conj lines
                             (str "  JAVA - " (str "ERROR: current version '" java-version "' is not supported"))
                             (str "         Use JAVA versions 11,17"))))]
    (println (doctor-table info-lines))))


(defn -main
  [& args]
  (let [[command subcommand] args]
    (case command
      "init" (initialize)
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
      (do
        (neyho.eywa.transit/init)
        (neyho.eywa.iam/init-default-encryption)
        (oauth2/start-maintenance)
        (init-default-encryption)
        (neyho.eywa.db.postgres/init)
        (neyho.eywa.dataset/init)
        (neyho.eywa.avatars.postgres/init)
        (neyho.eywa.administration/init)
        (neyho.eywa.server/start
          {:port (when-some [port (env :eywa-server-port 8080)] (Integer/parseInt port))
           :host (env :eywa-server-host "localhost")
           :context-configurator neyho.eywa.server.jetty/context-configuration})))))
