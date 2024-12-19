(ns neyho.eywa.env
  (:require
   [babashka.fs :as fs]
   [environ.core :refer [env]]))

(def home (str (fs/expand-home (env :eywa-home "~/.eywa"))))
(def pid (str home "/pid"))
(def log-dir (str (fs/absolutize (env :eywa-log-dir (str home "/logs")))))
(def config-dir (str (fs/absolutize (env :eywa-config-dir (str home "/config")))))
(def git-dir (str (fs/absolutize (env :eywa-git-dir (str home "/git")))))

(def iam-root-url (env
                   :eywa-iam-root-url
                   (env
                    :eywa-iam-url
                    "http://localhost:8080")))

(def encryption-master (env :eywa-dataset-encryption-key))
