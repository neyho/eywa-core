(ns neyho.eywa.env
  (:require
    [environ.core :refer [env]]))


(def home (env :eywa-home "~/.eywa/"))
(def pid (str home "pid"))
(def log-dir (env :eywa-log-dir (str home "logs/")))
(def config-dir (env :eywa-config-dir (str home "config/")))
(def git-dir (env :eywa-git-dir (str home "git/")))
