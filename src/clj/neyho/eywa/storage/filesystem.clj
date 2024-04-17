(ns neyho.eywa.storage.filesystem
  (:require
    clojure.string
    [environ.core :refer [env]]
    [clojure.java.io :as io]
    [clojure.tools.logging :as log]
    [neyho.eywa]
    [neyho.eywa.storage :as storage]
    [babashka.fs :as fs]))


(def separator java.io.File/separator)


(defn- not-supported []
  (throw (Exception. "NOT SUPPORTED ON LOCALSTORAGE")))


(defn- path [root key]
  (str
    (fs/expand-home root)
    (when-not (.startsWith key separator) separator)
    key))


(defn from-env
  []
  (env :local-storage-root ".eywa/storage/"))


(defn setup
  ([] (setup (from-env)))
  ([root]
   (try
     (fs/create-dirs (fs/expand-home root))
     (catch Throwable e
       (log/error e "Couldn't start LocalStorage service")))))


(defn tear-down
  ([] (tear-down (from-env)))
  ([root]
   (try
     (fs/delete-tree root)
     (catch Throwable e
       (log/error e "Couldn't tear down LocalStorage")))))

(defn init
  ([] (init (from-env)))
  ([root]
   (let [storage (neyho.eywa/->LocalStorage root)]
     (setup root)
     (alter-var-root #'neyho.eywa.storage/*storage* (constantly storage)))))


(extend-type neyho.eywa.LocalStorage
  storage/StorageProtocol
  (-available? [{:keys [root]}]
    (let [root' (fs/expand-home root)]
      (and
        (fs/exists? root')
        (fs/directory? root'))))
  (-search 
    ([this] (storage/-search this nil))
    ([_ _] (not-supported)))
  (-upload
    [{:keys [root]} file key]
    (let [target (io/file (path root key))]
      (io/make-parents target)
      (with-open [f (io/output-stream target)]
        (io/copy file f))))
  (-download 
    ([{:keys [root]} key]
     (io/input-stream (io/file (path root key))))
    ([_ _ _] 
     (not-supported))
    ([_ _ _ _]
     (not-supported)))
  (-delete 
    ([this key] (storage/-delete this key nil)
     (fs/delete-if-exists key))
    ([_ _ _]
     (not-supported))))
