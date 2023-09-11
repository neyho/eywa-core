(ns neyho.eywa.avatars.fs
  (:require
    clojure.string
    [clojure.java.io :as io]
    [clojure.tools.logging :as log]
    [neyho.eywa]
    neyho.eywa.avatars
    [taoensso.nippy :as nippy]
    [clojure.data.codec.base64 :as b64]
    [babashka.fs :as fs]))


(def separator java.io.File/separator)


(defonce root "~/.eywa/storage")


(defn setup
  []
  (try
    (fs/create-dirs (fs/expand-home root))
    (catch Throwable e
      (log/error e "Couldn't start LocalStorage service")))
  nil)


(defn tear-down
  []
  (try
    (fs/delete-tree root)
    (catch Throwable e
      (log/error e "Couldn't tear down LocalStorage")))
  nil)


(extend-type neyho.eywa.LocalStorage
  neyho.eywa.avatars/AvatarStoreProtocol
  (-set [_ {:keys [entity attribute record]} payload]
    (let [path (format "/avatars/%s/%s/%s" entity attribute record)
          target (io/file (str root path))]
      (io/make-parents target)
      (with-open [f (io/output-stream target)]
        (io/copy (.getBytes payload) f))))
  (-get
    [_ hex]
    (let [{:keys [entity attribute record]} (nippy/thaw (b64/decode (.getBytes hex)))
          path (format "/avatars/%s/%s/%s" entity attribute record)]
      (io/input-stream (io/file path)))))


(comment
  (def s (neyho.eywa/->LocalStorage "~/.eywa/storage"))
  (storage/available?)
  (fs/exists? "/Users/robi/.m2/repository/lilactown/helix/0.1.5/helix-0.1.5.jar:helix/core.clj")
  (fs/exists? "/Users/robi/.m2/repository/lilactown/helix/0.1.5/helix-0.1.5.jar")
  (type s))
