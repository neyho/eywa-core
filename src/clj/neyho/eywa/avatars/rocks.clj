(ns neyho.eywa.avatars.rocks
  (:require
    neyho.eywa
    neyho.eywa.avatars
    [neyho.eywa.db.rocksdb :as rocks]
    [taoensso.nippy :as nippy]
    [clojure.data.codec.base64 :as b64]))



(defonce ^:dynamic *rocks* nil)


(extend-type neyho.eywa.RocksDB
  neyho.eywa.avatars/AvatarStoreProtocol
  (-get [_ hex]
    (rocks/get
      *rocks*
      (select-keys
        (nippy/thaw (b64/decode (.getBytes hex)))
        [:entity :attribute :record])))
  (-set [_ record payload]
    (rocks/put *rocks* record payload)))



(defn init []
  (alter-var-root
    #'neyho.eywa.avatars/*store*
    (fn [_] (neyho.eywa/map->RocksDB nil)))
  (alter-var-root
    #'*rocks*
    (fn [_] (rocks/focus-column "avatars")))
  nil)
