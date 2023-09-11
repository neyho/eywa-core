(ns neyho.eywa.avatars
  (:require
    neyho.eywa)
  (:refer-clojure :exclude [set get]))



(defonce ^:dynamic *store* nil)


(defprotocol AvatarStoreProtocol
  (-set [this record payload] "For given record store avatar")
  (-get
    [this hex]
    "For given encoded avatar value return avatar base64 payload. Returns byte array
    or input-stream")
  (-delete
    [this record]
    "Delete avatar from storage for given record."))


(defn get [hex] (when *store* (-get *store* hex)))
(defn set [record payload] (when *store* (-set *store* record payload)))
