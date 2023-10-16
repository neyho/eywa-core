(ns neyho.eywa.storage)


(defonce ^:dynamic *storage* nil)


(defprotocol StorageProtocol
  (-available? [this])
  (-search [this] [this pattern])
  (-upload [this file key] [this file key version])
  (-download [this key] [this key version] [this key version path])
  (-delete [this key] [this key id]))


(defn available? [] (-available? *storage*))
(defn search [] (-search *storage*))
(defn upload [file key] (-upload *storage* file key))


(extend-type nil
  StorageProtocol
  (-available? [_])
  (-search
    ([_] (-search nil nil))
    ([_ _]))
  (-upload
    ([_ _ _])
    ([_ _ _ _]))
  (-download
    ([_ _])
    ([_ _ _])
    ([_ _ _ _]))
  (-delete
    ([_])
    ([_ _ _])))


(defn download
  ([key] (-download *storage* key))
  ([key version] (-download *storage* key version))
  ([key version path] (-download *storage* key version path)))


(defn delete [key] (-delete *storage* key))
