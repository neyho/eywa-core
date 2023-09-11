(ns neyho.eywa.db.rocksdb.core
  (:refer-clojure :exclude [get])
  (:import
    [org.rocksdb RocksDB]
    [java.io Closeable]))


(defprotocol IRocksDB
  (^RocksDB db  [_])
  (batch [_] [_ options])
  (iterator [_] [_ start end reverse?])
  (get [_ k])
  (put [_ k v])
  (del [_ k])
  (snapshot [_] "Creates snapshot of current DB")
  (set-column-family
    [this name]
    [this name settings]
    "Will returns same database but focused on column family instead of \"defaul\"
    column family")
  (remove-column-family
    [this name]
    "Will remove column family and all of its values")
  (column-family [_] "Returns current column family"))


(defrecord DB
  [^RocksDB db settings]
  Closeable
  (close [{:keys [db]}] (.close db)))
