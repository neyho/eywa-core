(ns neyho.eywa
  "This namespace exists only to hold record definitions. When reloading
  protocol implementations in other namespace by using extend-type, record
  definitions are unchanged and therefore don't break 'system'")


(defrecord Server [host port])

(defrecord Postgres [host port user db password max-connections datasource])

(defrecord S3 [credentials bucket client])

(defrecord RocksDB [])

(defrecord LocalStorage [root])
