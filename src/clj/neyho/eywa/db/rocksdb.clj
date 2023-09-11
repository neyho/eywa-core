(ns neyho.eywa.db.rocksdb
  (:refer-clojure :exclude [get sync])
  (:require
    [babashka.fs :as fs]
    [clojure.java.io :as io]
    [taoensso.nippy :as nippy]
    [neyho.eywa.db.rocksdb.core :as core
     :refer [->DB]])
  (:import
    [java.nio.file Files]
    [java.nio.file.attribute FileAttribute]
    [org.rocksdb
     Options
     ColumnFamilyDescriptor
     ConfigOptions
     OptionsUtil
     DBOptions
     CompressionType
     RocksDB]))



(defonce ^:dynamic *db* nil)


(defn -get-handle
  ([db] (-get-handle db "default"))
  ([db column-name]
   (get-in db [:settings :handles column-name])))


(defn key-decoder
  ([db]
   (get-in db [:settings :decoders :key])))


(defn set-key-decoder
  ([db decoder]
   (assoc-in db [:settings :decoders :key] decoder)))


(defn key-encoder
  ([db]
   (get-in db [:settings :encoders :key])))


(defn set-key-encoder
  ([db encoder]
   (assoc-in db [:settings :encoders :key] encoder)))


(defn val-encoder
  ([db] (get-in db [:settings :encoders :val])))


(defn set-val-encoder
  ([db encoder]
   (assoc-in db [:settings :encoders :val] encoder)))


(defn val-decoder
  ([db] (get-in db [:settings :decoders :val])))


(defn set-val-decoder
  ([db decoder]
   (assoc-in db [:settings :decoders :val] decoder)))


(extend-type neyho.eywa.db.rocksdb.core.DB
  neyho.eywa.db.rocksdb.core/IRocksDB
  (core/db [{:keys [db]}]
    db)
  (core/get [{:keys [db]
              {column :column-family} :settings
              :as this} k]
    (let [handle (when column (-get-handle this column))]
      (some->
        (if handle (.get db handle ((key-encoder this) k)) (.get db ((key-encoder this) k)))
        ((val-decoder this)))))
  (core/put [{:keys [db]
              {column :column-family} :settings
              :as this} k v]
    (let [k ((key-encoder this) k) 
          v ((val-encoder this) v)
          handle (-get-handle this column)]
      (if handle
        (.put db handle k v)
        (.put db k v))))
  (core/del [{:keys [db]
              {column :column-family} :settings
              :as this} k]
    (let [k ((key-encoder this) k)
          handle (-get-handle this column)]
      (if handle
        (.delete db handle k)
        (.delete db k))))
  (core/set-column-family
    [{:keys [db] :as this} column-name]
    (if-some [handle (-get-handle this column-name)]
      ;; if handle already exists
      (->
        this 
        (assoc-in [:settings :column-family] column-name)
        (assoc-in [:settings :handles column-name] handle))
      ;; if column family doesn't exist jet, try to create one
      ;; and than associate in handles
      (let [handle (.createColumnFamily db (ColumnFamilyDescriptor. (.getBytes column-name)))
            db' (->
                  this
                  (assoc-in [:settings :column-family] column-name)
                  (assoc-in [:settings :handles column-name] handle))]
        db')))
  (core/remove-column-family
    [{:keys [db] :as this} column-name]
    (let [handle (-get-handle this column-name)]
      (.dropColumnFamily db handle)
      (->
        this
        (assoc-in [:settings :column-family] "default")
        (update-in [:settings :handles] dissoc column-name))))
  (column-family
    [{{column-family :column-family} :settings
      db :db}]
    (or column-family (String. (.getName (.getDescriptor (.getDefaultColumnFamily db)))))))


(defn transparent-freeze
  [data]
  (nippy/freeze data {:incl-metadata? false}))


(defn list-column-families
  ([path] (list-column-families (Options.) path))
  ([options path]
   (mapv
     #(String. %)
     (RocksDB/listColumnFamilies options (str (fs/expand-home path))))))


(defn load-latest-options
  [path]
  (let [db-options (DBOptions.)
        config-options (ConfigOptions.)
        descriptors (java.util.ArrayList.)]
    (OptionsUtil/loadLatestOptions
      config-options
      (str (fs/expand-home path))
      db-options
      descriptors)
    {:db db-options
     :config config-options
     :descriptors descriptors}))


(defn create-db
  "Creates a closeable database object, which takes a directory and zero or more settings
  The key and val encoder/decoders are functions for transforming to and from byte-arrays."
  ([directory]
   (create-db
     directory
     {:compress? true
      :cache-size 32
      :block-size (* 16 1024)
      :write-buffer-size (* 32 1024 1024)
      :create-missing? true
      :create-missing-families? true
      :error-if-exists? false}))
  ([directory {:keys [create-missing? error-if-exists?
                      create-missing-families?
                      write-buffer-size block-size
                      compress?
                      key-encoder
                      key-decoder
                      val-encoder
                      val-decoder]
               :or {key-encoder transparent-freeze
                    val-encoder transparent-freeze
                    key-decoder nippy/thaw
                    val-decoder nippy/thaw}
               :as settings}]
   (when create-missing?
     (Files/createDirectories
       (.toPath (io/file directory))
       (into-array FileAttribute [])))
   (let [path (str (fs/expand-home directory))]
     (if-let [{db-options :db
               descriptors :descriptors}
              (try
                (load-latest-options path)
                (catch Throwable _ nil))]
       (let [handles (java.util.ArrayList.)
             db (RocksDB/open
                  db-options
                  path
                  descriptors
                  ;; ColumnFamilyHandle
                  handles)]
         (->DB
           db
           {:handles (reduce
                       (fn [r h]
                         (let [descriptor (.getDescriptor h)]
                           (assoc r (String. (.getName descriptor)) h)))
                       nil
                       handles)
            :encoders {:key key-encoder
                       :val val-encoder}
            :decoders {:key key-decoder
                       :val val-decoder}}))
       (let [options (if (instance? Options settings) settings
                       (cond-> (Options.) 
                         create-missing? (.setCreateIfMissing true)
                         create-missing-families? (.setCreateMissingColumnFamilies true)
                         error-if-exists? (.setErrorIfExists false)
                         (some? compress?) (.setCompressionType (if compress?
                                                                  CompressionType/SNAPPY_COMPRESSION
                                                                  CompressionType/NO_COMPRESSION))
                         block-size (.setArenaBlockSize block-size)
                         write-buffer-size (.setDbWriteBufferSize write-buffer-size)))]
         (->DB
           (RocksDB/open options path)
           {:encoders {:key key-encoder
                       :val val-encoder}
            :decoders {:key key-decoder
                       :val val-decoder}}))))))


(defn put
  "Puts one or more key/value pairs into the given db"
  ([key val] (put *db* key val))
  ([db key val]
   (assert (some? db) "RocksDB not available")
   (core/put db key val)))


(defn get
  "Returns the value of `key` for the given database or snapshot. If the key doesn't exist, returns
   `default-value` or nil."
  ([key] (get *db* key))
  ([db key]
   (assert (some? db) "RocksDB not available")
   (core/get db key)))


(defn delete
  "Deletes one or more keys in the given `db`."
  ([key] (delete *db* key))
  ([db key]
   (assert (some? db) "RocksDB not available")
   (core/del db key)))


(defn close
  ([] (close *db*))
  ([db]
   (when db (.close db))))


(defn size
  ([] (size *db*))
  ([{db :db}]
   (format
     "%.4f Mb"
     (/
      (java.lang.Long/parseLong (.getProperty db "rocksdb.estimate-live-data-size"))
      10485760.0))))


(defn open [path]
  (create-db (str (fs/expand-home path))))


(defn focus-column
  ([name]
   (let [db' (focus-column *db* name)]
     (alter-var-root #'*db* (fn [_] db'))
     db'))
  ([db name]
   (core/set-column-family db name)))


(defn delete-column
  ([name]
   (let [db' (delete-column *db* name)]
     (alter-var-root #'*db* (fn [_] db'))
     db'))
  ([db name]
   (core/remove-column-family db name)))


(defn init
  []
  (alter-var-root
    #'neyho.eywa.db.rocksdb/*db*
    (fn [_] (open "~/.eywa/data/rocks")))
  nil)


(comment
  (-get-handle *db* "keycloak")
  (delete-column "keycloak")
  (def db (focus-column "avatars"))
  (def db (focus-column "tokens"))
  (def db (focus-column "default"))
  (def iterator (.newIterator (:db db) (-get-handle db "avatars")))
  (def iterator (.newIterator (:db db) (-get-handle db "tokens")))
  (def iterator (.newIterator (:db db)))
  (time
    (def avatars
      (with-open [iterator (.newIterator (:db db) (-get-handle db "avatars"))]
        (.seekToFirst iterator)
        (loop [result nil]
          (if-not (.isValid iterator) result
            (let [result' (assoc result
                                 ((key-decoder db) (.key iterator))
                                 ((val-decoder db) (.value iterator)))]
              (.next iterator)
              (recur result')))))))
  (.seekToFirst iterator)
  (.seekToLast iterator)
  (.isValid iterator)
  (.close iterator)

  (.key iterator)
  (.value iterator)

  (.next iterator)
  (.close iterator))


;;;;

;;(defn- closeable-seq
;;  "Creates a seq which can be closed, given a latch which can be closed
;;   and dereferenced to check whether it's already been closed."
;;  [s close-fn]
;;  (reify
;;    Closeable
;;    (close [this] (close-fn))

;;    clojure.lang.Sequential
;;    clojure.lang.ISeq
;;    clojure.lang.Seqable
;;    clojure.lang.IPersistentCollection
;;    (equiv [this x]
;;      (loop [a this, b x]
;;        (if (or (empty? a) (empty? b))
;;          (and (empty? a) (empty? b))
;;          (if (= (first x) (first b))
;;            (recur (rest a) (rest b))
;;            false))))
;;    (empty [_]
;;      [])
;;    (count [_]
;;      (count s))
;;    (cons [_ a]
;;      (cons a s))
;;    (next [_]
;;      (next s))
;;    (more [this]
;;      (let [rst (next this)]
;;        (if (empty? rst)
;;          '()
;;          rst)))
;;    (first [_]
;;      (first s))
;;    (seq [_]
;;      (seq s))))

;;(deftype close-type [iterator]
;;  Object
;;  (finalize [_] (.close iterator))
;;  clojure.lang.IFn
;;  (invoke [_] (.close iterator)))

;;(defn- iterator-seq- [^RocksIterator iterator start end reverse? key-decoder key-encoder val-decoder]
;;  (if start
;;    (if reverse?
;;      (.seekForPrev ^RocksIterator iterator (bs/to-byte-array (key-encoder start)))
;;      (.seek ^RocksIterator iterator (bs/to-byte-array (key-encoder start))))
;;    (if reverse?
;;      (.seekToLast ^RocksIterator iterator)
;;      (.seekToFirst ^RocksIterator iterator)))

;;  (let [iter-step-fn (if reverse? #(doto % .prev) #(doto % .next))
;;        iter (fn iter [it]
;;               (if-not (.isValid it) '()
;;                       (lazy-seq (cons [(.key it) (.value it)]
;;                                       (iter (iter-step-fn it))))))
;;        s (iter iterator)
;;        end-test-fn (if reverse? neg? pos?)
;;        s (if end
;;            (let [end (bs/to-byte-array (key-encoder end))]
;;              (take-while
;;               #(not (end-test-fn (bs/compare-bytes (first %) end)))
;;               s))
;;            s)]
;;    (closeable-seq
;;     (map #(vector
;;            (key-decoder (first %))
;;            (val-decoder (second %)))
;;          s)
;;     (->close-type iterator))))


;;;;;


;;(defrecord Snapshot
;;  [db
;;   key-decoder
;;   key-encoder
;;   val-decoder
;;   ^ReadOptions read-options]
;;  IRocksDB
;;  (snapshot- [this] this)
;;  (db- [_] (db- db))
;;  (get- [_ k]
;;    (val-decoder (.get (db- db) read-options (bs/to-byte-array (key-encoder k)))))
;;  (iterator- [_ start end reverse?]
;;    (iterator-seq-
;;      (.newIterator (db- db) read-options)
;;      start
;;      end
;;      reverse?
;;      key-decoder
;;      key-encoder
;;      val-decoder))
;;  Closeable
;;  (close [_]
;;    (-> read-options .snapshot .close))
;;  (finalize [this] (.close this)))

;;(defrecord Batch
;;  [^RocksDB db
;;   ^WriteBatch batch
;;   key-encoder
;;   val-encoder
;;   ^WriteOptions options]
;;  IRocksDB
;;  (db- [_] db)
;;  (batch- [this _] this)
;;  (put- [_ k v _]
;;    (.put batch
;;      (bs/to-byte-array (key-encoder k))
;;      (bs/to-byte-array (val-encoder v))))
;;  (del- [_ k _]
;;    (.delete batch (bs/to-byte-array (key-encoder k))))
;;  Closeable
;;  (close [_]
;;    (.write db (or options (WriteOptions.)) batch)
;;    (.close batch)))



;;(def ^:private option-setters
;;  {:create-if-missing? #(.setCreateIfMissing ^Options %1 %2)
;;   :error-if-exists?   #(.setErrorIfExists ^Options %1 %2)
;;   :write-buffer-size  #(.setDbWriteBufferSize ^Options %1 %2)
;;   :block-size         #(.setArenablockSize ^Options %1 %2)
;;   :max-open-files     #(.setMaxOpenFiles ^Options %1 %2)
;;   :cache-size         #(.setBlockCacheSize ^Options %1 %2)
;;   :comparator         #(.setComparator ^Options %1 %2)
;;   :paranoid-checks?   #(.setParanoidChecks ^Options %1 %2)
;;   :compress?          #(.setCompressionType ^Options %1 (if % CompressionType/SNAPPY_COMPRESSION CompressionType/NO_COMPRESSION))
;;   :logger             #(.setLogger ^Options %1 %2)})

;;(defn create-db
;;  "Creates a closeable database object, which takes a directory and zero or more options.

;;   The key and val encoder/decoders are functions for transforming to and from byte-arrays."
;;  [directory
;;   {:keys [key-decoder
;;           key-encoder
;;           val-decoder
;;           val-encoder
;;           create-if-missing?
;;           error-if-exists?
;;           write-buffer-size
;;           block-size
;;           max-open-files
;;           cache-size
;;           comparator
;;           compress?
;;           paranoid-checks?
;;           block-restart-interval
;;           logger]
;;    :or {key-decoder identity
;;         key-encoder identity
;;         val-decoder identity
;;         val-encoder identity
;;         compress? true
;;         cache-size 32
;;         block-size (* 16 1024)
;;         write-buffer-size (* 32 1024 1024)
;;         create-if-missing? true
;;         error-if-exists? false}
;;    :as options}]
;;  (->DB
;;    (RocksDB/open 
;;     (let [opts (Options.)]
;;        (doseq [[k v] options]
;;          (when (contains? option-setters k)
;;            ((option-setters k) opts v)))
;;        opts
;;      directory))
;;    key-decoder
;;    key-encoder
;;    val-decoder
;;    val-encoder))

;;(defn destroy-db
;;  "Destroys the database at the specified `directory`."
;;  [directory]
;;  (RocksDB/destroyDB
;;    directory
;;    (Options.)))

;;;;;



;;(defn snapshot
;;  "Returns a snapshot of the database that can be used with `get` and `iterator`. This implements
;;   java.io.Closeable, and can leak space in the database if not closed."
;;  [db]
;;  (snapshot- db))

;;(defn iterator
;;  "Returns a closeable sequence of map entries (accessed with `key` and `val`) that is the inclusive
;;   range from `start `to `end`.  If exhausted, the sequence is automatically closed."
;;  ([db]
;;   (iterator db nil nil))
;;  ([db start]
;;   (iterator db start nil))
;;  ([db start end]
;;   (iterator- db start end false)))

;;(defn reverse-iterator
;;  "Returns a closeable sequence of map entries (accessed with `key` and `val`) that is the inclusive
;;   range from `start `to `end` in reverse order.  If exhausted, the sequence is automatically closed."
;;  ([db]
;;   (reverse-iterator db nil nil))
;;  ([db start]
;;   (reverse-iterator db start nil))
;;  ([db start end]
;;   (iterator- db start end true)))

;;(defn put
;;  "Puts one or more key/value pairs into the given `db`."
;;  ([db])
     
;;  ([db key val]
;;   (put- db key val nil))
;;  ([db key val & key-vals]
;;   (with-open [^Batch batch (batch- db nil)]
;;     (put- batch key val nil)
;;     (doseq [[k v] (partition 2 key-vals)]
;;       (put- batch k v nil)))))



;;(defn sync
;;  "Forces the database to fsync."
;;  [db]
;;  (with-open [^Batch batch (batch- db (doto (WriteOptions.) (.setSync true)))]))
    

;;(defn stats
;;  "Returns statistics for the database."
;;  [db]
;;  (.getProperty (db- db) "rocksdb.stats"))

;;(defn bounds
;;  "Returns a tuple of the lower and upper keys in the database or snapshot."
;;  [db]
;;  (let [key-decoder (:key-decoder db)]
;;    (with-open [^RocksIterator iterator (condp instance? db
;;                                          DB (.newIterator (db- db))
;;                                          Snapshot (.newIterator (db- db) (:read-options db)))]
;;      (doto iterator .seekToFirst)
;;      (when (.isValid iterator)
;;        [(-> (doto iterator .seekToFirst) .key key-decoder)
;;         (-> (doto iterator .seekToLast) .key key-decoder)]))))

;;(defn compact
;;  "Forces compaction of database over the given range. If `start` or `end` are nil, they default to
;;   the full range of the database."
;;  ([db]
;;   (compact db nil nil))
;;  ([db start]
;;   (compact db start nil))
;;  ([db start end]
;;   (let [encoder (:key-encoder db)
;;         [start' end'] (bounds db)
;;         start (or start start')
;;         end (or end end')]
;;     (when (and start end)
;;       (.compactRange (db- db)
;;         (bs/to-byte-array (encoder start))
;;         (bs/to-byte-array (encoder end)))))))

;;(defn batch
;;  "Batch a collection of put and/or delete operations into the supplied `db`.
;;   Takes a map of the form `{:put [key1 value1 key2 value2] :delete [key3 key4]}`.
;;   If `:put` key is provided, it must contain an even-length sequence of alternating keys and values."
;;  ([db])
;;  ([db {puts :put deletes :delete}]
;;   (assert (even? (count puts)) ":put option requires even number of keys and values.")
;;   (with-open [^Batch batch (batch- db nil)]
;;     (doseq [[k v] (partition 2 puts)]
;;       (put- batch k v nil))
;;     (doseq [k deletes]
;;       (del- batch k nil)))))
