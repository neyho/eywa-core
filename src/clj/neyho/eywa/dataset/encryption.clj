(ns neyho.eywa.dataset.encryption
  (:require
    [clojure.string :as str]
    [next.jdbc :as jdbc]
    [neyho.eywa.env :as env]
    [neyho.eywa.db :refer [*db*]]
    [neyho.eywa.db.postgres.next
     :refer [execute-one!]]
    [clojure.data.json :as json]
    [buddy.core.crypto :as crypto]
    [buddy.core.codecs :as codecs]
    [buddy.core.nonce :as nonce])
  (:import
    [org.postgresql.util PGobject]
    [java.util Random]
    java.math.BigInteger
    [javax.crypto.spec SecretKeySpec GCMParameterSpec]
    [javax.crypto Cipher KeyGenerator SecretKey]
    [java.util Base64]))



(defn- gen-master [] (BigInteger. 128 (Random.)))



(defonce ^:dynamic *master-key* 4714597897634884447769226850258298369)
(defonce ^:dynamic *dke* nil)


(defonce dkes (atom nil))


(defn get-dke [id] (get @dkes id))


(defn create-dke-table
  []
  (let [ddl (str/join
              "\n"
              ["create table __dkes("
               "   id SERIAL PRIMARY KEY,"
               "   dek jsonb not null,"
               "   key_algorithm VARCHAR(50) not null,"
               "   created_at TIMESTAMP default now(),"
               "   expires_at timestamp,"
               "   active boolean default true"
               ");"])]
    (with-open [con (jdbc/get-connection (:datasource *db*))]
      (execute-one! con [ddl]))))


(defn drop-dke-table
  []
  (with-open [con (jdbc/get-connection (:datasource *db*))]
    (execute-one! con ["drop table if exists \"__dkes\""])))


(defn generate-key [key-size]
  (let [key-bytes (nonce/random-bytes (/ key-size 8))] ;; Generate random bytes for key
    (SecretKeySpec. key-bytes "AES")))


(def master-key->aes
  (memoize
    (fn []
      (let [bs (take 32
                     (concat (.toByteArray (java.math.BigInteger. (.toString *master-key*)))
                             (repeat 0)))]
        (SecretKeySpec. (byte-array bs) "AES")))))


(defn encrypt-dek
  [dek]
  (let [key-string (.encodeToString (Base64/getEncoder) (.getEncoded dek))
        iv (nonce/random-bytes 12)
        encrypted (crypto/encrypt
                    (.getBytes key-string "UTF-8")
                    (.getEncoded (master-key->aes))
                    iv
                    {:alg :aes256-gcm})]
    {:key (.encodeToString (Base64/getEncoder) encrypted)
     :iv (.encodeToString (Base64/getEncoder) iv)}))



(defn decrypt-dek
  [{aes-key :key iv :iv}]
  (let [aes-key (.decode (Base64/getDecoder) aes-key)
        iv (.decode (Base64/getDecoder) iv)
        decrypted (crypto/decrypt
                    aes-key
                    (.getEncoded (master-key->aes))
                    iv
                    {:alg :aes256-gcm})
        decoded (.decode (Base64/getDecoder) decrypted)]
    decoded
    #_(String. decrypted "UTF-8")
    #_(.getBytes (String. decrypted "UTF-8") "UTF-8")))


(defn create-dek
  []
  (let [dek (generate-key 256)
        encrypted (encrypt-dek dek)
        {id :__dkes/id} (with-open [con (jdbc/get-connection (:datasource *db*))]
                          (jdbc/execute-one!
                            con
                            ["insert into __dkes (dek, key_algorithm, active) values (?, ?, ?) returning id"
                             (doto (PGobject.)
                               (.setType "jsonb")
                               (.setValue (json/write-str encrypted :key-fn name)))
                             "aes256-gcm"
                             true]))]
    (swap! dkes assoc id encrypted)))


(defn initialize-dkes
  []
  (reset!
    dkes
    (reduce
      (fn [r {id :__dkes/id
              dek :__dkes/dek
              active :__dkes/active}]
        (let [db-dek (json/read-str (.getValue dek) :key-fn keyword)]
          (assoc r id (decrypt-dek db-dek))))
      nil
      (with-open [con (jdbc/get-connection (:datasource *db*))]
        (jdbc/execute!
          con
          ["select id,dek,active from __dkes"])))))


(defn encrypt-data
  [data]
  (let [iv (nonce/random-bytes 12)
        current-dek *dke*
        dek (get-dke current-dek)
        encrypted (crypto/encrypt
                    (.getBytes data "UTF-8")
                    dek iv
                    {:alg :aes256-gcm})
        payload {:data (.encodeToString (Base64/getEncoder) encrypted)
                 :dke current-dek
                 :iv (.encodeToString (Base64/getEncoder) iv)}]
    payload))


(defn decrypt-data
  [{:keys [data dke iv]}]
  (let [aes-key (get-dke dke)
        iv (.decode (Base64/getDecoder) iv)
        decrypted (crypto/decrypt
                    (.decode (Base64/getDecoder) data)
                    aes-key 
                    iv
                    {:alg :aes256-gcm})]
    (String. decrypted "UTF-8")))


(comment
  (def dek (generate-key 256))
  (def encrypted (encrypt-dek dek))
  (let [;dek (generate-key 256)
        encrypted-dek (encrypt-dek dek)
        decrypted-dek (decrypt-dek encrypted-dek)]
    (println "WHEN ENCRYPTED: " encrypted-dek)
    decrypted-dek)
  (->
    (generate-dek 256)
    encrypt-dek
    decrypt-dek)
  (create-dke-table)
  (drop-dke-table))
