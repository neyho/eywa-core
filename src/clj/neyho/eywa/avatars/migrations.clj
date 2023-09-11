(ns neyho.eywa.avatars.migrations
  (:require
    environ.core
    clojure.string
    [clojure.java.io :as io]
    [camel-snake-kebab.core :as csk]
    [clojure.data.codec.base64 :as b64]
    neyho.eywa
    [neyho.eywa.storage :as storage]
    [neyho.eywa.dataset.core :as core]
    [neyho.eywa.dataset :as dataset])
  (:import
    java.nio.charset.StandardCharsets
    (javax.crypto Cipher KeyGenerator)
    (javax.crypto.spec SecretKeySpec)
    (java.security SecureRandom)
    (org.apache.commons.codec.binary Base64)))


(defn avatar-entities
  []
  (let [model (dataset/deployed-model)
        entities (core/get-entities model)
        avatar-entities (reduce
                          (fn [r {:keys [euuid attributes]}]
                            (let [avatar-attributes (filter
                                                      (fn [{t :type}]
                                                        (= t "avatar"))
                                                      attributes)]
                              (if (empty? avatar-attributes) r
                                (assoc r euuid avatar-attributes))))
                          nil
                          entities)]
    avatar-entities))


(defn get-all-avatars
  []
  (reduce-kv
    (fn [r entity-euuid attributes]
      (let [attribute->euuid (reduce
                               (fn [r {n :name e :euuid}]
                                 (assoc r (csk/->snake_case_keyword n) e))
                               nil
                               attributes)
            selection (zipmap (keys attribute->euuid) (repeat nil))
            search-result (dataset/search-entity entity-euuid nil selection)
            result (mapcat
                    (fn [data]
                      (let [record-euuid (:euuid data)
                            avatar-data (dissoc data :euuid)
                            avatar-keys (keys avatar-data)]
                        (map
                          (fn [k]
                            [{:entity entity-euuid
                              :attribute (attribute->euuid k)
                              :record record-euuid}
                             (get avatar-data k)])
                          avatar-keys))) 
                    search-result)]
        (concat r result)))
    nil
    (avatar-entities)))


(defn- bytes- [s]
  (.getBytes s "UTF-8"))

(defn base64 [b]
  (Base64/encodeBase64String b))

(defn debase64 [s]
  (Base64/decodeBase64 (bytes- s)))

(defn get-raw-key [seed]
  (let [keygen (KeyGenerator/getInstance "AES")
        sr (SecureRandom/getInstance "SHA1PRNG")]
    (.setSeed sr (bytes- seed))
    (.init keygen 128 sr)
    (.. keygen generateKey getEncoded)))

(defn get-cipher [mode seed]
  (let [key-spec (SecretKeySpec. (get-raw-key seed) "AES")
        cipher (Cipher/getInstance "AES")]
    (.init cipher mode key-spec)
    cipher))


(def secret "+@vataR")


(defn encrypt [text]
  (let [bytes (bytes- (str text))
        cipher (get-cipher Cipher/ENCRYPT_MODE secret)]
    (base64 (.doFinal cipher bytes))))


(defn decrypt [text]
  (let [cipher (get-cipher Cipher/DECRYPT_MODE secret)]
    (String. (.doFinal cipher (debase64 text)))))


(def generate-avatar-link 
  (fn [ident]
    (encrypt (str ident))))


(defn storage->rocks
  [all-avatars]
  (doseq [[{entity-euuid :entity
            record-euuid :record} hex] all-avatars
          :let [[id field] (try
                             (clojure.string/split (decrypt hex) #":")
                             (catch Throwable _ nil))
                avatar-path (when (and id field)
                              (str "uploads/avatars/" (if (empty? id) "default_user" (str id \: field))))
                img (when avatar-path
                      (try
                        (storage/download avatar-path)
                        (catch Throwable _ nil)))
                avatar-base64 (when img
                                (let [out (java.io.ByteArrayOutputStream.)]
                                  (io/copy img out)
                                  (String.
                                    (b64/encode (.toByteArray out))
                                    StandardCharsets/UTF_8)))
                data (assoc
                       {:euuid record-euuid}
                       (keyword field) avatar-base64)]]
    (println (format "[%s] Storing new link %s" entity-euuid [record-euuid (keyword field) (some? avatar-base64)]))
    (dataset/sync-entity entity-euuid data)
    #_(when img (rocks/put record avatar-base64))))



; (defn migrate-rocks->postgres
;   []
;   (let [db (rocks/focus-column "avatars")
;         all-avatars
;         (with-open [iterator (.newIterator (:db db) (rocks/-get-handle db "avatars"))]
;           (.seekToFirst iterator)
;           (loop [result nil]
;             (if-not (.isValid iterator) result
;               (let [result' (assoc result
;                                    ((rocks/key-decoder db) (.key iterator))
;                                    ((rocks/val-decoder db) (.value iterator)))]
;                 (.next iterator)
;                 (recur result')))))
;         postgres (neyho.eywa/map->Postgres neyho.eywa.db/*db*)]
;     (doseq [[record payload] all-avatars]
;       (avatars/-set postgres record payload))))

; (defn migrate-rocks->s3
;   []
;   (let [db (rocks/focus-column "avatars")
;         all-avatars
;         (with-open [iterator (.newIterator (:db db) (rocks/-get-handle db "avatars"))]
;           (.seekToFirst iterator)
;           (loop [result nil]
;             (if-not (.isValid iterator) result
;               (let [result' (assoc result
;                                    ((rocks/key-decoder db) (.key iterator))
;                                    ((rocks/val-decoder db) (.value iterator)))]
;                 (.next iterator)
;                 (recur result')))))
;         s3 (neyho.eywa/map->S3 {:bucket (environ.core/env :aws-s3-bucket-name)})]
;     (doseq [[record payload] all-avatars]
;       (avatars/-set s3 record payload))))


(comment
  (def records
    (with-open [connection (jdbc/get-connection (:datasource *db*))]
      (postgres/execute!
        connection
        ["select * from __avatars"])))
  (io/copy (:data (nth records 10)) (io/file (str "/Users/robi/dev/EYWA/avatars/dtest.png")))
  (doseq [{:keys [data]} records]
    (io/copy data (io/file (str "/Users/robi/dev/EYWA/avatars/" (gensym "avatar_") ".png"))))
  (defn postgres->local
    []
    ))





(comment
  ; (def all-avatars (get-all-avatars))
  ; (spit "ivs-avatars.edn" (vec all-avatars))
  ; (def all-avatars nil)
  (def all-avatars
    (clojure.edn/read-string
      (slurp "ivs-avatars.edn")))
  ; (def all-avatars (get-all-avatars))
  (str (rocks/get (-> all-avatars ffirst)))
  (def record (-> all-avatars ffirst))
  (first b)
  (second b)
  (= (first b) (second b))
  (def hex (-> all-avatars first second))
  (let [[id field] (try
                      (clojure.string/split (avatars/decrypt hex) #":")
                      (catch Throwable _ nil))
         avatar-path (when (and id field)
                       (str "uploads/avatars/" (if (empty? id) "default_user" (str id \: field))))
         img (when avatar-path
               (try
                 (storage/download avatar-path)
                 (catch Throwable _ nil)))
         avatar-base64 (when img
                                (let [out (java.io.ByteArrayOutputStream.)]
                                  (io/copy img out)
                                  (String.
                                    (b64/encode (.toByteArray out))
                                    StandardCharsets/UTF_8)))]
    (re-find #"(?<=data:image/.{2,4};base64,)?.*" avatar-base64)))
