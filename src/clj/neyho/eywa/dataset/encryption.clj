(ns neyho.eywa.dataset.encryption
  (:require
    [clojure.tools.logging :as log]
    [clojure.string :as str]
    [clojure.math.combinatorics :as combo]
    [environ.core :refer [env]]
    [next.jdbc :as jdbc]
    [neyho.eywa.db :refer [*db*]]
    [neyho.eywa.db.postgres.next
     :refer [execute-one!]]
    [neyho.eywa.iam.access :as access]
    [clojure.data.json :as json]
    [buddy.core.crypto :as crypto]
    [buddy.core.nonce :as nonce]
    [neyho.eywa.iam.shamir
     :refer [create-shares
             reconstruct-secret]]
    [com.walmartlabs.lacinia.resolve :as r])
  (:import
    [org.postgresql.util PGobject]
    [java.util Random]
    java.math.BigInteger
    [javax.crypto.spec SecretKeySpec]
    [java.util Base64]))


(defonce ^:dynamic *master-key* nil)
(defonce ^:dynamic *dek* nil)


(defn random-master [] (.toString (BigInteger. 128 (Random.))))


(defn gen-key
  ([] (gen-key (.toString (BigInteger. 128 (Random.)))))
  ([data]
   (let [bs (take 32
                  (concat
                    (.toByteArray (java.math.BigInteger. data))
                    (repeat 0)))]
     (SecretKeySpec. (byte-array bs) "AES"))))


(defonce deks (atom nil))


(defn get-dek [data]
  (cond
    (number? data) (get @deks data)
    (instance? javax.crypto.spec.SecretKeySpec data) (.getEncoded data)
    :else data))


(defn create-dek-table
  []
  (let [ddl (str/join
              "\n"
              ["create table __deks("
               "   id SERIAL PRIMARY KEY,"
               "   dek jsonb not null,"
               "   encryption_barrier jsonb,"
               "   key_algorithm VARCHAR(50) not null,"
               "   created_at TIMESTAMP default now(),"
               "   expires_at timestamp,"
               "   active boolean default true"
               ");"])]
    (with-open [con (jdbc/get-connection (:datasource *db*))]
      (execute-one! con [ddl]))))


(defn drop-dek-table
  []
  (with-open [con (jdbc/get-connection (:datasource *db*))]
    (execute-one! con ["drop table if exists \"__deks\""])))


(defn generate-key [key-size]
  (let [key-bytes (nonce/random-bytes (/ key-size 8))] ;; Generate random bytes for key
    (SecretKeySpec. key-bytes "AES")))


(defn encrypt-dek
  [dek]
  (let [key-string (.encodeToString (Base64/getEncoder) (.getEncoded dek))
        iv (nonce/random-bytes 12)
        encrypted (crypto/encrypt
                    (.getBytes key-string "UTF-8")
                    (.getEncoded *master-key*)
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
                    (.getEncoded *master-key*)
                    iv
                    {:alg :aes256-gcm})
        decoded (.decode (Base64/getDecoder) decrypted)]
    decoded))


(def ^:private encryption-barrier "this_was_encrypted")


(defn encrypt-data
  [data]
  (if-not *dek*
    (do
      (log/error "Couldn't encrypt data since *dek* isn't specified...")
      (throw
        (ex-info
         "Couldn't encrypt data. Encryptiion is not initialized"
         {:type :encryption/not-initialized})))
    (let [iv (nonce/random-bytes 12)
          current-dek *dek*
          dek (get-dek current-dek)
          encrypted (crypto/encrypt
                      (.getBytes data "UTF-8")
                      dek iv
                      {:alg :aes256-gcm})
          payload {:data (.encodeToString (Base64/getEncoder) encrypted)
                   :dek current-dek
                   :iv (.encodeToString (Base64/getEncoder) iv)}]
      payload)))


(defn decrypt-data
  [{:keys [data dek iv]}]
  (when (and dek iv)
    (let [aes-key (get-dek dek)
          iv (.decode (Base64/getDecoder) iv)
          decrypted (crypto/decrypt
                      (.decode (Base64/getDecoder) data)
                      aes-key 
                      iv
                      {:alg :aes256-gcm})]
      (String. decrypted "UTF-8"))))


(defn add-dek-barrier
  [id]
  (with-open [con (jdbc/get-connection (:datasource *db*))]
    (jdbc/execute-one!
      con
      ["update __deks set encryption_barrier=? where id=?"
       (doto (PGobject.)
            (.setType "jsonb")
            (.setValue
              (json/write-str
                (binding [*dek* id]
                  (encrypt-data encryption-barrier))))) id])))


(defn set-dek-active
  [id]
  (with-open [con (jdbc/get-connection (:datasource *db*))]
    (jdbc/execute-one!
      con
      ["update __deks set active=false where id!=?" id])))


(defn dek->db
  [dek]
  (let [encrypted (encrypt-dek dek)
        {id :__deks/id} (with-open [con (jdbc/get-connection (:datasource *db*))]
                          (jdbc/execute-one!
                            con
                            ["insert into __deks (dek, key_algorithm, active) values (?, ?, ?) returning id"
                             (doto (PGobject.)
                               (.setType "jsonb")
                               (.setValue (json/write-str encrypted :key-fn name)))
                             "aes256-gcm"
                             true]))]
    id))


(defn create-dek
  []
  (let [dek (generate-key 256)
        id (dek->db dek)]
    (swap! deks assoc id (.getEncoded dek))
    (add-dek-barrier id)
    (set-dek-active id)
    (alter-var-root #'*dek* (fn [_] id))
    id))


(defn db-deks
  []
  (with-open [con (jdbc/get-connection (:datasource *db*))]
    (jdbc/execute!
      con
      ["select id,dek,active,encryption_barrier from __deks"])))


(defn init-deks
  []
  (reset! deks nil)
  (try
    ;; If there are some DEK in deks table
    ;; that means that some data has may have been
    ;; encrypted... So we check if master key can
    ;; can decrypt deks and encryptioin_barrier as well
    (if-let [known-deks (not-empty (db-deks))]
      (reduce
        (fn [r {id :__deks/id
                dek :__deks/dek
                encryption_barrier :__deks/encryption_barrier
                active? :__deks/active}]
          (let [db-dek (json/read-str (.getValue dek) :key-fn keyword)
                _encryption-barrier (json/read-str (.getValue encryption_barrier) :key-fn keyword)
                dek (decrypt-dek db-dek)
                _ (swap! deks assoc id dek)
                valid? (= encryption-barrier
                          (try
                            (binding [*dek* dek]
                              (decrypt-data _encryption-barrier))
                            (catch Throwable _
                              (log/errorf "[ENCRYPTION] Couldn't decrypt encryption barrier: %s" id))))]
            (if-not valid? r
              (do
                (when active?
                  (alter-var-root #'*dek* (fn [_] id)))
                (assoc r id dek)))))
        nil
        known-deks)
      ;; If there are no encrypted deks, than create new DEK
      ;; and mark it active... 
      (create-dek))
    (catch Throwable ex
      (log/errorf
        ex "[ENCRYPTION] [%s] DEK initialized! Couldn't initialize all DEKs... Master key not valid"
        (count (keys @deks)))
      (reset! deks nil)
      (throw ex))))


(defn initialized? [] (some? *master-key*))


(defn init
  ([] (init (env :eywa-encryption-master-key)))
  ([master]
   (let [bs (take 32
                  (concat
                    (.toByteArray (java.math.BigInteger. master))
                    (repeat 0)))
         master-key (SecretKeySpec. (byte-array bs) "AES")]
     (when (and (initialized?) (not= master-key *master-key*))
       (throw
         (ex-info
           "Encryption already initialized with different master key!"
           {:master master
            :master/key master-key})))
     (try
       (binding [*master-key* master-key]
         (init-deks))
       (alter-var-root #'*master-key* (fn [_] master-key))
       (catch Throwable _
         (log/errorf "[ENCRYPTION] Couldn't initialize dataset encryption")
         nil)))))


(defonce ^:private available-shares (atom nil))


(defn process-init-abuse
  [_]
  (r/with-error nil
    {:message "Master key already initialized. Your attempt will be logged and processed"}))


(defn process-not-authorized
  [_]
  (r/with-error nil
    {:message "Not authorized!"}))


;; @resolve
(defn init-with-share
  [ctx {{:keys [id value]} :share} _]
  (cond
    (not (access/superuser?)) (process-not-authorized ctx)
    ;;
    (some? *master-key*) (process-init-abuse ctx) 
    ;;
    :else
    (do
      (swap! available-shares assoc id (BigInteger. value))
      (let [shares (seq @available-shares)
            threshold (env :eywa-encryption-share-threshold 3)
            combinations (combo/combinations shares threshold)]
        (if-let [response (some
                            (fn [shares]
                              (let [{:keys [status] :as response}
                                    (try
                                      (let [secret (reconstruct-secret shares)]
                                        (when (init (str secret))
                                          {:status :INITIALIZED
                                           :shares (map first shares)
                                           :message "You have successfully unseald encryption!"}))
                                      (catch Throwable _ nil))]
                                (when-not (= status :ERROR)
                                  response)))
                            combinations)]
          response
          {:status :ERROR
           :message "Available shares couldn't decrypt master key"})))))


;; @resolve
(defn init-with-master
  [ctx {:keys [master]} _]
  (cond
    (not (access/superuser?)) (process-not-authorized ctx)
    ;;
    (some? *master-key*) (process-init-abuse ctx)
    ;;
    :else (try
            (init master)
            {:status :INITIALIZED
             :message "You have successfully initialized encryption!"}
            (catch Throwable _
              {:status :ERROR
               :message "Provided master key is not valid!"}))))


;; @resolve
(defn generate-master
  [ctx _ _]
  (cond
    ;;
    (not (access/superuser?)) (process-not-authorized ctx)
    ;;
    (or
      (some? *master-key*)
      (not-empty (db-deks)))
    (r/with-error nil
      {:message "Master is already in use. Can't generate new one!"})
    ;;
    :else
    (let [master (.toString (java.math.BigInteger. (.getEncoded (gen-key))))]
      (init master)
      master)))


;; @resolve
(defn generate-shares
  [ctx {:keys [shares threshold]} _]
  (cond
    ;;
    (not (access/superuser?)) (process-not-authorized ctx)
    ;;
    (or
      (some? *master-key*)
      (not-empty (db-deks)))
    (r/with-error nil
      {:message "Master is already in use. Can't generate new one!"})
    ;;
    :else
    (let [master (java.math.BigInteger. 128 (Random.))
          shares (create-shares master shares threshold)]
      (init (str master))
      (map (fn [[id share]] {:id id :value (str share)}) shares))))



(comment
  (dek->db dek)
  (def master *master-key*)
  (do
    (reset! available-shares nil)
    (reset! deks nil)
    (alter-var-root #'*dek* (fn [_] nil))
    (alter-var-root #'*master-key* (fn [_] nil)))
  (alter-var-root #'*master-key* (fn [_] (gen-key)))
  (alter-var-root #'*master-key* (fn [_] (gen-key (env :eywa-encryption-master-key))))
  (init)
  (init (random-master))
  (= master *master-key*)
  (add-dek-barrier 1)
  (def dek (generate-key 256))
  (def encrypted (encrypt-dek dek))
  (let [;dek (generate-key 256)
        encrypted-dek (encrypt-dek dek)
        decrypted-dek (decrypt-dek encrypted-dek)]
    decrypted-dek)
  (->
    (generate-dek 256)
    encrypt-dek
    decrypt-dek)
  (create-dek-table)
  (create-dek)
  (drop-dke-table)
  (drop-dek-table))
