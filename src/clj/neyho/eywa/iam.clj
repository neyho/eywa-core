(ns neyho.eywa.iam
  (:require
   [clojure.string :as str]
   clojure.set
   clojure.pprint
   clojure.data.json
   [patcho.patch :as patch]
   [clojure.core.async :as async]
   [clojure.java.io :as io]
   [clojure.tools.logging :as log]
   [buddy.sign.jwt :as jwt]
   [buddy.core.codecs]
   [buddy.core.hash]
   [buddy.hashers :as hashers]
   [buddy.core.keys :as keys]
   [neyho.eywa.data
    :refer [*EYWA*
            *ROOT*
            *PUBLIC_ROLE*
            *PUBLIC_USER*]]
   [neyho.eywa.transit :refer [<-transit]]
   [neyho.eywa.iam.uuids :as iu]
   [neyho.eywa.dataset
    :as dataset
    :refer [get-entity
            search-entity
            sync-entity
            delete-entity]]
   [neyho.eywa.lacinia :as lacinia]
   [neyho.eywa.iam :as iam]
   [neyho.eywa.iam.gen :as gen]
   [neyho.eywa.iam.access :as access]
   [neyho.eywa.iam.access.context :refer [*user*]]
   [neyho.eywa.iam.util
    :refer [import-role
            import-api
            import-app]]
   [neyho.eywa.dataset.core :as core]
   [com.walmartlabs.lacinia.resolve :as resolve]
   [com.walmartlabs.lacinia.selection :as selection])
  (:import
   [java.security KeyPairGenerator]))

(def alphabet (.toCharArray "0123456789abcdefghijklmnopqrstuvwxyz"))
(def MASK 35)

(defonce subscription (async/chan (async/sliding-buffer 10000)))
(defonce publisher
  (async/pub
   subscription
   (fn [{:keys [topic]
         :or {topic ::broadcast}}]
     topic)))

(defn publish [topic data]
  (async/put! subscription (assoc data :topic topic)))

(defn hash-uuid [uuid]
  (let [^long lo (.getLeastSignificantBits uuid)
        ^long hi (.getMostSignificantBits uuid)
        uuid-bytes (-> (java.nio.ByteBuffer/allocate 16)
                       (.putLong hi)
                       (.putLong lo)
                       (.array))
        builder (StringBuilder.)]
    (.toString
     (reduce
      (fn [b by]
        (.append b (get alphabet (bit-and by MASK))))
      builder
      uuid-bytes))))

(defn root?
  [roles]
  (contains? (set roles) (:euuid *ROOT*)))

(defonce encryption-keys (atom '()))

(defn base64-url-encode [input]
  (let [encoded (buddy.core.codecs/bytes->b64-str input)]
    (.replaceAll (str encoded) "=" "")))

(defn encode-rsa-key [rsa-key]
  (let [modulus (.getModulus rsa-key)
        exponent (.getPublicExponent rsa-key)
        n (base64-url-encode (.toByteArray modulus))
        e (base64-url-encode (.toByteArray exponent))]
    {:kty "RSA"
     :n n
     :e e
     :use "sig"
     :alg "RS256"
     :kid (base64-url-encode (buddy.core.hash/sha256 (str n e)))}))

(defn add-key-pair
  [{:keys [public private] :as key-pair}]
  (when-not (keys/public-key? public)
    (throw (ex-info "Unacceptable public key" {:key public})))
  (when-not (keys/private-key? private)
    (throw (ex-info "Unacceptable private key" {:key private})))
  (swap! encryption-keys (fn [current]
                           (let [[active deactivate] (split-at 3 (conj current key-pair))]
                             (when (not-empty deactivate)
                               (iam/publish
                                :keypair/removed
                                {:key-pairs deactivate}))
                             active)))
  (iam/publish
   :keypair/added
   {:key-pair key-pair})
  nil)

(defn get-encryption-key
  ([kid key-type]
   (some
    (fn [{target key-type id :kid}]
      (when (= kid id)
        target))
    @encryption-keys)))

(defn generate-key-pair
  []
  (let [generator (KeyPairGenerator/getInstance "RSA")
        key-pair (.generateKeyPair generator)
        public (.getPublic key-pair)
        private (.getPrivate key-pair)]
    {:kid (:kid (encode-rsa-key private))
     :private private
     :public public}))

(defn rotate-keypair
  []
  (add-key-pair (generate-key-pair)))

(defn init-default-encryption
  []
  (add-key-pair (generate-key-pair)))

(defn sign-data
  "Function encrypts data that should be in map form and returns encrypted
  string."
  ([data] (sign-data data {:alg :rs256}))
  ([data settings]
   (let [[{private-key :private kid :kid}] @encryption-keys]
     (jwt/sign data private-key (assoc settings :header {:kid kid
                                                         :type "JWT"})))))

(comment
  (def t (sign-data {:a 100}))
  (unsign-data t)
  (rotate-keypair)
  (jwt/decode-header t)
  (let [{public1 :public
         private1 :private} (generate-key-pair)
        {public2 :public
         private2 :private} (generate-key-pair)]
    (def public1 public1) (def public2 public2)
    (def private1 private1) (def private2 private2))
  (def data {:iss "majka du"
             :sub "robi"
             :exp "nikad"})
  (=
   (jwt/sign data private1 {:alg :rs256})
   (jwt/sign data private2 {:alg :rs256}))
  (=
   (jwt/sign (assoc data :sub "kittt") private1 {:alg :rs256})
   (jwt/sign data private1 {:alg :rs256})))

(defn unsign-data
  "Function takes encrypted string and returns decrypted data."
  [data]
  (when-let [{:keys [kid]} (jwt/decode-header data)]
    (let [public (get-encryption-key kid :public)]
      (jwt/unsign data public {:alg :rs256}))))

(defn jwt-decode
  [token]
  (let [[header payload] (str/split token #"\.")]
    {:header (clojure.data.json/read-str (buddy.core.codecs/b64->str header))
     :payload (clojure.data.json/read-str (buddy.core.codecs/b64->str payload))}))

(comment
  (jwt-decode "eyJhbGciOiJSUzI1NiIsImtpZCI6IlBqWXJJQmlVYzloVkN4dzFDVnZMSUkzMG5MV0x4WFoxd0s5TEEvc3NBejQiLCJ0eXBlIjoiSldUIn0.eyJhdWQiOiJodHRwczovL3d3dy5leXdhb25saW5lLmNvbS9leXdhIiwicGVybWlzc2lvbnMiOltdLCJzdWIiOiJhZG1pbiIsImlzcyI6Imh0dHA6Ly9sb2NhbGhvc3Q6ODA4MCIsInJvbGVzIjpbIjYwMWVlOThkLTc5NmItNDNmMy1hYzFmLTg4MTg1MTQwN2YzNCJdLCJleHAiOjE3NDc5MTUyMjIsInNjb3BlIjoicGVybWlzc2lvbnMgcHJvZmlsZSByb2xlczp1dWlkIG9wZW5pZCIsImNsaWVudF9pZCI6Ik1VTUFEUEFEQUtRSFNERkRHRkFFSlpKWFVTRkpHRk9PWVRXVkFVREVGVlBVUlVPUCIsImlhdCI6MTc0NzkxMTYyMiwic2lkIjoid29BU1ZHRk51TnFpb01UbkF3SkhQS0NERXJ6UnZFIiwic2Vzc2lvbiI6IndvQVNWR0ZOdU5xaW9NVG5Bd0pIUEtDREVyelJ2RSJ9.DafgS9lWsmfokxLG9Z4tlyXNvnrPcpcMVeZdV4vD8hlOB65o_727R8GlaguV24fPztw5mRv82fBGMejhZqg1S9NGroJIIuT8i63U3dFFYHABa2BRy3FLIrkQopBnBjlo-pNtDiOQ_DUAe4cmqvudZoUhopX5T11N39h2wmhU1x9eZvHPXsbxKZzjLEfRJC3Uen-wqPMHyM88yqModm-qO5UuOQaMfN2CZ8JuJSaDTf0RVNIEzLeAxkIBEGVRAlryDU36GjpVugelaU5wvL6RBzAZJ96SDRWZFmGJqHT_UG0TlR-ukpSbv4RKLlBZHSfPz32_6AkZXMFgSaNzE8m5Ig"))

(defn get-password [username]
  (:password
   (get-entity
    iu/app
    {:name username}
    {:password nil})))

(comment
  (delete-user (get-user-details "oauth_test")))

(defn get-user-details [username]
  (some->
   (get-entity
    iu/user
    {:name username}
    {:_eid nil
     :euuid nil
     :name nil
     :password nil
     :active nil
     :avatar nil
     :settings nil
     :person_info [{:selections
                    {:name nil
                     :given_name nil
                     :middle_name nil
                     :nickname nil
                     :prefered_username nil
                     :profile nil
                     :picture nil
                     :website nil
                     :email nil
                     :email_verified nil
                     :gender nil
                     :birth_date nil
                     :zoneinfo nil
                     :phone_number nil
                     :phone_number_verified nil
                     :address nil}}]
     :groups [{:selections {:euuid nil}}]
     :roles [{:selections {:euuid nil}}]})
   (update :roles #(set (map :euuid %)))
   (update :groups #(set (map :euuid %)))))

(defn validate-password
  [user-password password-hash]
  (hashers/check user-password password-hash))

(defn jwt-token? [token]
  (= 2 (count (re-seq #"\." token))))

(defn get-client
  [id]
  (get-entity
   iu/app
   {:id id}
   {:euuid nil
    :id nil
    :name nil
    :type nil
    :active nil
    :secret nil
    :settings nil}))

(defn get-clients
  [ids]
  (search-entity
   iu/app
   {:id {:_in ids}}
   {:euuid nil
    :id nil
    :name nil
    :type nil
    :active nil
    :secret nil
    :settings nil}))

(defn add-client [{:keys [id name secret settings type]
                   :or {id (gen/client-id)
                        type :public}}]
  (let [secret (or secret
                   (when (#{:confidential "confidential"} type)
                     (gen/client-secret)))]
    (sync-entity
     iu/app
     {:id id
      :name name
      :type type
      :settings settings
      :secret secret
      :active true})))

(defn remove-client [{:keys [euuid]}]
  (delete-entity iu/app {:euuid euuid}))

(defn set-user
  [user]
  (sync-entity iu/user user))

(defn delete-user
  [user]
  (delete-entity iu/user (select-keys user [:euuid])))

(defn list-clients
  []
  (search-entity
   iu/app nil
   {:euuid nil
    :name nil
    :id nil
    :secret nil
    :type nil
    :settings nil}))

(defn wrap-protect
  [protection resolver]
  (if (not-empty protection)
    (fn wrapped-protection
      [ctx args value]
      ; (log/infof "RESOLVING: %s" resolver)
      ; (def protection protection)
      ; (def resolver resolver)
      ; (def value value)
      ; (def ctx ctx)
      ; (def args args)
      (let [{:keys [scopes roles]}
            (reduce
             (fn [result definition]
               (let [{:keys [scopes roles]} (selection/arguments definition)]
                 (->
                  result
                  (update :scopes (fnil clojure.set/union #{}) (set scopes))
                  (update :roles
                          (fnil clojure.set/union #{})
                          (map #(java.util.UUID/fromString %) roles)))))
             nil
             protection)]
        ; (def user *user*)
        ; (def roles neyho.eywa.iam.access.context/*roles*)
        ; (def scopes scopes)
        ; (def roles roles)
        ; (def user-scopes neyho.eywa.iam.access.context/*scopes*)
        (if (and
             (or (empty? scopes)
                 (some access/scope-allowed? scopes))
             (or (empty? roles)
                 (access/roles-allowed? roles)))
          (resolver ctx args value)
          (resolve/resolve-as
           nil
           {:message "Access denied!"
            :code :unauthorized}))))
    resolver))

(comment
  (binding [*user* user
            neyho.eywa.iam.access.context/*roles* roles]
    (access/scope-allowed? scopes))
  (dataset/sync-entity
   iu/app
   (->
    (dataset/get-entity
     iu/app
     {:euuid #uuid "34c6eea5-3c95-4de1-a547-5e1b34ea16ea"}
     {:euuid nil
      :settings nil})
    (update-in
     [:settings "redirections"]
     (comp distinct (fnil conj []))
     "http://localhost:8000/eywa/callback"
     "http://localhost:8000/eywa/silent-callback"
     "http://localhost:8000/blipkit/callback"
     "http://localhost:8000/blipkit/silent-callback")
    (update-in
     [:settings "logout-redirections"]
     (comp distinct (fnil conj []))
     "http://localhost:8000/eywa/"
     "http://localhost:8000/blipkit/"))))

(defn ensure-public
  []
  (dataset/sync-entity
   iu/user
   (assoc *PUBLIC_USER* :roles [*PUBLIC_ROLE*])))

(defn current-version
  []
  (<-transit (slurp (io/resource "dataset/iam.json"))))

(defn level-iam
  []
  (let [{deployed-version :name} (dataset/latest-deployed-version #uuid "c5c85417-0aef-4c44-9e86-8090647d6378")]
    (patch/apply ::dataset deployed-version)))

(patch/current-version ::dataset (:name (<-transit (slurp (io/resource "dataset/iam.json")))))

(patch/upgrade
 ::dataset "0.80.0"
 (log/info "[IAM] Old version of IAM dataset deployed. Deploying newer version!")
 (dataset/deploy! (current-version))
 (dataset/reload)
 (log/infof "[IAM] Noticed that OAuth was not initialized!")
 (binding [*user* *EYWA*]
   (import-app "exports/app_eywa_frontend.json")
   (import-api "exports/api_eywa_graphql.json")
   (doseq [role ["exports/role_dataset_developer.json"
                 "exports/role_dataset_modeler.json"
                 "exports/role_dataset_explorer.json"
                 "exports/role_iam_admin.json"
                 "exports/role_iam_user.json"]]
     (import-role role))))

(comment
  (patch/version ::dataset))

(defn start
  []
  (log/info "Initializing IAM...")
  (try
    ;; TODO - Roles and permission shema should be initialized from database
    ;; and tracked by relations and entity changes just like in neyho.eywa.iam.access namespace
    ; (lacinia/add-shard ::graphql (slurp (io/resource "iam.graphql")))
    (ensure-public)
    (level-iam)
    (dataset/bind-service-user #'*PUBLIC_USER*)
    (lacinia/add-directive :protect wrap-protect)
    (log/info "IAM initialized")
    (catch Throwable e
      (log/error e "Couldn't load role schema"))))

(defn stop
  []
  (dosync
   (ref-set lacinia/compiled nil)
   (ref-set lacinia/state nil)))

(defn setup
  [{:keys [users groups roles services]}]
  (binding [core/*return-type* :edn
            *user* (:_eid *EYWA*)]
    (log/info  "Creating ROOT user role")
    (dataset/sync-entity iu/user-role *ROOT*)
    ;; Add public role and user
    (dataset/sync-entity
     iu/user
     (assoc *PUBLIC_USER* :roles [*PUBLIC_ROLE*]))
    (log/info "ROOT user role created")
    (doseq [user users]
      (log/infof "Adding user %s" (dissoc user :password))
      (dataset/sync-entity
       iu/user
       (assoc user :avatar nil :type :PERSON)))
    (doseq [group groups]
      (log/infof "Adding user group %s" group)
      (dataset/sync-entity
       iu/user-group
       (assoc group :avatar nil)))
    (doseq [role roles]
      (log/infof "Adding user role %s" role)
      (dataset/sync-entity
       iu/user-role
       (assoc role :avatar nil)))
    (doseq [service services
            :let [euuid (java.util.UUID/randomUUID)]]
      (log/infof "Adding service %s" service)
      (dataset/sync-entity
       iu/user
       (assoc service
         :euuid euuid
         :type :SERVICE
         :avatar nil)))))
