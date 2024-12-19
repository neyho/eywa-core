(ns neyho.eywa.iam
  (:require
   [clojure.string :as str]
   clojure.set
   clojure.pprint
   clojure.data.json
   [clojure.tools.logging :as log]
   [buddy.sign.jwt :as jwt]
   [buddy.core.codecs]
   [buddy.core.hash]
   [buddy.hashers :as hashers]
   [buddy.core.keys :as keys]
   [neyho.eywa.data
    :refer [*EYWA*
            *ROOT*]]
   [neyho.eywa.iam.uuids :as iu]
   [neyho.eywa.dataset
    :as dataset
    :refer [get-entity
            search-entity
            sync-entity
            delete-entity]]
   [neyho.eywa.lacinia :as lacinia]
   [neyho.eywa.env :as env]
   [neyho.eywa.iam.gen :as gen]
   [neyho.eywa.iam.access :as access]
   [neyho.eywa.iam.access.context :refer [*user*]]
   [neyho.eywa.dataset.core :as core]
   [com.walmartlabs.lacinia.resolve :as resolve]
   [com.walmartlabs.lacinia.selection :as selection])
  (:import
   [java.security KeyPairGenerator]))

(def alphabet (.toCharArray "0123456789abcdefghijklmnopqrstuvwxyz"))
(def MASK 35)

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
  (swap! encryption-keys (fn [current] (take 3 (conj current (assoc key-pair :kid (:kid (encode-rsa-key private))))))))

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
    {:private private
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
  (if-let [{:keys [kid]} (jwt/decode-header data)]
    (let [public (get-encryption-key kid :public)]
      (jwt/unsign data public {:alg :rs256}))))

(defn jwt-decode
  [token]
  (let [[header payload] (str/split token #"\.")]
    {:header (clojure.data.json/read-str (buddy.core.codecs/b64->str header))
     :payload (clojure.data.json/read-str (buddy.core.codecs/b64->str payload))}))

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
      (comment
        (type value))
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
        (if (and
             (or (empty? scopes)
                 (access/scope-allowed? scopes))
             (or (empty? roles)
                 (access/roles-allowed? roles)))
          (resolver ctx args value)
          (resolve/resolve-as
           nil
           {:message "Access denied!"
            :code :unauthorized}))))
    resolver))

(defn start
  []
  (log/info "Initializing IAM...")
  (try
    ;; TODO - Roles and permission shema should be initialized from database
    ;; and tracked by relations and entity changes just like in neyho.eywa.iam.access namespace
    ; (lacinia/add-shard ::graphql (slurp (io/resource "iam.graphql")))
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

;; DEPRECATED - in favor of neyho.eywa.iam.util/import*
; (defn init-eywa-frontend-client
;   []
;   (letfn [(ensure [coll should-contain]
;             (if (not-any? #(= should-contain %) coll)
;               (conj coll should-contain)
;               coll))]
;     (let [domain env/iam-root-url
;           client (when domain
;                    (->
;                      {:euuid #uuid "34c6eea5-3c95-4de1-a547-5e1b34ea16ea",
;                       :id "MUMADPADAKQHSDFDGFAEJZJXUSFJGFOOYTWVAUDEFVPURUOP",
;                       :name "EYWA",
;                       :type "public",
;                       :active true,
;                       :secret nil,
;                       :settings
;                       {"logo-url" "https://www.eywaonline.com/eywa/logo/eywa.svg",
;                        "login-page" "",
;                        "redirections" [] ,
;                        "token-expiry" {"id" 600, "access" 3600, "refresh" 129600},
;                        "allowed-grants" ["authorization_code"
;                                          "urn:ietf:params:oauth:grant-type:device_code"
;                                          "refresh_token"],
;                        "logout-redirections" []}}
;                      ; (update-in [:settings "redirections"] ensure (str domain "/eywa/callback"))
;                      ; (update-in [:settings "logout-redirections"] ensure (str domain "/eywa/"))
;                      ; (update-in [:settings "logout-redirections"] ensure (str domain "/eywa"))
;                      (update-in [:settings "redirections"] ensure "https://my.eywaonline.com/eywa/callback")
;                      (update-in [:settings "redirections"] ensure "https://my.eywaonline.com/eywa/silent-callback")
;                      (update-in [:settings "logout-redirections"] ensure "https://my.eywaonline.com/")))]
;       (when client (add-client client)))))
