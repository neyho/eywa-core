(ns neyho.eywa.iam
  (:require
    [clojure.string :as str]
    [clojure.java.io :as io]
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
    [neyho.eywa.authorization.components :as c]
    [neyho.eywa.iam.gen :as gen]
    [neyho.eywa.iam.access.context :refer [*user*]]
    [neyho.eywa.dataset.core :as core])
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
    (def private1 private1) (def private2 private2)
    )
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



;; @hook
(defn reload-schema
  [ctx args v]
  (log/info "Reloading access schema")
  (dataset/load-role-schema)
  [ctx args v])


(defn init
  []
  (log/info "Initializing Administration...")
  (try
    (dataset/load-role-schema)
    (log/info "Administration initialized")
    (lacinia/add-shard ::administration (slurp (io/resource "administration.graphql")))
    (catch Throwable e
      (log/error e "Couldn't load role schema"))))


(def permissions
  [{:euuid c/eywa
    :name "EYWA"}
   {:euuid c/iam
    :name "IAM"
    :roles [*ROOT*]
    :parent {:euuid c/eywa}}
   {:euuid c/users
    :name "Users"
    :roles [*ROOT*]
    :parent {:euuid c/iam}}
   {:euuid c/user-add
    :name "User Add"
    :roles [*ROOT*]
    :parent {:euuid c/users}}
   {:euuid c/user-modify
    :name "User Edit"
    :roles [*ROOT*]
    :parent {:euuid c/users}}
   {:euuid c/user-delete
    :name "User Delete"
    :roles [*ROOT*]
    :parent {:euuid c/users}}
   {:euuid c/groups
    :name "Groups"
    :roles [*ROOT*]
    :parent {:euuid c/iam}}
   {:euuid c/group-add
    :name "Group Add"
    :roles [*ROOT*]
    :parent {:euuid c/groups}}
   {:euuid c/group-modify
    :name "Group Edit"
    :roles [*ROOT*]
    :parent {:euuid c/groups}}
   {:euuid c/group-delete
    :name "Group Delete"
    :roles [*ROOT*]
    :parent {:euuid c/groups}}
   {:euuid c/group-members
    :name "Group Members"
    :roles [*ROOT*]
    :parent {:euuid c/groups}}
   {:euuid c/roles
    :name "Roles"
    :roles [*ROOT*]
    :parent {:euuid c/iam}}
   {:euuid c/role-add
    :name "Role Add"
    :roles [*ROOT*]
    :parent {:euuid c/roles}}
   {:euuid c/role-modify
    :name "Role Edit"
    :roles [*ROOT*]
    :parent {:euuid c/roles}}
   {:euuid c/role-delete
    :name "Role Delete"
    :roles [*ROOT*]
    :parent {:euuid c/roles}}
   {:euuid c/role-members
    :name "Role Members"
    :roles [*ROOT*]
    :parent {:euuid c/roles}}
   {:euuid c/role-permissions
    :name "Role Members"
    :roles [*ROOT*]
    :parent {:euuid c/roles}}
   ;;
   {:euuid c/apps
    :name "Apps"
    :roles [*ROOT*]
    :parent {:euuid c/iam}}
   {:euuid c/app-add
    :name "App Add"
    :roles [*ROOT*]
    :parent {:euuid c/apps}}
   {:euuid c/app-edit
    :name "App Edit"
    :roles [*ROOT*]
    :parent {:euuid c/apps}}
   {:euuid c/app-delete
    :name "App Delete"
    :roles [*ROOT*]
    :parent {:euuid c/apps}}])


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


(defn init-eywa-frontend-client
  []
  (letfn [(ensure [coll should-contain]
            (if (not-any? #(= should-contain %) coll)
              (conj coll should-contain)
              coll))]
    (let [domain env/iam-root-url
          client (when domain
                   (->
                     {:euuid #uuid "34c6eea5-3c95-4de1-a547-5e1b34ea16ea",
                      :id "MUMADPADAKQHSDFDGFAEJZJXUSFJGFOOYTWVAUDEFVPURUOP",
                      :name "EYWA Frontend",
                      :type "public",
                      :active true,
                      :secret nil,
                      :settings
                      {"logo-url" "https://www.eywaonline.com/eywa/logo/eywa.svg",
                       "login-page" "",
                       "redirections" [] ,
                       "token-expiry" {"id" 600, "access" 3600, "refresh" 129600},
                       "allowed-grants" ["authorization_code"
                                         "urn:ietf:params:oauth:grant-type:device_code"],
                       "refresh-tokens" true,
                       "logout-redirections" []}}
                     (update-in [:settings "redirections"] ensure (str domain "/eywa/callback"))
                     (update-in [:settings "logout-redirections"] ensure (str domain "/eywa/"))
                     (update-in [:settings "logout-redirections"] ensure (str domain "/eywa"))))]
      (when client (add-client client)))))



(comment
  (time (get-client "oauth_test_confidential"))
  
  (get-client "MUMADPADAKQHSDFDGFAEJZJXUSFJGFOOYTWVAUDEFVPURUOP")
  (get-client "HJIUVTENYXXOKEMGYXEUEHIKVKCKJPCNCLSXLSKNMFVEMAWJ")


  (sync-entity
    neyho.eywa.iam.uuids/user
    {:name "oauth_test"
     :password "change-me"
     :person_info {:name "Bosko Buha"
                   :given_name "Bosko"
                   :family_name "Buha"
                   :prefered_username "bbuha"
                   :nickname "partizaner"
                   :profile "https://bbuha.partizani.yu"
                   :picture "https://bbuha.partizani.yu/slika1"
                   :website "https://bbuha.partizani.yu/omeni"
                   :email_verified true
                   :gender nil
                   :birthdate "1984-01-00"
                   :zoneinfo nil
                   :phone_number "+38533333333"
                   :phone_number_verified nil
                   :address "Ulica Gora i Planina 44"}})


  (unsign-data
    (sign-data
      {:name "oauth_test"
       :password "change-me"}
      {:alg :rs256
       :exp (vura/date 2024 3 26 10 53)}))


  (def client
    (add-client
      {:euuid #uuid "7f30e780-37a1-11ef-a949-02a535895d2d",
       :id "HJIUVTENYXXOKEMGYXEUEHIKVKCKJPCNCLSXLSKNMFVEMAWJ",
       :name "oidc-client-test",
       :type :public,
       :active true,
       :secret nil
       :settings
       {"version" 0,
        "login-page" "http://localhost:8080/oauth/login/index.html",
        "redirections"
        ["http://localhost:8080/eywa/"
         "http://localhost:8080/eywa"
         "http://localhost:8080/eywa/callback"
         "http://localhost:8080/eywa/silent-callback"
         "http://localhost:8000/eywa/callback"
         "http://localhost:8000/eywa/silent-callback"
         "http://localhost:8080/app/kbdev"
         "http://localhost:5173/authentication/callback"
         "http://localhost:1234/sample.html"
         "http://localhost:1234/code-flow-duendesoftware/sample.html"
         "http://localhost:1234/code-flow-duendesoftware/sample-silent.html"
         "http://localhost:1234/code-flow-duendesoftware/sample-popup-signin.html"
         "http://localhost:1234/code-flow-duendesoftware/sample-popup-signout.html"
         "http://localhost:1234/oidc-client/sample.html"
         "http://localhost:1234/user-manager/sample.html"
         "http://localhost:1234/user-manager/sample.html"
         "http://localhost:1234/auth/callback"
         "http://localhost:1234/auth/silent-callback"
         "http://localhost:1234/"
         ],
        "token-expiry" {"access" 600, "refresh" 129600},
        "allowed-grants" ["refresh_token" "code" "token" "id_token"],
        "logout-redirections" ["http://localhost:5173/"
                               "http://localhost:1234/"
                               "http://localhost:8000/eywa/"
                               "http://localhost:1234/code-flow-duendesoftware/sample.html"]
        "refresh-tokens" true}}))


  (get-client (:id client))
  (remove-client client)

  
  ;;
  (add-client
    {:id "XFYWDCONOFSZMTVAEOQHTZFHSUCTXQ",
     :password "e9w7BwGDTLBgaHYxMpctUrOy_aVA4tiZHlgfb2GrotWiBhr_u0",
     :euuid #uuid "3349f1ff-2118-4b3e-babf-a8b68b7e98df",
     :name "oauth_test_confidential",
     :type :confidential,
     :settings
     {:version 0,
      :allowed-grants
      ["refresh_token" "client_credentials" "password" "code"],
      :token-expiry {:access (vura/minutes 5)
                     :refresh (vura/days 1.5)}
      :refresh-tokens true,
      :login-page "http://localhost:8080/login/kbdev/",
      :redirections
      ["http://localhost:8080/eywa/" "http://localhost:8080/app/kbdev"]}})

  ;;
  (add-client
    {:id "ZHXGGUGLQVOSJZHZCETLFTUZWSSRWG",
     :password nil,
     :euuid #uuid "62972fcf-3cfe-4d34-baea-055308612a0d",
     :name "oauth_test_public",
     :type :public,
     :settings
     {:version 0,
      :logo-url nil
      :login-page "http://localhost:8080/login/kbdev",
      :token-expiry {"access" (vura/minutes 5)
                     "refresh" (vura/days 1.5)}
      :allowed-grants
      ["refresh_token" "client_credentials" "password" "code"],
      :redirections
      ["http://localhost:8080/eywa/" "http://localhost:8080/app/kbdev"]}})


  (remove-client #uuid "62972fcf-3cfe-4d34-baea-055308612a0d")
  (remove-client #uuid "3349f1ff-2118-4b3e-babf-a8b68b7e98df"))
