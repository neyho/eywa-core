(ns neyho.eywa.iam
  (:require
    [clojure.string :as str]
    clojure.java.io
    clojure.pprint
    clojure.data.json
    [vura.core :as vura]
    [buddy.sign.jwt :as jwt]
    [buddy.core.codecs]
    [buddy.hashers :as hashers]
    [buddy.core.keys :as keys]
    [buddy.sign.util :refer [to-timestamp]]
    [nano-id.core :refer [nano-id] :as nano-id]
    [buddy.sign.jwk :as jwk]
    [neyho.eywa.iam.oauth2.uuids :as ou]
    [neyho.eywa.iam.uuids :as iu]
    [neyho.eywa.dataset
     :as dataset
     :refer [get-entity
             search-entity
             sync-entity
             delete-entity]])
  (:import
    [java.security KeyPairGenerator]))


(defonce ^:dynamic *private-key* nil)
(defonce ^:dynamic *public-key* nil)


(defn init-encryption
  [{:keys [public private]}]
  (if-not (keys/public-key? public) 
    (throw (ex-info "Unacceptable public key" {:key public}))
    (alter-var-root #'*private-key* (constantly private)))
  (if-not (keys/private-key? private)
    (throw (ex-info "Unacceptable private key" {:key private}))
    (alter-var-root #'*public-key* (constantly public))))


(defn generate-key-pair
  []
  (let [generator (KeyPairGenerator/getInstance "RSA")
        key-pair (.generateKeyPair generator)
        public (.getPublic key-pair)
        private (.getPrivate key-pair)]
    {:private private
     :public public}))


(defn init-default-encryption
  []
  (init-encryption (generate-key-pair)))


(defn sign-data
  "Function encrypts data that should be in map form and returns encrypted
  string."
  ([data] (sign-data
            data
            {:alg :rs256}))
  ([data settings]
   (jwt/sign
     data
     *private-key*
     settings)))


(comment
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
   (jwt/sign data private1 {:alg :rs256}))
  )


(defn unsign-data
  "Function takes encrypted string and returns decrypted data."
  [data]
  (jwt/unsign data *public-key* {:alg :rs256}))


(defn jwt-decode
  [token]
  (let [[header payload] (str/split token #"\.")]
    {:header (clojure.data.json/read-str (String. (buddy.core.codecs/b64->str header)))
     :payload (clojure.data.json/read-str (String. (buddy.core.codecs/b64->str payload)))}))

(comment
  
  ()
  (jwt/unsign)
  (jwt/decrypt token))


(defn get-password [username]
  (:password
    (get-entity
      ou/client
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
    ou/client
    {:id id}
    {:euuid nil
     :id nil
     :name nil
     :type nil
     :active nil
     :secret nil
     :settings nil}))


(let [alphabet "ACDEFGHIJKLMNOPQRSTUVWXYZ"]
  (def gen-client-id (nano-id/custom alphabet 48)))


(let [alphabet "ACDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz1234567890-"]
  (def gen-secret (nano-id/custom alphabet 48)))


(defn add-client [{:keys [id name secret settings type]
                   :or {id (gen-client-id)
                        type :public}}]
  (let [secret (or secret
                   (when (#{:confidential "confidential"} type)
                     (gen-secret)))]
    (sync-entity
      ou/client
      {:id id
       :name name
       :type type
       :settings settings
       :secret secret
       :active true})))


(defn remove-client [{:keys [euuid]}]
  (delete-entity iu/user {:euuid euuid}))


(defn set-user
  [user]
  (sync-entity iu/user user))

(defn delete-user
  [user]
  (delete-entity iu/user (select-keys user [:euuid])))


(defn list-clients
  []
  (search-entity
    ou/client nil
    {:euuid nil
     :name nil
     :id nil
     :secret nil
     :type nil
     :settings nil}))


(comment
  (time (get-client "oauth_test_confidential"))
  (get-client "XFYWDCONOFSZMTVAEOQHTZFHSUCTXQ")
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
        "login-page" "http://localhost:8080/login/eywa",
        "redirections"
        ["http://localhost:8080/eywa/"
         "http://localhost:8080/app/kbdev"
         "http://localhost:5173/authentication/callback"],
        "token-expiry" {"access" 300000, "refresh" 129600000},
        "allowed-grants" ["refresh_token" "code" "token" "id_token"],
        "logout-redirections" ["http://localhost:5173/"]
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
