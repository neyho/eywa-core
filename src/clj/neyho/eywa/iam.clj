(ns neyho.eywa.iam
  (:require
    [clojure.string :as str]
    clojure.java.io
    clojure.pprint
    clojure.data.json
    [vura.core :as vura]
    [buddy.sign.jwt :as jwt]
    [buddy.core.codecs.base64]
    [buddy.hashers :as hashers]
    [buddy.core.keys :as keys]
    [buddy.sign.util :refer [to-timestamp]]
    [buddy.sign.jwk :as jwk]
    [neyho.eywa.iam.oauth2.uuids :as ou]
    [neyho.eywa.iam.uuids :as iu]
    [neyho.eywa.dataset
     :as dataset
     :refer [get-entity
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


(defn init-default-encryption
  []
  (let [generator (KeyPairGenerator/getInstance "RSA")
        key-pair (.generateKeyPair generator)
        public (.getPublic key-pair)
        private (.getPrivate key-pair)]
    (init-encryption
      {:private private
       :public public})))


(defn sign-data
  "Function encrypts data that should be in map form and returns encrypted
  string."
  ([data] (sign-data
            data
            {:alg :rs256
             :exp (->
                    (vura/date)
                    vura/date->value
                    (+ vura/day)
                    vura/value->date
                    to-timestamp)}))
  ([data settings]
   (jwt/sign
     data
     *private-key*
     settings)))


(defn unsign-data
  "Function takes encrypted string and returns decrypted data."
  [data]
  (jwt/unsign data *public-key* {:alg :rs256}))


(defn jwt-decode
  [token]
  (let [[header payload] (str/split token #"\.")]
    {:header (clojure.data.json/read-str (String. (buddy.core.codecs.base64/decode header)))
     :payload (clojure.data.json/read-str (String. (buddy.core.codecs.base64/decode payload)))}))

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


(defn get-user-details [username]
  (->
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
     :password nil
     :settings nil}))


(defn add-client [{:keys [id name password settings]}]
  (sync-entity
    ou/client
    (cond->
      {:id id
       :name name
       :settings settings
       :active true}
      password (assoc :password password))))


(defn remove-client [{:keys [euuid]}]
  (delete-entity iu/user {:euuid euuid}))


(defn set-user
  [user]
  (sync-entity iu/user user))

(defn delete-user
  [user]
  (delete-entity iu/user (:euuid user)))


(comment
  (time (get-client "oauth_test_confidential"))
  (get-client "XFYWDCONOFSZMTVAEOQHTZFHSUCTXQ")
  (sync-entity
    neyho.eywa.iam.uuids/user
    {:name "oauth_test"
     :password "change-me"})

  (unsign-data
    (sign-data
      {:name "oauth_test"
       :password "change-me"}
      {:alg :rs256
       :exp (vura/date 2024 3 26 10 53)}))
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
