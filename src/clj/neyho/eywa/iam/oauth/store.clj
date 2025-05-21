(ns neyho.eywa.iam.oauth.store
  (:require
   [vura.core :as vura]
   [patcho.patch :as patch]
   [clojure.pprint :refer [pprint]]
   [clojure.tools.logging :as log]
   [clojure.string :as str]
   [clojure.core.async :as async]
   [clojure.java.io :as io]
   [buddy.sign.jwt :as jwt]
   [environ.core :refer [env]]
   [neyho.eywa.iam :as iam]
   [neyho.eywa.transit :refer [<-transit]]
   [neyho.eywa.iam.oauth.core :as core]
   [neyho.eywa.iam.oauth.token :as token]
   [neyho.eywa.dataset.encryption :as encryption]
   [neyho.eywa.dataset :as dataset])
  (:import
   [java.util Base64]
   [java.security KeyFactory]
   [java.security.spec
    X509EncodedKeySpec
    PKCS8EncodedKeySpec]))

(def -client- #uuid "0757bd93-7abf-45b4-8437-2841283edcba")
(def -session- #uuid "b2562198-0817-4508-a941-d898373298e5")
(def -access- #uuid "405d7201-a74a-490d-a5f7-669701d1a735")
(def -refresh- #uuid "6a6511b9-0616-4eee-99e0-729c6058985c")
(def -key-pairs- #uuid "fd76f554-1158-4101-9469-98cd70dcbe68")

(defn ->b64 [^bytes value] (.encodeToString (Base64/getEncoder) value))
(defn <-b64 [^String value] (.decode (Base64/getDecoder) value))

(defn encode-rsa [rsa-key] (->b64 (.getEncoded rsa-key)))

(defn decode-public-key [^bytes rsa-key]
  (let [_bytes (<-b64 rsa-key)
        kf (KeyFactory/getInstance "RSA")
        spec (X509EncodedKeySpec. _bytes)]
    (.generatePublic kf spec)))

(defn decode-private-key [^bytes rsa-key]
  (let [_bytes (<-b64 rsa-key)
        kf (KeyFactory/getInstance "RSA")
        spec (PKCS8EncodedKeySpec. _bytes)]
    (.generatePrivate kf spec)))

(defn get-key-pairs
  []
  (map
   (fn [kp]
     (->
      kp
      (update :public decode-public-key)
      (update :private decode-private-key)))
   (dataset/search-entity
    -key-pairs-
    {:active {:_eq true}
     :_order_by {:modified_on :desc}}
    {:euuid nil
     :modified_on nil
     :kid nil
     :public nil
     :private nil})))

(defn on-key-pair-add
  [{{:keys [kid public private]} :key-pair}]
  (try
    (dataset/stack-entity
     -key-pairs-
     {:kid kid
      :active true
      :public (encode-rsa public)
      :private (encode-rsa private)})
    (catch Throwable ex
      (log/error ex "[OAuth Store] Couldn't save RSA keypair. Check if encryption is enabled"))))

(defn on-key-pair-remove
  [{{:keys [kid]} :key-pair}]
  (dataset/stack-entity -key-pairs- {:kid kid :active false}))

(defn on-token-revoke
  [{token-type :token/key
    token :token/data}]
  (dataset/stack-entity
   (if (= token-type :access_token) -access- -refresh-)
   {:value token
    :revoked true}))

(defn on-tokens-grant
  [{{refresh-token :refresh_token
     access-token :access_token} :tokens
    :keys [session]}]
  (let [{:keys [kid]} (jwt/decode-header access-token)]
    (when access-token
      (dataset/stack-entity
       -access-
       {:value access-token
        :session {:id session}
        :expires-at (core/expires-at access-token)
        :signed_by {:kid kid}}))
    (when refresh-token
      (dataset/stack-entity
       -refresh-
       {:value refresh-token
        :session {:id session}
        :expires-at (core/expires-at refresh-token)
        :signed_by {:kid kid}}))))

(defn on-session-create
  [{:keys [session audience user scope client]}]
  (dataset/stack-entity
   -session-
   {:id session
    :user {:euuid (:euuid user)}
    :audience audience
    :active true
    :client {:euuid client}
    :scope (str/join " " scope)}))

(defn on-session-kill
  [{:keys [session]}]
  (dataset/stack-entity
   -session-
   {:id session
    :active false}))

(defn current-version
  []
  (<-transit (slurp (io/resource "dataset/oauth_session.json"))))

; (defn load-dataset
;   []
;   (let [{current-version :name :as current} (current-version)
;         ;;
;         {deployed-version :name}
;         (dataset/latest-deployed-version #uuid "0f9bb720-4b94-445c-9780-a4af09e8536c")]
;     (when (and (vrs/newer? current-version deployed-version) db/*db*))))

(patch/upgrade
 ::dataset "0.1.3"
 (log/info "[IAM] Old version of OAuth Store is deployed. Deploying newer version!")
 (dataset/deploy! (current-version))
 (dataset/reload))

(defn level-store
  []
  (let [{current-version :name} (current-version)
        ;;
        {deployed-version :name} (dataset/latest-deployed-version #uuid "0f9bb720-4b94-445c-9780-a4af09e8536c")]
    (patch/apply ::dataset deployed-version current-version)))

(defn open-store
  []
  (let [store-messages (async/chan (async/sliding-buffer 200))
        topics [:keypair/added :keypair/removed
                :oauth.revoke/token :oauth.grant/tokens
                :oauth.session/created :oauth.session/killed]]
    (doseq [topic topics]
      (log/infof "[OAuth Store] Subscribing to: %s" topic)
      (async/sub iam/publisher topic store-messages))
    (log/info "[OAuth Store] Store waiting for messages...")
    (letfn [(test-message [_key data]
              (when (= (:topic data) _key)
                data))]
      (async/go-loop
       [data (async/<! store-messages)]
        (log/debugf "[OAuth Store] Received message\n%s" (with-out-str (pprint data)))
        (try
          (condp test-message data
            :keypair/removed :>> on-key-pair-remove
            :keypair/added :>> on-key-pair-add
            :oauth.session/created :>> on-session-create
            :oauth.session/killed :>> on-session-kill
            :oauth.grant/tokens :>> on-tokens-grant
            :oauth.revoke/token :>> on-token-revoke
            nil)
          (catch Throwable ex
            (log/errorf ex "[OAuth Store] Couldn't process received message: %s" (with-out-str (pprint data)))))
        (recur (async/<! store-messages))))))

(defn load-session
  [{[{access-token :value}] :access_tokens
    [{refresh-token :value}] :refresh_tokens
    session :id
    user :user
    {client :euuid} :client}]
  (let [{{audience "aud" scope "scope"} :payload} (iam/jwt-decode access-token)
        scope (set (str/split scope #" "))
        user-details (core/get-resource-owner (:name user))]
    (core/set-session session {:client client
                               :last-active (vura/date)})
    (token/set-session-tokens session audience
                              {:access_token access-token
                               :refresh_token refresh-token})
    (core/set-session-audience-scope session audience scope)
    (core/set-session-resource-owner session user-details)
    (core/set-session-authorized-at session (vura/date))))

(comment
  (def sessions *1)
  (load-session (first sessions)))

(defn load-sessions
  []
  (let [sessions
        (dataset/search-entity
         -session-
         {:active {:_boolean :TRUE}}
         {:euuid nil
          :id nil
          :client  [{:selections {:euuid nil}}]
          :user [{:selections {:name nil}}]
          :access_tokens [{:selections {:value nil}
                           :args {:_where {:revoked {:_boolean :NOT_TRUE}}}}]
          :refresh_tokens [{:selections {:value nil}
                            :args {:_maybe {:revoked {:_boolean :NOT_TRUE}}}}]})]
    (doseq [session sessions] (load-session session))))

(defn start
  ([]
   ;; TODO - add here OAuth model deployment
   (let [persistent? (#{"true" "TRUE" "YES" "yes" "y" "1"} (env :eywa-oauth-persistence))
         kps (try
               (not-empty (get-key-pairs))
               (catch Throwable ex
                 (log/errorf "[OAuth Store] Couldn't read from key pair table!")
                 ex))]
     (level-store)
     (cond
       ;;
       (and persistent? (not (encryption/initialized?)))
       (do
         (log/error "[OAuth Store] Initialize encryption. Can't store RSA private keys!")
         (iam/init-default-encryption))
       ;;
       (and persistent? (instance? Exception kps))
       (iam/init-default-encryption)
       ;;
       (and persistent? (empty? kps))
       (do
         (open-store)
         (iam/init-default-encryption))
       ;;
       (and persistent? (not-empty kps))
       (do
         (open-store)
         (reset! iam/encryption-keys kps)
         (load-sessions))
       ;;
       :else
       (iam/init-default-encryption)))))

(defn purge-key-pairs
  ([] (purge-key-pairs 0))
  ([older-than]
   (let [now (vura/time->value (vura/date))]
     (dataset/purge-entity
      -key-pairs-
      {:_where {:modified_on {:_le (vura/value->time
                                    (- now older-than))}}}
      {:kid nil}))))

(defn purge-sessions
  []
  (dataset/purge-entity
   -session-
   nil
   {:euuid nil
    :access_tokens [{:selections {:euuid nil}}]
    :refresh_tokens [{:selections {:euuid nil}}]}))

(defn purge-tokens
  []
  (dataset/purge-entity -access- nil {:euuid nil})
  (dataset/purge-entity -refresh- nil {:euuid nil}))

(comment
  (def older-than 0)
  (->
   (deref iam/encryption-keys)
   first
   :public
   iam/encode-rsa-key))
