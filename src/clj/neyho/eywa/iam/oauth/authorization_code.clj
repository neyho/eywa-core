(ns neyho.eywa.iam.oauth.authorization-code
  (:require
   clojure.java.io
   clojure.pprint
   [clojure.data.json :as json]
   [clojure.tools.logging :as log]
   [nano-id.core :as nano-id]
   [vura.core :as vura]
   [neyho.eywa.iam :refer [publish]]
   [neyho.eywa.iam.oauth.core :as core
    :refer [get-client
            pprint]]
   [neyho.eywa.iam.oauth.token :as token
    :refer [grant-token
            token-error
            client-id-missmatch
            owner-not-authorized]]))

(defonce ^:dynamic *authorization-codes* (atom nil))

(def grant "authorization_code")

(defn delete [code]
  (swap! *authorization-codes* dissoc code))

(let [alphabet "ACDEFGHJKLMNOPQRSTUVWXYZ"]
  (def gen-authorization-code (nano-id/custom alphabet 30)))

(defn bind-authorization-code
  [session]
  (let [code (gen-authorization-code)]
    (swap! *authorization-codes* assoc code {:session session
                                             :at (System/currentTimeMillis)})
    (swap! core/*sessions* update session
           (fn [current]
             (->
              current
              (assoc :code code)
              (dissoc :authorization-code-used?))))
    (publish :oauth.grant/code {:session session :code code})
    code))

(defn set-session-authorized-at
  [session timestamp]
  (swap! core/*sessions* assoc-in [session :authorized-at] timestamp))

(defn get-session-authorized-at
  [session]
  (get-in @core/*sessions* [session :authorized-at]))

(defn get-session-code
  ([session]
   (get-in @core/*sessions* [session :code])))

(defn get-code-request
  [code]
  (get-in @*authorization-codes* [code :request]))

(defn get-code-session
  [code]
  (get-in @*authorization-codes* [code :session]))

(defn get-code-client
  [code]
  (get-client (:client_id (get-code-request code))))

(defn revoke-authorization-code
  ([code]
   (when code
     (let [session (get-code-session code)]
       (swap! *authorization-codes* dissoc code)
       (swap! core/*sessions* update session dissoc :code)
       (publish :oauth.revoke/code {:code code :session session})))))

(defn code-was-issued? [code] (true? (get-in @*authorization-codes* [code :issued?])))

(defn validate-client [request]
  (let [{:keys [client_id redirect_uri]} request
        base-redirect-uri (core/get-base-uri redirect_uri)
        {:keys [euuid]
         {redirections "redirections"
          allowed-grants "allowed-grants"} :settings
         :as client} (get-client client_id)
        grants (set allowed-grants)]
    (log/debugf "Validating client: %s" (pprint client))
    (cond
      (nil? euuid)
      (throw
       (ex-info
        "Client not registered"
        {:type "client_not_registered"
         :request request}))
      ;;
      (empty? redirections)
      (throw
       (ex-info
        "Client missing redirections"
        {:type "no_redirections"
         :request request}))
      ;;
      (empty? redirect_uri)
      (throw
       (ex-info
        "Client hasn't provided redirect URI"
        {:type "missing_redirect"
         :request request}))
      ;;
      (not-any? #(= base-redirect-uri %) redirections)
      (throw
       (ex-info
        "Client provided uri doesn't match available redirect URI(s)"
        {:type "redirect_missmatch"
         :request request}))
      ;;
      (not (contains? grants grant))
      (throw
       (ex-info
        "Client doesn't support authorization_code flow"
        {:type "access_denied"
         :request request}))
      ;;
      :else
      (do
        (swap! core/*clients* assoc (:euuid client) client)
        client))))

(defmethod grant-token "authorization_code"
  [request]
  (let [{:keys [code redirect_uri client_id client_secret]} request
        {{request-redirect-uri :redirect_uri
          :as original-request} :request
         client-euuid :client
         :keys [session expires-at]} (get @*authorization-codes* code)
        {id :id :as client
         _secret :secret
         {:strs [allowed-grants]} :settings
         session-client :id} (get @core/*clients* client-euuid)
        grants (set allowed-grants)
        {:keys [active]} (core/get-session-resource-owner session)]
    (log/debugf
     "[%s] Processing token code grant for code: %s\n%s"
     session code (pprint request))
    ; (def code code)
    ; (def session session)
    (if-not session
      ;; If session isn't available, that is if somebody
      ;; is trying to hack in
      (token-error
       "invalid_request"
       "Trying to abuse token endpoint for code that"
       "doesn't exsist or has expired. Further actions"
       "will be logged and processed")
      ;; If there is some session than check other requirements
      (cond
        ;;
        (not (contains? grants "authorization_code"))
        (token-error
         "unauthorized_grant"
         "Client sent access token request"
         "for grant type that is outside"
         "of client configured privileges")
        ;;
        (not (contains? @*authorization-codes* code))
        (token-error
         "invalid_request"
         "Provided authorization code is illegal!"
         "Your request will be logged"
         "and processed")
        ;;
        (< expires-at (System/currentTimeMillis))
        (token-error
         "invalid_request"
         "This authorization code has expired. Restart"
         "authentication process.")
        ;; If redirect uri doesn't match
        (not= request-redirect-uri redirect_uri)
        (token-error
         "invalid_request"
         "Redirect URI that you provided doesn't"
         "match URI that was issued to provided authorization code")
        ;; If client ids don't match
        (not= session-client client_id)
        (token-error
         "invalid_client"
         "Client ID that was provided doesn't"
         :w  "match client ID that was used in authorization request")
        ;;
        (and (some? _secret) (empty? client_secret))
        (token-error
         "invalid_client"
         "Client secret wasn't provided")
        ;; If client has secret, than
        (and (some? _secret) (not= client_secret _secret))
        (token-error
         "invalid_client"
         "Provided client secret is wrong")
        (not= id client_id)
        (do
          (log/debugf "[%s] Client don't match client from authorization request" code)
          client-id-missmatch)
        ;;
        (not active)
        (do
          (log/debugf "[%s] Resource owner is not active." code)
          (delete code)
          (core/kill-session session)
          owner-not-authorized)
        ;; Issue that token
        :else
        (let [tokens (token/generate client session original-request)
              response (json/write-str tokens)]
          (log/debugf "[%s]Returning token response:\n%s" code response)
          (revoke-authorization-code code)
          {:status 200
           :headers {"Content-Type" "application/json;charset=UTF-8"
                     "Pragma" "no-cache"
                     "Cache-Control" "no-store"}
           :body response})))))

(defn mark-code-issued [session code]
  (swap! *authorization-codes* update code
         (fn [data]
           (assoc data
             :issued? true
             :session session
             :expires-at (->
                          (System/currentTimeMillis)
                          (+ (vura/minutes 5)))))))

(defn clean-codes
  ([] (clean-codes (vura/minutes 8)))
  ([timeout]
   (let [now (System/currentTimeMillis)]
     (swap! *authorization-codes*
            (fn [codes]
              (reduce-kv
               (fn [result code {:keys [created-on] :as data}]
                 (if (or (nil? created-on) (> (- now created-on) timeout)) result
                     (assoc result code data)))
               nil
               codes))))))

(comment
  (clean-codes))
