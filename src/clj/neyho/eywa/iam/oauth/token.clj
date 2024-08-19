(ns neyho.eywa.iam.oauth.token
  (:require
    [clojure.string :as str]
    clojure.java.io
    clojure.pprint
    [clojure.tools.logging :as log]
    [nano-id.core :as nano-id]
    [vura.core :as vura]
    [clojure.data.json :as json]
    [buddy.sign.util :refer [to-timestamp]]
    [io.pedestal.interceptor.chain :as chain]
    [neyho.eywa.iam
     :refer [sign-data
             validate-password]]
    [neyho.eywa.iam.oauth.core :as core
     :refer [pprint
             process-scope
             sign-token]]
    [neyho.eywa.iam.oauth.authorization-code :as ac]))


(defonce ^:dynamic *tokens* (atom nil))

(let [alphabet "ACDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"]
  (def gen-token (nano-id/custom alphabet 50)))


(let [default (vura/hours 2)]
  (defn access-token-expiry
    [{{{expiry "access"} "token-expiry"} :settings}]
    (or expiry default)))


(let [default (vura/days 1.5)]
  (defn refresh-token-expiry
    [{{{expiry "refresh"} "token-expiry"} :settings}]
    (or expiry default)))


(defn token-error [code & description]
  ; (log/debugf "Returning error: %s\n%s" code (str/join "\n" description))
  {:status 400
   :headers {"Content-Type" "application/json;charset=UTF-8"
             "Pragma" "no-cache"
             "Cache-Control" "no-store"}
   :body (json/write-str
           {:error code
            :error_description (str/join "\n" description)})})


(defn set-session-tokens
  ([session tokens] (set-session-tokens session nil tokens))
  ([session audience tokens]
   (swap! core/*sessions* assoc-in [session :tokens audience] tokens)
   (swap! *tokens*
          (fn [current-tokens]
            (reduce-kv
              (fn [tokens token-key data]
                (log/debugf "[%s] Adding token %s %s" session token-key data)
                (assoc-in tokens [token-key data] session))
              current-tokens
              tokens)))
   nil))


(defn get-token-session
  [token-key token]
  (get-in @*tokens* [token-key token]))


(defn get-token-audience
  [token-key token]
  (let [session (get-token-session token-key token)]
    (reduce-kv
      (fn [_ audience {_token token-key}]
        (when (= token _token)
          (reduced audience)))
      nil
      session)))


(defn get-session-access-token
  ([session] (get-session-access-token session nil))
  ([session audience]
   (get-in @core/*sessions* [session :tokens audience :access_token])))


(defn get-session-refresh-token
  ([session] (get-session-refresh-token session nil))
  ([session audience]
   (get-in @core/*sessions* [session :tokens audience :refresh_token])))


(defn revoke-token
  ([session token-key] (revoke-token session nil token-key))
  ([session audience token-key]
   (let [{{{token token-key} :tokens} audience} (core/get-session session)]
     (swap! core/*sessions* update-in [session :tokens audience] dissoc token-key)
     (when token
       (swap! *tokens* update token-key dissoc token)
       (core/publish :revoke/token
                     {:token/key token-key
                      :token/data token
                      :session session})))
   nil))


(defn revoke-session-tokens
  ([session]
   (doseq [[audience] (core/get-session session)]
     (revoke-session-tokens session audience)))
  ([session audience]
   (let [{{:keys [tokens]} audience} (core/get-session session)]
     (doseq [[token-key] tokens]
       (revoke-token session token-key)))
   nil))


(defmethod sign-token :refresh_token
  [_ _ data]
  data
  #_(let [client (get-session-client session)]
    (sign-data
      (assoc data
             :exp (-> (vura/date)
                      vura/date->value
                      (+ (refresh-token-expiry client))
                      vura/value->date
                      to-timestamp))
      {:alg :rs256})))


(defmethod sign-token :access_token
  [_ _ data]
  (sign-data data {:alg :rs256}))


(comment
  (def d
    (sign-data
      (assoc {:a 100}
             :exp (-> 
                    (System/currentTimeMillis)
                    (quot 1000)
                    (+ (access-token-expiry client))))
      {:alg :rs256}))
  (def client (val (first @*clients*)))
  (unsign-data d))


(def unsupported (core/json-error 500 "unsupported" "This feature isn't supported at the moment"))


(def client-id-missmatch
  (token-error
    "unauthorized_client"
    "Refresh token that you have provided"
    "doesn't belong to given client"))


(def owner-not-authorized
  (token-error
    "resource_owner_unauthorized"
    "Provided refresh token doesn't have active user"))


(def refresh-not-supported
  (token-error
    "invalid_request"
    "The client configuration does not support"
    "token refresh requests."))


(def cookie-session-missmatch
  (token-error
    "invalid_request"
    "You session is not provided by this server."
    "This action will be logged and processed!"))


(defmulti grant-token (fn [{:keys [grant_type]}] grant_type))

  
(defmethod grant-token :default [_] unsupported)


(defmethod grant-token "authorization_code"
  [request]
  (let [{:keys [code redirect_uri client_id client_secret grant_type audience]} request
        {request-redirect-uri :redirect_uri
         scope :scope} (ac/get-code-request code)
        session (ac/get-code-session code)
        {{refresh? "refresh-tokens"} :settings :as client
         id :id} (core/get-session-client session)
        {:keys [euuid active]} (core/get-session-resource-owner session)]
    (log/debugf
      "[%s] Processing token code grant for code: %s\n%s"
      session code (pprint request))
    (if-not session
      ;; If session isn't available, that is if somebody
      ;; is trying to hack in
      (token-error
        "invalid_request"
        "Trying to abuse token endpoint for code that"
        "doesn't exsist or has expired. Further actions"
        "will be logged and processed")
      ;; If there is some session than check other requirements
      (let [{_secret :secret
             {:strs [allowed-grants]} :settings
             session-client :id} (core/get-session-client session)
            grants (set allowed-grants)]
        (cond
          ;;
          (not (contains? @ac/*authorization-codes* code))
          (token-error
            "invalid_request"
            "Provided authorization code is illegal!"
            "Your request will be logged"
            "and processed")
          ;;
          (or
            (not (contains? grants "code"))
            (not= grant_type "authorization_code"))
          (token-error
            "unauthorized_grant"
            "Client sent access token request"
            "for grant type that is outside"
            "of client configured privileges")
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
            "match client ID that was used in authorization request")
          ;;
          (and (some? _secret) (empty? client_secret))
          (token-error
            "invalid_client"
            "Client secret wasn't provided")
          ;; If client has secret, than
          (and (some? _secret) (not (validate-password client_secret _secret)))
          (token-error
            "invalid_client"
            "Provided client secret is wrong")
          (not= id client_id)
          client-id-missmatch
          ;;
          (not active)
          (do
            (core/kill-session session)
            owner-not-authorized)
          ;; Issue that token
          :else
          (let [access-exp (-> 
                             (System/currentTimeMillis)
                             (quot 1000)
                             (+ (access-token-expiry client)))
                access-token {:session session
                              :aud audience
                              :exp access-exp
                              :iss (core/domain+)
                              :sub euuid
                              :iat (-> (vura/date) to-timestamp)
                              :client_id client_id
                              :sid session
                              :scope (str/join " " scope)}
                response (if (pos? access-exp)
                           (let [refresh-token (when (and refresh? session
                                                          (contains? scope "offline_access"))
                                                 (log/debugf "Creating refresh token: %s" session)
                                                 (gen-token)
                                                 #_{:iss *iss*
                                                    :sid session})
                                 tokens (reduce
                                          (fn [tokens scope]
                                            (process-scope session tokens scope))
                                          (if refresh-token
                                            {:access_token access-token
                                             :refresh_token refresh-token}
                                            {:access_token access-token})
                                          scope)
                                 signed-tokens (reduce-kv
                                                 (fn [tokens token data]
                                                   (assoc tokens token (sign-token session token data)))
                                                 tokens
                                                 tokens)]
                             (def tokens tokens)
                             (def refresh-token refresh-token)
                             (def access-token access-token)
                             (def scope scope)
                             (def session session)
                             (when refresh-token (revoke-token session audience :refresh_token))
                             (when session (set-session-tokens session audience signed-tokens))
                             (ac/revoke-authorization-code code)
                             (assoc signed-tokens
                                    :type "Bearer"
                                    :scope (str/join " " scope)
                                    :expires_in (access-token-expiry client)))
                           (let [tokens (reduce
                                          (fn [tokens scope]
                                            (process-scope session tokens scope))
                                          {:access_token access-token}
                                          scope)
                                 signed-tokens (reduce-kv
                                                 (fn [tokens token data]
                                                   (assoc tokens token (sign-token session token data)))
                                                 tokens
                                                 tokens)]
                             (ac/revoke-authorization-code code)
                             (assoc signed-tokens
                                    :expires_in (access-token-expiry client)
                                    :scope (str/join " " scope)
                                    :type "Bearer")))]
            {:status 200
             :headers {"Content-Type" "application/json;charset=UTF-8"
                       "Pragma" "no-cache"
                       "Cache-Control" "no-store"}
             :body (json/write-str response)}))))))


(defmethod grant-token "refresh_token"
  [{:keys [refresh_token client_id scope audience]
    cookie-session :idsrv/session}]
  (let [session (get-token-session :refresh_token refresh_token)
        {{refresh? "refresh-tokens"} :settings :as client} (core/get-session-client session)
        {:keys [euuid active]} (core/get-session-resource-owner session)]
    (when session (revoke-session-tokens session audience))
    (cond
      ;;
      (not active)
      (do
        (core/kill-session session)
        owner-not-authorized)
      ;;
      (not refresh?)
      refresh-not-supported
      ;;
      (and cookie-session (not= cookie-session session))
      cookie-session-missmatch
      ;;
      :else
      (let [access-exp (->
                         (System/currentTimeMillis)
                         (quot 1000)
                         (+ (access-token-expiry client)))
            access-token {:session session
                          :aud audience
                          :iss (core/domain+)
                          :sub euuid
                          :iat (-> (vura/date) to-timestamp)
                          :client_id client_id
                          :sid session
                          :exp access-exp 
                          :scope (str/join " " scope)}
            response (if (pos? access-exp)
                       (let [refresh-token (gen-token)
                             ; refresh-token {:iss *iss*
                             ;                :sid session}
                             tokens (reduce
                                      (fn [tokens scope]
                                        (process-scope session tokens scope))
                                      {:access_token access-token
                                       :refresh_token refresh-token}
                                      scope)
                             signed-tokens (reduce-kv
                                             (fn [tokens token data]
                                               (if-let [payload (sign-token session token data)]
                                                 (assoc tokens token payload)
                                                 tokens))
                                             tokens
                                             tokens)]
                         (revoke-session-tokens session audience)
                         (when session (set-session-tokens session audience signed-tokens))
                         (assoc signed-tokens
                                :type "Bearer"
                                :expires_in (access-token-expiry client)
                                :scope (str/join " " scope)))
                       (let [tokens (reduce
                                      (fn [tokens scope]
                                        (process-scope session tokens scope))
                                      {:access_token access-token}
                                      scope)
                             signed-tokens (reduce-kv
                                             (fn [tokens token data]
                                               (assoc tokens token (sign-token session token data)))
                                             tokens
                                             tokens)]
                         (assoc signed-tokens
                                :type "Bearer"
                                :expires_in (access-token-expiry client) 
                                :scope (str/join " " scope))))]
        {:status 200
         :headers {"Content-Type" "application/json;charset=UTF-8"
                   "Pragma" "no-cache"
                   "Cache-Control" "no-store"}
         :body (json/write-str response)}))))



(defn token-endpoint
  [{:keys [grant_type] :as request}]
  (log/debugf "Received token endpoint request\n%s" (pprint request))
  (case grant_type
    ;; Authorization code grant
    ("authorization_code" "refresh_token" "urn:ietf:params:oauth:grant-type:device_code")
    (grant-token request)
    ;;else
    (core/handle-request-error
      {:type "unsupported_grant_type"
       :grant_type grant_type})))



(def token-interceptor
  {:name ::token
   :enter
   (fn [{request :request :as context}]
     (chain/terminate
       (assoc context :response (token-endpoint (:params request)))))})


(let [invalid-client (core/json-error "invalid_client" "Client ID is not valid")
      invalid-token (core/json-error "invalid_token" "Token is not valid")]
  (def revoke-token-interceptor
    {:enter
     (fn [{{{:keys [token_type_hint token] :as params} :params} :request :as ctx}]
       (let [token-key (when token_type_hint (keyword token_type_hint))
             tokens @*tokens*
             [token-key session] (some
                                   (fn [token-key]
                                     (when-some [session (get-in tokens [token-key token])]
                                       [token-key session]))
                                   [token-key :access_token :refresh_token])]

         (letfn [(error [response]
                   (log/errorf "[%s] Couldn't revoke token. Returning\n%s" session response)
                   (chain/terminate (assoc ctx :response response)))]
           (cond
             (nil? session) (error invalid-token)
             (core/clients-doesnt-match? session params) (error invalid-client)
             :else (do
                     (log/debugf "[%s] Revoking token %s %s" session token-key token)
                     (revoke-token token-key token)
                     (assoc ctx :response
                            {:status 200
                             :headers {"Content-Type" "application/json"
                                       "Cache-Control" "no-store"
                                       "Pragma" "no-cache"}}))))))}))
