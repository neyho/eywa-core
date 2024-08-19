(ns neyho.eywa.iam.oauth
  (:require
    [clojure.string :as str]
    clojure.java.io
    clojure.pprint
    [clojure.set :as set]
    [clojure.core.async :as async]
    [clojure.tools.logging :as log]
    [clojure.walk :refer [keywordize-keys]]
    [clojure.spec.alpha :as s]
    [nano-id.core :as nano-id]
    [vura.core :as vura]
    [ring.util.codec :as codec]
    [clojure.data.json :as json]
    [buddy.sign.util :refer [to-timestamp]]
    [buddy.core.codecs :as codecs]
    [buddy.core.crypto :as crypto]
    [io.pedestal.interceptor.chain :as chain]
    [io.pedestal.http.body-params :as bp]
    [neyho.eywa.iam
     :refer [sign-data
             unsign-data
             get-client
             validate-password
             get-user-details]]
    [neyho.eywa.server.interceptors :refer [spa-interceptor]]
    [neyho.eywa.env :as env])
  (:import
    [java.util Base64]))


(defn pprint [data] (with-out-str (clojure.pprint/pprint data)))


(defonce subscription (async/chan (async/sliding-buffer 10000)))
(defonce publisher
  (async/pub
    subscription
    (fn [{:keys [topic]
          :or {topic ::broadcast}}]
      topic)))


(defn publish [topic data]
  (async/put! subscription (assoc data :topic topic)))


(defonce ^:dynamic *iss* "http://localhost:8080")


(defonce ^:dynamic *tokens* (atom nil))
(defonce ^:dynamic *authorization-codes* (atom nil))
(defonce ^:dynamic *device-codes* (atom nil))


(defonce ^:dynamic *resource-owners* (atom nil))
(defonce ^:dynamic *clients* (atom nil))
(defonce ^:dynamic *sessions* (atom nil))


(defn domain+
  ([] (domain+ ""))
  ([path]
   (str env/iam-protocol "://" env/iam-domain path)))



(defonce ^:dynamic *encryption-key* (nano-id/nano-id 32))
(defonce ^:dynamic *initialization-vector* (nano-id/nano-id 12))


(defn encrypt [data]
            (let [json-data (.getBytes (json/write-str data))]
              (codecs/bytes->hex
                (crypto/encrypt
                  json-data *encryption-key* *initialization-vector*
                  {:alg :aes256-gcm}))))


(defn decrypt [encrypted-data]
  (String.
    (crypto/decrypt
      (codecs/hex->bytes encrypted-data) *encryption-key* *initialization-vector*
      {:alg :aes256-gcm})))


(let [alphabet "ACDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"]
  (def gen-session-id (nano-id/custom alphabet 30)))

(let [alphabet "ACDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"]
  (def gen-token (nano-id/custom alphabet 50)))

(let [alphabet "ACDEFGHJKLMNOPQRSTUVWXYZ"]
  (def gen-authorization-code (nano-id/custom alphabet 30)))


(let [default (vura/hours 2)]
  (defn access-token-expiry
    [{{{expiry "access"} "token-expiry"} :settings}]
    (or expiry default)))


(let [default (vura/days 1.5)]
  (defn refresh-token-expiry
    [{{{expiry "refresh"} "token-expiry"} :settings}]
    (or expiry default)))


(defmulti sign-token (fn [_ token-key _] token-key))


(defmethod sign-token :default
  [session token-key data]
  (log/errorf "[%s] Couldn't signt token `%s`" session token-key)
  data)


(defmulti process-scope (fn [_ _ scope] scope))


(defmethod process-scope :default
  [session tokens scope]
  (log/errorf "[%s] Couldn't find scope resolver for scope `%s`" session scope)
  tokens)


(declare revoke-session-tokens set-session-tokens)


(defn get-session-client [session]
  (let [euuid (get-in @*sessions* [session :client])]
    (get @*clients* euuid)))


(defn get-session-resource-owner [session]
  (let [euuid (get-in @*sessions* [session :resource-owner])]
    (get @*resource-owners* euuid)))


(defn token-error [code & description]
  ; (log/debugf "Returning error: %s\n%s" code (str/join "\n" description))
  {:status 400
   :headers {"Content-Type" "application/json;charset=UTF-8"
             "Pragma" "no-cache"
             "Cache-Control" "no-store"}
   :body (json/write-str
           {:error code
            :error_description (str/join "\n" description)})})


(defn get-base-uri
  "Returns the base URI without query parameters from the given URL."
  [url]
  (when (not-empty url)
    (let [uri (java.net.URI. url)]
      (str (java.net.URI. (.getScheme uri) (.getAuthority uri) (.getPath uri) nil nil)))))


(defn validate-resource-owner [username password]
  (let [{db-password :password
         active :active
         :as resource-owner} (get-user-details username)]
    (if-not active nil
      (when (validate-password password db-password)
        (dissoc resource-owner :password)))))


(defn set-session-resource-owner
  [session {:keys [euuid] :as resource-owner}]
  (swap! *sessions* assoc-in [session :resource-owner] euuid)
  (swap! *resource-owners* update euuid
         (fn [current]
           (->
             current 
             (merge resource-owner)
             (update :sessions (fnil conj #{}) session))))
  nil)


(defn remove-session-resource-owner [session]
  (let [euuid (get-in @*sessions* [session :resource-owner])]
    (swap! *sessions* update session dissoc :resource-owner)
    (when euuid
      (swap! *resource-owners* update euuid
             (fn [current]
               (update current :sessions (fnil disj #{}) session))))
    nil))


(defn set-session-client [session {:keys [euuid] :as client}]
  (swap! *sessions* assoc-in [session :client] euuid)
  (swap! *clients* update euuid
         (fn [current]
           (->
             current 
             (merge client)
             (update :sessions (fnil conj #{}) session))))
  nil)


(defn remove-session-client [session]
  (let [euuid (get-in @*sessions* [session :client])]
    (swap! *sessions* update session dissoc :client)
    (when euuid
      (swap! *clients* update euuid
             (fn [current]
               (update current :sessions (fnil disj #{}) session))))
    nil))


(defn get-session [id] (get @*sessions* id))
(defn set-session [id data] (swap! *sessions* assoc id data))
(defn remove-session [id]
  (let [{{client-euuid :euuid :as client} :client
         {resource-owner-euuid :euuid :as resource-owner} :resource-owner} (get-session id)]
    (swap! *sessions* dissoc id)
    ;; Remove session from resource owner
    (when resource-owner
      (swap! *resource-owners* update resource-owner-euuid
             (fn [current]
               (update current :sessions (fnil disj #{}) id))))
    ;; Remove session from client
    (when client
      (swap! *clients* update client-euuid
             (fn [current]
               (update current :sessions (fnil disj #{}) id))))
    nil))


(defn get-redirection-uris [session]
  (let [{{:strs [redirections]} :settings} (get-session-client session)]
    redirections))


(def request-errors
  (reduce-kv
    (fn [r k v]
      (assoc r k (str/join "\n" v)))
    nil
    {"invalid_request"
     ["The request is missing a required parameter, includes an"
      "invalid parameter value, includes a parameter more than" 
      "once, or is otherwise malformed."]
     ;;
     "unauthorized_client"
     ["The client is not authorized to request an authorization"
      "code using this method."]
     ;;
     "access_denied"
     ["The resource owner or authorization server denied the"
      "request."]
     ;;
     "unsupported_response_type"
     ["The authorization server does not support obtaining an"
      "authorization code using this method."]
     ;;
     "invalid_scope"
     ["The requested scope is invalid, unknown, or malformed."]
     ;;
     "server_error"
     ["The authorization server encountered an unexpected"
      "condition that prevented it from fulfilling the request."]
     ;;
     "temporarily_unavailable"
     ["The authorization server is currently unable to handle"
      "the request due to a temporary overloading or maintenance"
      "of the server."]}))


(defn handle-request-error
  [{t :type
    session :session
    request :request
    description :description
    :as data}]
  (let [{:keys [state redirect_uri]} (or
                                       request
                                       (:request (get-session session)))
        base-redirect-uri (get-base-uri redirect_uri)]
    (when session (remove-session session))
    (case t
      ;; When redirection uri error than redirect to 
      ("no_redirections"
        "missing_redirect" "redirect_missmatch" "missing_response_type"
        "client_not_registered" "corrupt_session" "unsupported_grant_type")
      {:status 302
       :headers {"Location" (str "/oauth2/request_error?"
                                 (codec/form-encode (select-keys data [:type])))
                 "Cache-Control" "no-cache"}}
      ;; Otherwise
      {:status 302
       ;; TODO - handle when redirect uri already has parameters
       :headers {"Location" (str base-redirect-uri "?"
                                 (codec/form-encode
                                   (cond->
                                     {:error t}
                                     description (assoc :error_description description)
                                     state (assoc :state state))))
                 "Cache-Control" "no-cache"}})))


(defn bind-authorization-code
  [session]
  (let [code (gen-authorization-code)]
    (swap! *authorization-codes* assoc code {:session session
                                             :at (System/currentTimeMillis)})
    (swap! *sessions* update session
           (fn [current]
             (->
               current 
               (assoc :code code)
               (dissoc :authorization-code-used?))))
    (publish :grant/code {:session session :code code})
    code))


(defn set-session-authorized-at
  [session timestamp]
  (swap! *sessions* assoc-in [session :authorized-at] timestamp))


(defn get-session-authorized-at
  [session]
  (get-in @*sessions* [session :authorized-at]))


(defn set-session-tokens
  ([session tokens] (set-session-tokens session nil tokens))
  ([session audience tokens]
   (swap! *sessions* assoc-in [session :tokens audience] tokens)
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


(defn get-session-code
  ([session]
   (get-in @*sessions* [session :code])))


(defn get-session-access-token
  ([session] (get-session-access-token session nil))
  ([session audience]
   (get-in @*sessions* [session :tokens audience :access_token])))


(defn get-session-refresh-token
  ([session] (get-session-refresh-token session nil))
  ([session audience]
   (get-in @*sessions* [session :tokens audience :refresh_token])))


(defn get-code-session
  [code]
  (get-in @*authorization-codes* [code :session]))


(defn revoke-authorization-code
  ([code]
   (when code
     (let [session (get-code-session code)]
       (swap! *authorization-codes* dissoc code)
       (swap! *sessions* update session dissoc :code)
       (publish :revoke/code {:code code :session session})))))


(defn revoke-token
  ([session token-key] (revoke-token session nil token-key))
  ([session audience token-key]
   (let [{{{token token-key} :tokens} audience} (get-session session)]
     (swap! *sessions* update-in [session :tokens audience] dissoc token-key)
     (when token
       (swap! *tokens* update token-key dissoc token)
       (publish :revoke/token
                {:token/key token-key
                 :token/data token
                 :session session})))
   nil))


(defn revoke-session-tokens
  ([session]
   (doseq [[audience] (get-session session)]
     (revoke-session-tokens session audience)))
  ([session audience]
   (let [{{:keys [tokens]} audience} (get-session session)]
     (doseq [[token-key] tokens]
       (revoke-token session token-key)))
   nil))


(defn kill-session
  [session]
  (let [session-data (get-session session)]
    (remove-session-resource-owner session)
    (remove-session-client session)
    (revoke-session-tokens session)
    (when-let [code (:code session-data)]
      (revoke-authorization-code code))
    (swap! *sessions* dissoc session)
    (publish :session/kill
             {:session session
              :data session-data})))


(defn kill-sessions
  []
  (doseq [[session] @*sessions*]
    (kill-session session)))



(s/def ::authorization-code-grant #{"authorization_code"})
(s/def ::password-grant #{"password"})
(s/def ::implict-grant #{"implicit"})
(s/def ::refresh-token-grant #{"refresh_token"})
(s/def ::client-credentials-grant #{"client_credentials"})


(s/def ::grant_type
  (s/or
    :authorization-code ::authorization-code-grant
    :password ::password-grant
    :refresh-token ::refresh-token-grant
    :implicit ::implict-grant))


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


(defn json-error
  [status & description]
  (let [_status (if (number? status) status 400)
        [code & description] (if (number? status)
                               description
                               (concat [status] description))]
    {:status _status
     :headers {"Content-Type" "application/json;charset=UTF-8"
               "Pragma" "no-cache"
               "Cache-Control" "no-store"}
     :body (json/write-str
             {:error code
              :error_description (str/join "\n" description)})}))





(defn get-code-request
  [code]
  (get-in @*authorization-codes* [code :request]))


(defmulti grant-token (fn [{:keys [grant_type]}] grant_type))


(let [unsupported (json-error 500 "unsupported" "This feature isn't supported at the moment")
      client-id-missmatch (token-error
                            "unauthorized_client"
                            "Refresh token that you have provided"
                            "doesn't belong to given client")
      owner-not-authorized (token-error
                             "resource_owner_unauthorized"
                             "Provided refresh token doesn't have active user")
      refresh-not-supported (token-error
                              "invalid_request"
                              "The client configuration does not support"
                              "token refresh requests.")
      cookie-session-missmatch (token-error
                                 "invalid_request"
                                 "You session is not provided by this server."
                                 "This action will be logged and processed!")]
  
  (defmethod grant-token :default [_] unsupported)

  (defmethod grant-token "authorization_code"
    [request]
    (let [{:keys [code redirect_uri client_id client_secret grant_type audience]} request
          {request-redirect-uri :redirect_uri
           scope :scope} (get-code-request code)
          session (get-code-session code)
          {{refresh? "refresh-tokens"} :settings :as client
           id :id} (get-session-client session)
          {:keys [euuid active]} (get-session-resource-owner session)]
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
               session-client :id} (get-session-client session)
              grants (set allowed-grants)]
          (cond
            ;;
            (not (contains? @*authorization-codes* code))
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
              (kill-session session)
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
                                :iss *iss*
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
                               (when refresh-token (revoke-token session audience :refresh_token))
                               (when session (set-session-tokens session audience signed-tokens))
                               (revoke-authorization-code code)
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
                               (revoke-authorization-code code)
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
          {{refresh? "refresh-tokens"} :settings :as client} (get-session-client session)
          {:keys [euuid active]} (get-session-resource-owner session)]
      (when session (revoke-session-tokens session audience))
      (cond
        ;;
        (not active)
            (do
              (kill-session session)
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
                            :iss *iss*
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
           :body (json/write-str response)})))))



(defn token-endpoint
  [{:keys [grant_type] :as request}]
  (log/debugf "Received token endpoint request\n%s" (pprint request))
  (case grant_type
    ;; Authorization code grant
    ("authorization_code" "refresh_token")
    (grant-token request)
    ;;else
    (handle-request-error
      {:type "unsupported_grant_type"
       :grant_type grant_type})))


(defn mark-code-issued [code] (swap! *authorization-codes* assoc-in [code :issued?] true))


(defn code-was-issued? [code] (true? (get-in @*authorization-codes* [code :issued?])))


(defn prepare-code
  ([session request]
   (let [{:keys [code]} (get-session session)]
     ;; Session has code bount
     (if (and code
              ;; and this code is valid
              (contains? @*authorization-codes* code)
              ;; and hasn't been shared to user
              (not (code-was-issued? code)))
       code
       (let [new-code (gen-authorization-code)]
         (swap! *authorization-codes*
                (fn [codes]
                  (->
                    codes
                    (assoc new-code {:request request
                                     :session session
                                     :at (System/currentTimeMillis)})
                    (dissoc code))))
         (swap! *sessions* assoc-in [session :code] new-code)
         code)))))



(defn validate-client [request]
  (let [{:keys [client_id state redirect_uri]
          request-secret :client_secret} request 
        base-redirect-uri (get-base-uri redirect_uri)
        {:keys [euuid secret type]
         {redirections "redirections"} :settings
         :as client} (get-client client_id)]
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
      (or (some? request-secret) (some? secret))
      (if (validate-password request-secret secret)
        client
        (throw
          (ex-info
            "Client secret missmatch"
            {:type "access_denied"
             :request request
             :state state})))
      ;;
      (and (= type "public") (nil? secret))
      client
      ;;
      :else
      (do
        (log/errorf "Couldn't validate client\n%s" (pprint request))
        (throw
          (ex-info "Unknown client error"
                   {:request request
                    :type "server_error"}))))))


(defn authorization-code-flow
  [{cookie-session :idsrv/session :as request
    :keys [prompt redirect_uri state]}]
  ; (def request request)
  ; (def cookie-session (get request :idsrv/session))
  ; (def now (System/currentTimeMillis))
  (let [now (System/currentTimeMillis)
        session (if (get-session cookie-session) cookie-session
                  (let [session (gen-session-id)]
                    (set-session session {:at now})
                    session))
        silent? (and (some? cookie-session) (= prompt "none"))]
    (cond
      ;; Check that cookie session and session match
      (and silent? cookie-session (not= cookie-session session))
      (handle-request-error
        {:request request
         :type "login_required"
         :state "310903921"
         :description "Your session isn't authenticated. First log in"})
      ;; Check that there isn't some other code active
      (and silent? (contains? (get-session session) :code)) 
      (handle-request-error
        {:request request
         :type "invalid_request"
         :description "Your session has unused access code active"})
      ;; When silent and above checks passed, than return directly to
      ;; requested redirect_uri, with prepared authorization code
      silent?
      (try
        (validate-client request)
        (let [code (prepare-code session request)]
          (mark-code-issued code)
          {:status 302
           :headers {"Location" (str redirect_uri "?" (codec/form-encode {:state state :code code}))}})
        (catch clojure.lang.ExceptionInfo ex
          (handle-request-error (ex-data ex))))
      ;;
      :else
      (try
        (let [client (validate-client request)]
          ;; Proper implementation
          (set-session-client session client)
          ;; Prepare session code
          (prepare-code session request)
          (let [{{url "login-page"} :settings} (get-session-client session)]
            {:status 302
             :headers {"Location" (str url "?" (codec/form-encode {:session session}))
                       "Cache-Control" "no-cache"}}))
        (catch clojure.lang.ExceptionInfo ex
          (handle-request-error (ex-data ex)))))))


(defn authorization-request
  [request]
  (log/debugf "Authorizing request:\n%s" request)
  (letfn [(split-spaces [request k]
            (if-some [val (get request k)]
              (assoc request k (set (str/split val #"\s+")))
              request))]
    (let [{:keys [response_type redirect_uri]
           :as request}
          (-> request
              (split-spaces :scope)
              (split-spaces :response_type))]
      (cond
        (empty? redirect_uri)
        (handle-request-error
          {:type "missing_redirect"
           :request request})
        ;;
        (empty? response_type)
        (handle-request-error
          {:type "missing_response_type"
           :request request})
        ;;
        (contains? response_type "code")
        (authorization-code-flow request)
        ;;
        :else
        (handle-request-error
          {:type "server_error"
           :request request})))))


(def authorize-request-interceptor
  {:name ::authorize-request
   :enter
   (fn [{{:keys [params]} :request :as context}]
     (chain/terminate
       (assoc context :response
              (authorization-request params))))})


(def authorize-request-error-interceptor
  {:name ::authorize-error
   :enter
   (fn [{{:keys [params]} :request :as context}]
     (chain/terminate
       (assoc context :response
              {:status 200
               :headers {"Content-Type" "text/html"}
               :body (if-some [description (get request-errors (:type params))]
                       description
                       (case (:type params)
                         "unsupported_grant_type" "Grant type specified isn't supported"
                         "corrupt_session" "Session id wasn't provided by access server"
                         "missing_response_type" "Client didn't specify response_type"
                         "client_not_registered" "Client is not registered"
                         "missing_redirect" "Client authorization request didn't specify response_type"
                         "redirect_missmatch" "Couldn't match requested redirect to any of configured redirects for client"
                         "no_redirections" "Client doesn't has 0 configured redirections"
                         (str "Server error - " (:type params))))})))})




(let [gen-device-code (nano-id/custom "ACDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz" 20)
      gen-user-code (nano-id/custom "0123456789" 6)]
  (defn device-code-response
    [request]
    (log/debugf "Device code request:\n%s" request)
    (letfn [(split-spaces [request k]
              (if-some [val (get request k)]
                (assoc request k (set (str/split val #"\s+")))
                request))]
      (let [{:keys [response_type redirect_uri] :as request}
            (-> request
                (split-spaces :scope)
                (split-spaces :response_type))
            device-code (gen-device-code)
            user-code (gen-user-code)]
        ;; Treba checkirati klijenta
        ;; dohvatiti konfiguraciju za klijenta
        {:device_code device-code
         :user_code user-code
         :verification_uri (domain+ "/oauth/activate-device")
         :verification_uri_complete (domain+ (str "/oauth/activate-device?user_code=" user-code))
         :interval 5
         :expires_in 900}))))


(def device-code-flow-interceptor
  {:name ::device-code-flow
   :enter
   (fn [{{:keys [params]} :request :as context}]
     (chain/terminate (assoc context :response (device-code-response params))))})


(def device-confirm-interceptor
  {:name ::device-confirm
   :enter (fn [{{:keys [params]} :request :as context}]
            (chain/terminate (assoc context :response (device-code-response params))))})


(def token-interceptor
  {:name ::token
   :enter
   (fn [{request :request :as context}]
     (chain/terminate
       (assoc context :response (token-endpoint (:params request)))))})


(defn- decode-base64-credentials
  [data]
  (when data
    (let [decoded-bytes (.decode (Base64/getDecoder) (.getBytes data "UTF-8"))
          decoded (String. decoded-bytes)]
      (str/split decoded #":" 2))))


(def basic-authorization-interceptor
  {:name ::authentication-basic
   :enter
   (fn [{{{authorization "authorization"} :headers} :request :as context}]
     (if-not authorization context
       (let [[_ credentials] (re-find #"Basic\s+(.*)" authorization)
             [id secret] (decode-base64-credentials credentials)]
         (update-in context [:request :params] assoc
                    :client_id id
                    :client_secret secret))))})


(def keywordize-params
  {:name ::keywordize-params
   :enter
   (fn [ctx]
     (update-in ctx [:request :params] keywordize-keys))})


(def scope->set
  {:name ::keywordize-params
   :enter
   (fn [ctx]
     (update-in ctx [:request :params :scope] (fn [scope] (when scope (set (str/split scope #"\s+"))))))})


(def redirect-to-login
  {:enter (fn [ctx]
            (chain/terminate
              (assoc ctx :response
                     {:status 302
                      :headers {"Location" (str "/oidc/login/index.html")
                                "Cache-Control" "no-cache"}})))})


(comment
  (def session "VlIeFLuNlzYvxpbOhXtoPAjJYXiphV")
  (def username "oauth_test")
  (def password "change-me"))

(defn login
  [{:keys [username password session]}]
  (let [{:keys [code] :as session-state} (get-session session)
        {response_type :response_type
         redirect-uri :redirect_uri
         :keys [state]} (get-code-request code)
        resource-owner (validate-resource-owner username password)]
    (letfn [(attach-session-cookie [ctx]
              (assoc-in ctx [:cookies "idsrv.session"]
                        {:value session
                         :path "/"
                         :http-only true
                         :secure true
                         :expires "Session"}))]
      (cond
        ;;
        ;;
        (nil? session-state)
        (handle-request-error {:type "corrupt_session" :session session}) 
        ;;
        (and resource-owner (set/intersection #{"code" "authorization_code"} response_type))
        (do
          (log/debugf "[%s] Binding code to session" code)
          (set-session-resource-owner session resource-owner)
          (set-session-authorized-at session (vura/date))
          (mark-code-issued code)
          (log/debugf "[%s] Validating resource owner: %s" session username)
          (attach-session-cookie
            {:status 302
             :headers {"Location" (str redirect-uri "?" (codec/form-encode {:state state :code code}))}}))
        ;;
        :else
        (let [{{url "login-page"} :settings} (get-session-client session)]
          {:status 302
           :headers {"Location" (str url "?" (codec/form-encode
                                               {:session session
                                                :error ["credentials"]}))
                     "Cache-Control" "no-cache"}})))))


(def login-interceptor
  {:name ::login
   :enter
   (fn [{{params :params :as request} :request :as context}]
     (let [{:keys [form-params]} (bp/form-parser request)
           data (merge params form-params)]
       (chain/terminate
         (assoc context :response (login data)))))})


(def routes
  #{["/oauth/authorize"
     :get [basic-authorization-interceptor
           (bp/body-params)
           keywordize-params
           scope->set
           authorize-request-interceptor]
     :route-name ::authorize-request]
    ["/oauth/request_error"
     :get [basic-authorization-interceptor
           (bp/body-params)
           keywordize-params
           scope->set
           authorize-request-error-interceptor]
     :route-name ::authorize-request-error]
    ["/oauth/token"
     :post [basic-authorization-interceptor
            (bp/body-params)
            keywordize-params
            scope->set
            token-interceptor]
     :route-name ::handle-token]
    ["/oidc/login" :get [redirect-to-login] :route-name ::short-login]
    ["/oidc/login/" :get [redirect-to-login] :route-name ::root-login]
    ["/oidc/login/*" :get [spa-interceptor] :route-name ::serve-login]
    ["/oidc/login/*" :post [login-interceptor] :route-name ::handle-login]})



(defn expired? [token]
  (try
    (unsign-data token)
    false
    (catch clojure.lang.ExceptionInfo ex
      (let [{:keys [cause]} (ex-data ex)]
        (= cause :exp)))))


(defn reset
  []
  (reset! *sessions* nil)
  (reset! *resource-owners* nil)
  (reset! *clients* nil)
  (reset! *tokens* nil)
  (reset! *authorization-codes* nil))


;; Maintenance
(defn clean-sessions
  "Function will clean all sessions that were issued
  authorization code, but code wasn't used for more
  than timeout value. Default timeout is 1 minute"
  ([] (clean-sessions (vura/minutes 1)))
  ([timeout]
   (let [now (System/currentTimeMillis)]
     (letfn [(scan? [at]
               (pos? (- now (+ at timeout))))]
       (doseq [[session {:keys [code at tokens]}] @*sessions*
               :when (scan? at)]
         (cond
           ;; Authorization code wasn't used
           (and (some? code) (empty? tokens))
           (do
             (log/debugf "[%s] Session timed out. No code or access token was assigned to this session" session)
             (kill-session session))
           :else
           nil))))))


(defn clean-codes
  "Function will remove all codes that are older than 'timeout'
  value. Default timeout = 5 minutes"
  ([] (clean-codes (vura/minute 5)))
  ([timeout]
   (let [now (System/currentTimeMillis)]
     (swap! *authorization-codes*
            (fn [codes]
              (reduce-kv
                (fn [result code {:keys [at]}]
                  (if (pos? (- now (+ at timeout)))
                    (dissoc result code)
                    result))
                codes
                codes))))))


(defonce maintenance-agent (agent {:running true :period (vura/seconds 30)}))


(defn maintenance
  [{:keys [running period] :as data}]
  (when running
    (log/debug "OAuth2 maintenance start")
    (send-off *agent* maintenance)
    (clean-sessions (vura/minutes 1))
    (clean-codes (vura/minutes 1))
    (log/debug "OAuth2 maintenance finish")
    (Thread/sleep period)
    data))


(defn start-maintenance
  []
  (send-off maintenance-agent maintenance))



(comment
  (clean-codes)
  (java.util.Date. 1720693899)
  (def token (-> *tokens* deref :access_token ffirst))
  (reset)
  (require '[buddy.sign.jwt :as jwt])
  (jwt/decode-header token)
  (reset))
