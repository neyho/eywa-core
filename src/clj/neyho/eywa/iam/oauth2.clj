(ns neyho.eywa.iam.oauth2
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
    [io.pedestal.interceptor.chain :as chain]
    [io.pedestal.http.body-params :as bp]
    [io.pedestal.http.ring-middlewares :as middleware]
    [neyho.eywa.iam
     :refer [sign-data
             unsign-data
             get-client
             validate-password
             get-user-details]]
    [neyho.eywa.server.interceptors :refer [spa-interceptor]])
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


(defonce ^:dynamic *resource-owners* (atom nil))
(defonce ^:dynamic *clients* (atom nil))
(defonce ^:dynamic *sessions* (atom nil))


(let [alphabet "ACDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"]
  (def gen-session-id (nano-id/custom alphabet 30)))


(let [alphabet "ACDEFGHJKLMNOPQRSTUVWXYZ"]
  (def gen-code (nano-id/custom alphabet 30)))


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
  (log/debugf "Returning error: %s\n%s" code (str/join "\n" description))
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


(defn validate-client [session]
  (let [{{:keys [client_id state redirect_uri]
          request-secret :client_secret} :request} (get @*sessions* session)
        base-redirect-uri (get-base-uri redirect_uri)
        {:keys [euuid secret type]
         {redirections "redirections"} :settings
         :as client} (get-client client_id)]
    (log/debugf "[%s] Validating client: %s" session (pprint client))
    (cond
      (nil? euuid)
      (throw
        (ex-info
          "Client not regiestered"
          {:type "client_not_registered"
           :session session}))
      ;;
      (empty? redirections)
      (throw
        (ex-info
          "Client missing redirections"
          {:type "no_redirections"
           :session session}))
      ;;
      (empty? redirect_uri)
      (throw
        (ex-info
          "Client hasn't provided redirect URI"
          {:type "missing_redirect"
           :session session}))
      ;;
      (not-any? #(= base-redirect-uri %) redirections)
      (throw
        (ex-info
          "Client provided uri doesn't match available redirect URI(s)"
          {:type "redirect_missmatch"
           :session session}))
      ;;
      (or (some? request-secret) (some? secret))
      (if (validate-password request-secret secret)
        client
        (throw
          (ex-info
            "Client secret missmatch"
            {:type "access_denied"
             :session session
             :state state})))
      ;;
      (and (= type "public") (nil? secret))
      client
      ;;
      :else
      (do
        (log/errorf "[%s] Couldn't validate client" session)
        (throw
          (ex-info "Unknown client error"
                   {:session session
                    :type "server_error"}))))))


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
                                     state (assoc :state state))))
                 "Cache-Control" "no-cache"}})))


(defn bind-authorization-code
  [session]
  (let [code (gen-code)]
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


;; Login
(defn login
  [{:keys [username password session]}]
  (let [{{response_type :response_type
          redirect-uri :redirect_uri
          state :state} :request
         :as session-state} (get-session session)
        resource-owner (validate-resource-owner username password)]
    (set-session-authorized-at session (vura/date))
    (log/debugf "[%s] Validating resource owner: %s" session username)
    (letfn [(attach-session-cookie [ctx]
              (assoc-in ctx [:cookies "idsrv.session"]
                        {:value session
                         :path "/"
                         :http-only true
                         :secure true
                         :expires "Session"}))]
      (cond
        ;;
        (nil? session-state)
        (handle-request-error {:type "corrupt_session" :session session}) 
        ;;
        (and resource-owner (set/intersection #{"code" "authorization_code"} response_type))
        (let [code (bind-authorization-code session)]
          (log/debugf "[%s] Binding code to session" code)
          (set-session-resource-owner session resource-owner)
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


(defn set-session-tokens
  [session tokens]
  (swap! *sessions* assoc-in [session :tokens] tokens)
  (swap! *tokens*
         (fn [current-tokens]
           (reduce-kv
             (fn [tokens token-key data]
               (log/debugf "[%s] Adding token %s %s" session token-key data)
               (assoc-in tokens [token-key data] session))
             current-tokens
             tokens)))
  nil)


(defn get-token-session
  [token-key token]
  (get-in @*tokens* [token-key token]))


(defn get-session-code
  [session]
  (get-in @*sessions* [session :code]))


(defn session-used-authorization-code [session]
  (swap! *sessions* assoc-in [session :authorization-code-used?] true))


(defn code-is-used? [session]
  (get-in @*sessions* [session :authorization-code-used?]))


(defn revoke-authorization-code
  [session]
  (let [{:keys [code]} (get-session session)]
    (swap! *sessions* update session
           (fn [current]
             (dissoc current :code)))
    (when code
      (swap! *authorization-codes* dissoc code))
    (publish :revoke/code {:code code :session session})))


(defn revoke-token
  [session token-key]
  (let [{{token token-key} :tokens} (get-session session)]
    (swap! *sessions* update-in [session :tokens] dissoc token-key)
    (when token
      (swap! *tokens* update token-key dissoc token)
      (publish :revoke/token
               {:token/key token-key
                :token/data token
                :session session})))
  nil)


(defn revoke-session-tokens
  [session]
  (let [{:keys [tokens]} (get-session session)]
    (doseq [[token-key] tokens]
      (revoke-token session token-key)))
  nil)


(defn kill-session
  [session]
  (let [session-data (get-session session)]
    (remove-session-resource-owner session)
    (remove-session-client session)
    (revoke-session-tokens session)
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
  [session _ data]
  (let [client (get-session-client session)]
    (sign-data
      (assoc data
             :exp (-> (vura/date)
                      vura/date->value
                      (+ (refresh-token-expiry client))
                      vura/value->date
                      to-timestamp))
      {:alg :rs256})))


(defmethod sign-token :access_token
  [session _ data]
  (let [client (get-session-client session)]
    (sign-data
      (assoc data
             :exp (-> (vura/date)
                      vura/date->value
                      (+ (access-token-expiry client))
                      vura/value->date
                      to-timestamp))
      {:alg :rs256
       :kid "evo neki kid"})))


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


(let [unsupported (json-error 500 "unsupported" "This feature isn't supported at the moment")]
  (def ^:dynamic *token-resolver*
    (fn gen-token
      ([session request]
       (try
         (let [{:keys [client_id grant_type code]
                request-scope :scope} request
               ;; use scope from authorization request
               {{authorization-code-scope :scope} :request} (get-session session)
               ;;
               scope (or authorization-code-scope request-scope)
               {{refresh? "refresh-tokens"} :settings :as client
                id :id} (get-session-client session)
               {:keys [euuid active]} (get-session-resource-owner session)
               expires-after (access-token-expiry client)
               grant-type (let [spec (s/conform ::grant_type grant_type)]
                            (if (s/invalid? spec) ::error
                              (first spec)))]
           (when session (revoke-session-tokens session))
           (cond
             ;;
             (not= id client_id)
             (token-error
               "unauthorized_client"
               "Refresh token that you have provided"
               "doesn't belong to given client")
             ;;
             (not active)
             (do
               (kill-session session)
               (token-error
                 "resource_owner_unauthorized"
                 "Provided refresh token doesn't have active user"))
             :else
             (case grant-type
               ;;
               (:password :implicit :client_credentials) 
               unsupported
               ;;
               :refresh-token
               (cond
                 (not refresh?)
                 (token-error
                   "invalid_request"
                   "The client configuration does not support"
                   "token refresh requests.")
                 :else
                 (let [access-token {:session session
                                     :iss *iss*
                                     :sub euuid
                                     :iat (-> (vura/date) to-timestamp)
                                     :client_id client_id
                                     :sid session
                                     :scope (str/join " " scope)}
                       response (if (pos? expires-after)
                                  (let [refresh-token {:iss *iss*
                                                       :sid session}
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
                                    (revoke-session-tokens session)
                                    (when session (set-session-tokens session signed-tokens))
                                    (assoc signed-tokens
                                           :type "Bearer"
                                           :expires_in (:exp access-token)
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
                                           :expires_in nil
                                           :scope (str/join " " scope))))]
                   {:status 200
                    :headers {"Content-Type" "application/json;charset=UTF-8"
                              "Pragma" "no-cache"
                              "Cache-Control" "no-store"}
                    :body (json/write-str response)}))
               ;;
               :authorization-code 
               (let [access-token {:session session
                                   :iss *iss*
                                   :sub euuid
                                   :iat (-> (vura/date) to-timestamp)
                                   :client_id client_id
                                   :sid session
                                   :scope (str/join " " scope)}
                     response (if (pos? expires-after)
                                (let [refresh-token (when (and refresh? session)
                                                      (log/debugf "Creating refresh token: %s" session)
                                                      {:iss *iss*
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
                                  (when refresh-token (revoke-token session :refresh_token))
                                  (when session (set-session-tokens session signed-tokens))
                                  (session-used-authorization-code session)
                                  (assoc signed-tokens
                                         :type "Bearer"
                                         :scope (str/join " " scope)
                                         :expires_in (:exp access-token)))
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
                                  (session-used-authorization-code session)
                                  (assoc signed-tokens
                                         :expires_in nil
                                         :scope (str/join " " scope)
                                         :type "Bearer")))]
                 {:status 200
                  :headers {"Content-Type" "application/json;charset=UTF-8"
                            "Pragma" "no-cache"
                            "Cache-Control" "no-store"}
                  :body (json/write-str response)}))))
         (catch Throwable ex
           (log/errorf ex "[%s] Couldn't resolve tokens" session)
           (throw ex)))))))


(defn get-code-session
  [code]
  (get-in @*authorization-codes* [code :session]))


(defn token-code-grant
  [data]
  (let [{:keys [code redirect_uri client_id client_secret grant_type]} data
        {:keys [session]} (get @*authorization-codes* code)]
    (log/debugf
      "[%s] Processing token code grant for code: %s\n%s"
      session code (pprint data))
    (if-not session
      ;; If session isn't available, that is if somebody
      ;; is trying to hack in
      (token-error
        "invalid_request"
        "Trying to abuse token endpoint for code that"
        "doesn't exsist or has expired. Further actions"
        "will be logged and processed")
      ;; If there is some session than check other requirements
      (let [{{session-redirect-uri :redirect_uri} :request} (get-session session)
            {_secret :secret
             {:strs [allowed-grants]} :settings
             session-client :id} (get-session-client session)
            grants (set allowed-grants)]
        (cond
          ;;
          (code-is-used? session)
          (token-error
            "invalid_request"
            "Provided authorization code has already"
            "been used. Your request will be logged"
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
          (not= session-redirect-uri redirect_uri)
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
          ;; Issue that token
          :else
          (*token-resolver* session data))))))


(defn token-password-grant
  [data]
  (let [{:keys [password username client_id client_secret]} data
        {_client-secret :secret
         {client-type "type"
          grants "allowed-grants"} :settings
         :as client} (get-client client_id)
        grants (set grants)]
    (cond
      (empty? client_id)
      (token-error
        "invalid_client"
        "Client ID wasn't provided. Are you mad?")
      ;;
      (empty? client)
      (token-error
        "invalid_client"
        "Client not registered")
      ;;
      (and
        (= client-type "confidential")
        (empty? client_secret))
      (token-error
        "invalid_client"
        "Client is configured as confidential."
        "Secret wasn't provided for client")
      ;;
      (and
        _client-secret
        (not (validate-password client_secret _client-secret)))
      (token-error
        "invalid_client"
        "Client secret is not correct"
        "Your request is logged and will"
        "processed")
      ;;
      (not (contains? grants "password"))
      (token-error
        "unauthorized_grant"
        "Client sent access token request"
        "for grant type that is outside"
        "of client configured privileges")

      ;;
      (empty? username)
      (token-error
        "invalid_request"
        "username wasn't provided. Are you mad?")
      ;;
      (empty? password)
      (token-error
        "invalid_request"
        "password wasn't provided. Are you mad?")
      ;;
      :else
      (let [resource-owner-details (validate-resource-owner username password)]
        (if (not resource-owner-details)
          (token-error
            "resource_owner_unauthorized"
            "Resource owner credentials are wrong")
          ;; Use refresh tokens to manage session
          ;; as long as refresh_tokens are used, i can still manage
          ;; session, and not have trillions of access tokens for single user
          ;; Now what if somebody is abusing or if somebody is programming 
          ;; client wrong and doesn't use refresh_token... What than?
          ;; Should i even track access tokens and refresh tokens in sessions
          ;; or what
          (let [now (System/currentTimeMillis)
                session (gen-session-id)]
            (set-session session
                         {:request (dissoc data :secret)
                          :at now})
            (set-session-client session client)
            (set-session-resource-owner session resource-owner-details)
            (*token-resolver* session data)))))))


(defn token-refresh-grant
  [{:keys [refresh_token] :as data}]
  (let [session (get-in @*tokens* [:refresh_token refresh_token])]
    (*token-resolver* session data)))


(defn token-client-credentials-grant
  [{:keys [client_id client_secret] :as data}]
  (if (empty? client_secret)
    (token-error
      "unauthorized_client"
      "Client secret is empty. You are not God")
    (let [{{grants "allowed-grants"} :settings
           _secret :secret} (get-client client_id)
          grants (set grants)]
      (cond
        ;;
        (not (contains? grants "client_credentials"))
        (token-error
          "invalid_client"
          "Client sent access token request"
          "for grant type that is outside"
          "of client configured privileges")
        ;;
        (not (validate-password client_secret _secret))
        (token-error
          "invalid_client"
          "Client credentials are wrong")
        :else
        (*token-resolver* nil data)))))


(defn token-endpoint
  [{:keys [grant_type] :as data}]
  (log/debugf "Received token endpoint request\n%s" (pprint data))
  (case grant_type
    ;; Authorization code grant
    "authorization_code"
    (token-code-grant data)
    ;; Resource Owner Password Credentials Grant
    "password"
    (token-password-grant data)
    ;; 
    "refresh_token"
    (token-refresh-grant data)
    ;; Client credentials grant
    "client_credentials"
    (token-client-credentials-grant data)
    ;;else
    (handle-request-error
      {:type "unsupported_grant_type"
       :grant_type grant_type})))


(defn authorization-code-flow
  [{cookie-session :idsrv/session :as request}]
  (let [session (or cookie-session (gen-session-id))
        now (System/currentTimeMillis)]
    (comment
      (def session (:idsrv/session request)))
    (try
      ;; If session exists, trust that session
      (if cookie-session
        (do
          ;; and revoke current tokens, since new code will
          ;; be published that will create new tokens
          (revoke-session-tokens session)
          ;; and merge current request into session as active request
          (swap! *sessions* update session merge {:request request :at now}))
        ;; when there is no cookie session, set session request for
        ;; further processing
        (set-session session {:request request :at now}))
      ;; Check that client is valid
      (let [client (validate-client session)]
        ;; Proper implementation
        (set-session-client session client)
        (let [{{url "login-page"} :settings} (get-session-client session)]
          {:status 302
           :headers {"Location" (str url "?" (codec/form-encode {:session session}))
                     "Cache-Control" "no-cache"}}))
      (catch clojure.lang.ExceptionInfo ex
        (handle-request-error (ex-data ex))))))


(defn authorization-request
  [{:keys [response_type redirect_uri]
    :as request}]
  (log/debugf "Authorizing request:\n%s" request)
  (case response_type
    ;;
    "code"
    (authorization-code-flow request)
    ;; TODO - check if this is valid
    ("token")
    (token-password-grant request)
    ;;
    "password"
    (token-password-grant request)
    ;;
    "client_credentials"
    (token-client-credentials-grant request) 
    ;;
    (cond
      ;;
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
      :else
      (handle-request-error
        {:type "server_error"
         :request request}))))


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


(def routes
  #{["/oauth2/authorize"
     :get [basic-authorization-interceptor
           (bp/body-params)
           keywordize-params
           scope->set
           authorize-request-interceptor]
     :route-name ::authorize-request]
    ["/oauth2/request_error"
     :get [basic-authorization-interceptor
           (bp/body-params)
           keywordize-params
           scope->set
           authorize-request-error-interceptor]
     :route-name ::authorize-request-error]
    ["/oauth2/token"
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
  ([] (clean-sessions (vura/minutes 1)))
  ([timeout]
   (let [now (System/currentTimeMillis)]
     (letfn [(scan? [at]
               (pos? (- now (+ at timeout))))]
       (doseq [[session {:keys [code at access-token refresh-token]}] @*sessions*
               :when (scan? at)]
         (cond
           ;; Not assigned
           (and (nil? code) (nil? access-token))
           (do
             (log/debugf "[%s] Session timed out. No code or access token was assigned to this session" session)
             (kill-session session))
           ;; access token expired
           (and (some? access-token) (nil? refresh-token) (expired? access-token))
           (do
             (log/debugf "[%s] Access token expired and no refresh token is available. Killing session" session)
             (kill-session session))
           ;; refresh token expired
           (and (some? refresh-token) (expired? access-token))
           (do
             (log/debugf "[%s] Refresh token expired. Killing session" session)
             (kill-session session))
           :else
           nil))))))


(defonce maintenance-agent (agent {:running true :period (vura/seconds 30)}))


(defn maintenance
  [{:keys [running period] :as data}]
  (when running
    (log/debug "OAuth2 maintenance start")
    (send-off *agent* maintenance)
    (clean-sessions (vura/minutes 1))
    (log/debug "OAuth2 maintenance finish")
    (Thread/sleep period)
    data))


(defn start-maintenance
  []
  (send-off maintenance-agent maintenance))



(comment
  (def token (-> *tokens* deref :access_token ffirst))
  (require '[buddy.sign.jwt :as jwt])
  (jwt/decode-header token)
  (reset))
