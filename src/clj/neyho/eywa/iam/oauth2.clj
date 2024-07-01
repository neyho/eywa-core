(ns neyho.eywa.iam.oauth2
  (:require
    [clojure.string :as str]
    clojure.java.io
    clojure.pprint
    [clojure.set :as set]
    [clojure.core.async :as async]
    [clojure.tools.logging :as log]
    [clojure.walk :refer [keywordize-keys]]
    [nano-id.core :refer [nano-id] :as nano-id]
    [vura.core :as vura]
    [ring.util.codec :as codec]
    [clojure.data.json :as json]
    [buddy.sign.util :refer [to-timestamp]]
    [io.pedestal.interceptor.chain :as chain]
    [io.pedestal.http.body-params :as bp]
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


(defonce ^:dynamic *refresh-tokens* (atom nil))
(defonce ^:dynamic *access-tokens* (atom nil))
(defonce ^:dynamic *authorization-codes* (atom nil))


(defonce ^:dynamic *resource-owners* (atom nil))
(defonce ^:dynamic *clients* (atom nil))
(defonce ^:dynamic *sessions* (atom nil))


(let [alphabet "ACDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"]
  (def gen-session-id (nano-id/custom alphabet 30)))


(let [alphabet "ACDEFGHJKLMNOPQRSTUVWXYZ"]
  (def gen-code (nano-id/custom alphabet 30)))


(defn access-token-expiry
  [{{{expiry "access"} "token-expiry"} :settings
    :or {expiry (vura/day 1.5)}}]
  expiry)


(defn refresh-token-expiry
  [{{{expiry "refresh"} "token-expiry"} :settings
    :or {expiry (vura/day 1.5)}}]
  expiry)


(declare revoke-access-token revoke-refresh-token remove-session-tokens set-session-tokens)


(def ^:dynamic *token-resolver*
  (fn gen-access-token
    ([resource-owner-details client] (gen-access-token resource-owner-details client nil))
    ([resource-owner-details {{refresh? "refresh-tokens"} :settings :as client} session]
     (let [expires-after (access-token-expiry client)]
       (when session (remove-session-tokens session))
       (if (pos? expires-after)
         (let [access-token (sign-data
                              (->
                                resource-owner-details 
                                (dissoc :password :avatar :settings :active :sessions)
                                (assoc :session session))
                              {:alg :rs256
                               :exp (-> (vura/date)
                                        vura/date->value
                                        (+ expires-after)
                                        vura/value->date
                                        to-timestamp)})
               refresh-token (when (and refresh? session)
                               (sign-data
                                 {:session session}
                                 {:alg :rs256
                                  :exp (-> (vura/date)
                                           vura/date->value
                                           (+ (refresh-token-expiry client))
                                           vura/value->date
                                           to-timestamp)}))
               tokens (if refresh-token
                        {:access_token access-token
                         :refresh_token refresh-token
                         :expires_in expires-after
                         :type "bearer"}
                        {:access_token access-token
                         :expires_in expires-after
                         :type "bearer"})]
           (when session (set-session-tokens session tokens))
           tokens)
         {:access_token (sign-data
                          (dissoc resource-owner-details
                                  :password :avatar :settings
                                  :active :sessions)
                          {:alg :rs256})
          :expires_in nil
          :type "bearer"})))))


(defn get-base-uri
  "Returns the base URI without query parameters from the given URL."
  [url]
  (when (not-empty url)
    (let [uri (java.net.URI. url)]
      (str (java.net.URI. (.getScheme uri) (.getAuthority uri) (.getPath uri) nil nil)))))


(defn validate-client [session]
  (let [{{:keys [client_id state redirect_uri]
          request-secret :client_secret
          :as request} :request} (get @*sessions* session)
        base-redirect-uri (get-base-uri redirect_uri)
        {:keys [euuid secret type]
         {redirections "redirections"} :settings
         :as client} (get-client client_id)]
    (log/debugf "[%s] Validating client: %s" session (pprint client))
    (comment
      (def client (get-client "HJIUVTENYXXOKEMGYXEUEHIKVKCKJPCNCLSXLSKNMFVEMAWJ")))
    ; (def request request)
    ; (def euuid euuid)
    ; (def redirections redirections)
    ; (def redirect_uri redirect_uri)
    ; (def base-redirect-uri base-redirect-uri)
    ; (def redirections redirections)
    ; (def request-secret request-secret)
    ; (def secret secret)
    ; (def secret secret)
    ; (def client client)
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


(defn get-session-resource-owner [session]
  (let [euuid (get-in @*sessions* [session :resource-owner])]
    (get @*resource-owners* euuid)))


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



(defn get-session-client [session]
  (let [euuid (get-in @*sessions* [session :client])]
    (get @*clients* euuid)))


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
             (assoc current :code code)))
    (publish :grant/code {:session session :code code})
    code))


;; Login
(defn login
  [{:keys [username password session]}]
  (let [{{response_type :response_type
          redirect-uri :redirect_uri
          state :state} :request
         :as session-state} (get-session session)
        resource-owner (validate-resource-owner username password)]
    (log/debugf "[%s] Validating resource owner: %s" session username)
    (cond
      ;;
      (nil? session-state)
      (handle-request-error {:type "corrupt_session" :session session}) 
      ;;
      (and resource-owner (set/intersection #{"code" "authorization_code"} response_type))
      (let [code (bind-authorization-code session)]
        (log/debugf "[%s] Binding code to session" code)
        (set-session-resource-owner session resource-owner)
        {:status 302
         :headers {"Location" (str redirect-uri "?" (codec/form-encode {:state state :code code}))}})
      ;;
      :else
      (let [{{url "login-page"} :settings} (get-session-client session)]
        {:status 302
         :headers {"Location" (str url "?" (codec/form-encode
                                             {:session session
                                              :error ["credentials"]}))
                   "Cache-Control" "no-cache"}}))))


(def login-interceptor
  {:name ::login
   :enter
   (fn [{{params :params :as request} :request :as context}]
     (let [{:keys [form-params]} (bp/form-parser request)
           data (merge params form-params)]
       (chain/terminate
         (assoc context :response (login data)))))})


(defn set-session-tokens
  [session {:keys [access_token refresh_token]}]
  (swap! *sessions* update session
         (fn [current]
           (cond-> current
             access_token (assoc :access-token access_token)
             refresh_token (assoc :refresh-token refresh_token))))
  (swap! *access-tokens* assoc access_token {:session session})
  (swap! *refresh-tokens* assoc refresh_token {:session session})
  nil)


(defn revoke-authorization-code
  [session]
  (let [{:keys [code]} (get-session session)]
    (swap! *sessions* update session
           (fn [current]
             (dissoc current :code)))
    (when code
      (swap! *authorization-codes* dissoc code))
    (publish :revoke/code {:code code :session session})))


(defn revoke-access-token
  [session]
  (let [{:keys [access-token]} (get-session session)]
    (swap! *sessions* update session dissoc :access-token)
    (when access-token
      (swap! *access-tokens* dissoc access-token)
      (publish :revoke/access-token
               {:access-token access-token
                :session session})))
  nil)


(defn revoke-refresh-token
  [session]
  (let [{:keys [refresh-token]} (get-session session)]
    (swap! *sessions* update session dissoc :refresh-token)
    (when refresh-token
      (swap! *refresh-tokens* dissoc refresh-token)
      (publish :revoke/refresh-token
               {:refresh-token refresh-token
                :session session}))))


(defn remove-session-tokens
  [session]
  (revoke-access-token session)
  (revoke-refresh-token session)
  nil)


(defn token-error [code & description]
  (log/debugf "Returning error: %s\n%s" code (str/join "\n" description))
  {:status 400
   :headers {"Content-Type" "application/json;charset=UTF-8"
             "Pragma" "no-cache"
             "Cache-Control" "no-store"}
   :body (json/write-str
           {:error code
            :error_description (str/join "\n" description)})})

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
        "be logged and processed")
      ;; If there is some session than check other requirements
      (let [{{session-redirect-uri :redirect_uri} :request} (get-session session)
            {_secret :secret
             {:strs [allowed-scopes allowed-grants]} :settings
             session-client :id
             :as client} (get-session-client session)
            scopes (set allowed-scopes)
            grants (set allowed-grants)]
        (cond
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
          (let [resource-owner (get-session-resource-owner session)
                resource-owner-details (get-user-details (:name resource-owner))
                tokens (*token-resolver* resource-owner-details client session)]
            (log/debugf "[%s] Authorization code flow token request is valid" session)
            (revoke-authorization-code session)
            (publish :grant/tokens tokens)
            {:status 200
             :headers {"Content-Type" "application/json;charset=UTF-8"
                       "Pragma" "no-cache"
                       "Cache-Control" "no-store"}
             :body (json/write-str tokens)}))))))


(defn token-password-grant
  [data]
  (let [{:keys [password username scope client_id client_secret]} data
        {_client-secret :secret
         {client-type "type"
          refresh? "refresh-tokens"
          grants "allowed-grants"
          scopes "allowed-scopes"} :settings
         :as client} (get-client client_id)
        scopes (set scopes)
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
        (cond
          (not resource-owner-details)
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
          refresh?
          (let [now (System/currentTimeMillis)
                session (gen-session-id)]
            (set-session session
                         {:request (dissoc data :password)
                          :at now})
            (set-session-client session client)
            (set-session-resource-owner session resource-owner-details)
            (let [tokens (*token-resolver* resource-owner-details client session)]
              (publish :grant/tokens tokens)
              {:status 200
               :headers {"Content-Type" "application/json;charset=UTF-8"
                         "Pragma" "no-cache"
                         "Cache-Control" "no-store"}
               :body (json/write-str tokens)}))
          ;;
          :else
          {:status 200
           :headers {"Content-Type" "application/json;charset=UTF-8"
                     "Pragma" "no-cache"
                     "Cache-Control" "no-store"}
           :body (let [access-token (*token-resolver* resource-owner-details client nil)]
                   (publish :grant/access-token {:access-token access-token})
                   (json/write-str
                     {:access_token access-token
                      :expires_in (access-token-expiry client)}))})))))


(defn kill-session
  [session]
  (let [session-data (get-session session)]
    (remove-session-resource-owner session)
    (remove-session-client session)
    (remove-session-tokens session)
    (swap! *sessions* dissoc session)
    (publish :session/kill
             {:session session
              :data session-data})))


(defn token-refresh-grant
  [{:keys [refresh_token client_id]}]
  (let [{:keys [session]} (get @*refresh-tokens* refresh_token)
        {:keys [name] :as client} (get-session-client session)
        {:keys [username]} (get-session-resource-owner session)
        {:keys [active] :as resource-owner-details} (get-user-details username)]
    (cond
      (not= name client_id)
      (token-error
        "unauthorized_client"
        "Refresh token that you have provided"
        "doesn't belong to given client")
      (not active)
      (do
        (kill-session session)
        (token-error
          "resource_owner_unauthorized"
          "Provided refresh token doesn't have active user"))
      :else
      (let [tokens (*token-resolver* resource-owner-details client session)]
        (publish :grant/tokens tokens)
        {:status 200
         :headers {"Content-Type" "application/json;charset=UTF-8"
                   "Pragma" "no-cache"
                   "Cache-Control" "no-store"}
         :body (json/write-str tokens)}))))


(defn token-client-credentials-grant
  [{:keys [client_id client_secret]}]
  (if (empty? client_secret)
    (token-error
      "unauthorized_client"
      "Client secret is empty. You are not God")
    (let [{{grants "allowed-grants"} :settings
           _secret :secret :as client} (get-client client_id)
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
        (let [tokens (*token-resolver* {:client client_id} client)]
          (publish :grant/grant tokens)
          {:status 200
           :headers {"Content-Type" "application/json;charset=UTF-8"
                     "Pragma" "no-cache"
                     "Cache-Control" "no-store"}
           :body (json/write-str tokens)})))))


(defn token-endpoint
  [{:keys [grant_type] :as data}]
  (log/debugf "[%s] Resolving request at token endpoint" "ioqjw")
  (case grant_type
    ;; Authorization code grant
    ("code" "authorization_code")
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
  [request]
  (let [session (gen-session-id)
        now (System/currentTimeMillis)]
    (try
      (set-session session
                   {:request request
                    :at now})
      (let [client (validate-client session)]
        ;; Proper implementation
        (set-session-client session client)
        (let [{{url "login-page"} :settings} (get-session-client session)]
          {:status 302
           :headers {"Location" (str url "?" (codec/form-encode {:session session}))
                     "Cache-Control" "no-cache"}}))
      (catch clojure.lang.ExceptionInfo ex
        (handle-request-error (ex-data ex))))))


(comment
  (def request {:response_type "code", :client_id "XFYWDCONOFSZMTVAEOQHTZFHSUCTXQ", :redirect_uri "http://localhost:8080/eywa/", :scope nil})
  (authorization-request request))


(defn authorization-request
  [{:keys [response_type username password redirect_uri]
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
     ; (def context context)
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
                      :headers {"Location" (str "/login/index.html")
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
    ["/login" :get [redirect-to-login] :route-name ::short-login]
    ["/login/" :get [redirect-to-login] :route-name ::root-login]
    ["/login/*" :get [spa-interceptor] :route-name ::serve-login]
    ["/login/*" :post [login-interceptor] :route-name ::handle-login]})


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
  (reset! *authorization-codes* nil)
  (reset! *refresh-tokens* nil)
  (reset! *access-tokens* nil))


;; Maintenance
(defn clean-sessions
  ([] (clean-sessions (vura/minutes 1)))
  ([timeout]
   (let [now (System/currentTimeMillis)]
     (letfn [(scan? [at]
               (pos? (- now (+ at timeout))))]
       (doseq [[session {:keys [request code at access-token refresh-token]}] @*sessions*
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
