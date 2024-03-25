(ns neyho.eywa.iam
  (:require
    [clojure.string :as str]
    clojure.java.io
    clojure.pprint
    [nano-id.core :refer [nano-id]]
    [vura.core :as vura]
    [ring.util.codec :as codec]
    [clojure.data.json :as json]
    [buddy.sign.jwt :as jwt]
    [buddy.hashers :as hashers]
    [buddy.core.keys :as keys]
    [buddy.sign.util :refer [to-timestamp]]
    [neyho.eywa.iam.oauth2.uuids :as ou]
    [neyho.eywa.iam.uuids :as iu]
    [neyho.eywa.iam.oauth2.client :refer [generate-client]]
    [neyho.eywa.dataset
     :as dataset
     :refer [get-entity
             sync-entity
             delete-entity]]
    [io.pedestal.interceptor.chain :as chain]
    [io.pedestal.http.body-params :as bp]
    [neyho.eywa.server.interceptors :refer [spa-interceptor]])
  (:import
    [java.util Base64]
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
            (->
              (vura/date)
              vura/date->value
              (+ vura/day)
              vura/value->date
              to-timestamp)))
  ([data valid]
   (jwt/sign
     data
     *private-key*
     {:alg :rs256
      :exp valid})))


(defn unsign-data
  "Function takes encrypted string and returns decrypted data."
  [data]
  (jwt/unsign data *public-key* {:alg :rs256}))


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


(comment
  (time (get-client "oauth_test_confidential"))
  (get-client "XFYWDCONOFSZMTVAEOQHTZFHSUCTXQ")
  (sync-entity
    neyho.eywa.iam.uuids/user
    {:name "oauth_test"
     :password "change-me"})
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
      :allowed-grants
      ["refresh_token" "client_credentials" "password" "code"],
      :redirections
      ["http://localhost:8080/eywa/" "http://localhost:8080/app/kbdev"]}})

  (remove-client #uuid "62972fcf-3cfe-4d34-baea-055308612a0d")
  (remove-client #uuid "3349f1ff-2118-4b3e-babf-a8b68b7e98df"))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                OAUTH2                                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;





(defonce ^:dynamic *refresh-tokens* (atom nil))
(defonce ^:dynamic *access-tokens* (atom nil))
(defonce ^:dynamic *authorization-codes* (atom nil))


(defonce ^:dynamic *resource-owners* (atom nil))
(defonce ^:dynamic *clients* (atom nil))
(defonce ^:dynamic *sessions* (atom nil))


(defonce ^:dynamic *access-token-resolver*
  (fn [resource-owner-details client]
    (sign-data
      (dissoc resource-owner-details :password :avatar :settings :active :sessions)
      (-> (vura/date)
          vura/date->value
          (+ (vura/minutes 5))
          vura/value->date
          to-timestamp))))


(defonce ^:dynamic *refresh-token-resolver*
  (fn [data client]
    (sign-data
      data
      (-> (vura/date)
          vura/date->value
          (+ (vura/hours 8))
          vura/value->date
          to-timestamp))))


(defn get-base-uri
  "Returns the base URI without query parameters from the given URL."
  [url]
  (let [uri (java.net.URI. url)]
    (str (java.net.URI. (.getScheme uri) (.getAuthority uri) (.getPath uri) nil nil))))


(defn validate-client [session]
  (let [{{:keys [client_id state redirect_uri]
          request-password :client-password} :request} (get @*sessions* session)
        base-redirect-uri (get-base-uri redirect_uri)
        {:keys [euuid password]
         {type "type"
          redirections "redirections"} :settings
         :as client} (get-client client_id)]
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
      (or (some? request-password) (some? password))
      (if (validate-password request-password password)
        client
        (throw
          (ex-info
            "Client password missmatch"
            {:type "access_denied"
             :session session
             :state state})))
      ;;
      (and (= type "public") (nil? password))
      client
      ;;
      :else
      (throw
        (ex-info "Unknown client error"
                 {:session session
                  :type "server_error"})))))


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
    (swap! @*sessions* update session dissoc :resource-owner)
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
    (swap! @*sessions* update session dissoc :client)
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


(defn- handle-request-error
  [{t :type
    session :session
    :as data}]
  (let [{{:keys [state redirect_uri]} :request} (get-session session)
        base-redirect-uri (get-base-uri redirect_uri)]
    (remove-session session)
    (case t
      ;; When redirection uri error than redirect to 
      ("no_redirections"
        "missing_redirect" "redirect_missmatch" "missing_response_type"
        "client_not_registered" "corrupt_session" "unsupported_grant_type")
      {:status 302
       :headers {"Location" (str "/oauth2/request_error?"
                                 (codec/form-encode data))
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


(defn bad-request
  [data]
  {:status 400
   :headers {"Content-Type" "application/json;charset=UTF-8"
             "Pragma" "no-cache"
             "Cache-Control" "no-store"}
   :body (json/write-str data)})


(defn return-token [session]
  (let []
    ))


(defn authorization-request
  [{:keys [response_type username password redirect_uri]
    :as request}]
  (let [now (System/currentTimeMillis)
        session (nano-id 20)]
    (set-session session
                 {:request request
                  :at now})
    (case response_type
      ;;
      ("code" "token")
      (try
        (let [client (validate-client session)]
          ;; Proper implementation
          (set-session-client session client)
          (let [{{url "login-page"} :settings} (get-session-client session)]
            {:status 302
             :headers {"Location" (str url "?" (codec/form-encode {:session session}))
                       "Cache-Control" "no-cache"}}))
        (catch clojure.lang.ExceptionInfo ex
          (handle-request-error (ex-data ex))))
      ;;
      "password"
      (try
        (let [user (validate-resource-owner username password)]
          (set-session-resource-owner session user)
          (remove-session session)
          (return-token session))
        (catch clojure.lang.ExceptionInfo ex
          (handle-request-error (ex-data ex))))
      "client_credentials"
      (try
        (validate-client session)
        (return-token session)
        (catch clojure.lang.ExceptionInfo ex (handle-request-error (ex-data ex))))
      ;;
      (cond
        ;;
        (empty? redirect_uri)
        (handle-request-error
          {:type "missing_redirect"
           :session session})
        ;;
        (empty? response_type)
        (handle-request-error
          {:type "missing_response_type"
           :session session})
        ;;
        :else
        (handle-request-error
          {:type "server_error"
           :session session})))))


(defn bind-authorization-code
  [session]
  (let [code (nano-id 30)]
    (swap! *authorization-codes* assoc code {:session session
                                             :at (System/currentTimeMillis)})
    (swap! *sessions* update session
           (fn [current]
             (assoc current :code code)))
    code))


;; Login
(defn login
  [{:keys [username password session]}]
  (let [{{request-type :response_type
          redirect-uri :redirect_uri
          state :state} :request
         :as session-state} (get-session session)
        resource-owner (validate-resource-owner username password)]
    (cond
      ;;
      (nil? session-state)
      (handle-request-error {:type "corrupt_session" :session session}) 
      ;;
      (and resource-owner (= "code" request-type))
      (let [code (bind-authorization-code session)]
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
      (swap! *authorization-codes* dissoc code))))


(defn revoke-access-token
  [session]
  (let [{:keys [access-token]} (get-session session)]
    (swap! *sessions* update session dissoc :access-token)
    (when access-token (swap! *access-tokens* dissoc access-token)))
  nil)


(defn revoke-refresh-token
  [session]
  (let [{:keys [refresh-token]} (get-session session)]
    (swap! *sessions* update session dissoc :refresh-token)
    (when refresh-token (swap! *refresh-tokens* dissoc refresh-token))))


(defn remove-session-tokens
  [session]
  (revoke-access-token session)
  (revoke-refresh-token session)
  nil)


(defn token-error [code & description]
  {:status 400
   :headers {"Content-Type" "application/json;charset=UTF-8"
             "Pragma" "no-cache"
             "Cache-Control" "no-store"}
   :body (json/write-str
           {:error code
            :error_description (str/join "\n" description)})})

(defn token-code-grant
  [data]
  (let [{:keys [code redirect_uri client_id client-password]} data
        {:keys [session]} (get *authorization-codes* code)]
    (if-not session
      ;; If session isn't available, that is if somebody
      ;; is trying to hack in
      (token-error
        "invalid_request"
        "Trying to abuse token endpoint for code that"
        "doesn't exsist or has expired. Further actions"
        "be logged and processed")
      ;; If there is some session than check other requirements
      (let [{{session-redirect-uri :redirect_uri} :request
             session-client :client} (get-session session)
            {_password :password
             :strs [allowed-scopes allowed-grants]
             :as client} (get-session-client session)
            scopes (set allowed-scopes)
            grants (set allowed-grants)]
        (cond
          ;;
          (not (contains? grants "code"))
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
          (and (some? _password) (empty? client-password))
          (token-error
            "invalid_client"
            "Client password wasn't provided")
          ;; If client has password, than
          (and (some? _password) (not (validate-password client-password _password)))
          (token-error
            "invalid_client"
            "Provided client password is wrong")
          ;; Issue that token
          :else
          (let [resource-owner (get-session-resource-owner session)
                resource-owner-details (assoc
                                         (get-user-details (:name resource-owner))
                                         :session session)
                access-token (*access-token-resolver* resource-owner-details client)
                refresh-token (*refresh-token-resolver* {:session session} client)
                response {:access_token access-token
                          :refresh_token refresh-token
                          :token_type "bearer"
                          :expires_in (quot (vura/minutes 1) vura/second)}]
            (remove-session-tokens session)
            (set-session-tokens session response)
            (revoke-authorization-code session)
            {:status 200
             :headers {"Content-Type" "application/json;charset=UTF-8"
                       "Pragma" "no-cache"
                       "Cache-Control" "no-store"}
             :body (json/write-str response)}))))))


(defn token-password-grant
  [data]
  (let [{:keys [password username scope client_id client_password]} data
        {_client-password :password
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
        (empty? client_password))
      (token-error
        "invalid_client"
        "Client is configured as confidential."
        "Password wasn't provided for client")
      ;;
      (and
        _client-password
        (not (validate-password client_password _client-password)))
      (token-error
        "invalid_client"
        "Client password is not correct"
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
        (comment (def username "oauth_test") (def password "change-me"))
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
                session (nano-id 20)
                access-token (*access-token-resolver* resource-owner-details client)
                refresh-token (*refresh-token-resolver* {:session session} client)
                body {:access_token access-token
                      :token_type "Bearer"
                      :refresh_token refresh-token 
                      :expires_in 3600}]
            (set-session session
                         {:request (dissoc data :password)
                          :at now})
            (set-session-client session client)
            (set-session-resource-owner session resource-owner-details)
            (set-session-tokens session body)
            {:status 200
             :headers {"Content-Type" "application/json;charset=UTF-8"
                       "Pragma" "no-cache"
                       "Cache-Control" "no-store"}
             :body (json/write-str body)})
          ;;
          :else
          {:status 200
           :headers {"Content-Type" "application/json;charset=UTF-8"
                     "Pragma" "no-cache"
                     "Cache-Control" "no-store"}
           :body (json/write-str
                   {:access_token (*access-token-resolver* resource-owner-details client)
                    :expires_in 3600})})))))


(defn kill-session
  [session]
  (remove-session-resource-owner session)
  (remove-session-client session)
  (remove-session-tokens session)
  (swap! *sessions* dissoc session))


(defn token-refresh-grant
  [{:keys [refresh_token scope client_id]}]
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
      (let [access-token (*access-token-resolver* resource-owner-details client)
            refresh-token (*refresh-token-resolver* {:session session} client)
            response {:access_token access-token
                      :refresh_token refresh-token
                      :token_type "bearer"
                      :expires_in (quot (vura/minutes 1) vura/second)}]
        (remove-session-tokens session)
        (set-session-tokens session response)
        {:status 200
         :headers {"Content-Type" "application/json;charset=UTF-8"
                   "Pragma" "no-cache"
                   "Cache-Control" "no-store"}
         :body (json/write-str response)}))))


;; TODO - check this, it was coded with little interest and focus
(defn token-client-credentials-grant
  [{:keys [client_id client_password]}]
  (if (empty? client_password)
    (token-error
      "unauthorized_client"
      "Client password is empty. You are not God")
    (let [{{grants "allowed-grants"} :settings
           _password :password :as client} (get-client client_id)
          grants (set grants)]
      (cond
        ;;
        (not (contains? grants "client_credentials"))
        (token-error
          "unauthorized_grant"
          "Client sent access token request"
          "for grant type that is outside"
          "of client configured privileges")
        ;;
        (not (validate-password client_password _password))
        (token-error
          "unauthorized_client"
          "Client credentials are wrong")
        :else
        (let [access-token (*access-token-resolver* {:client client_id} client)
              response {:access_token access-token
                        :token_type "bearer"
                        :expires_in (quot (vura/minutes 1) vura/second)}]
          {:status 200
           :headers {"Content-Type" "application/json;charset=UTF-8"
                     "Pragma" "no-cache"
                     "Cache-Control" "no-store"}
           :body (json/write-str response)})))))


(defn token
  [{{:keys [grant_type] :as data} :params}]
  (case grant_type
    ;; Authorization code grant
    "code"
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


(defn reset
  []
  (reset! *sessions* nil)
  (reset! *resource-owners* nil)
  (reset! *clients* nil)
  (reset! *authorization-codes* nil)
  (reset! *refresh-tokens* nil)
  (reset! *access-tokens* nil))


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


(def login-interceptor
  {:name ::login
   :enter
   (fn [{{{:keys [session]} :params
          :as request} :request :as context}]
     (let [{:keys [form-params]} (bp/form-parser request)]
       (chain/terminate
         (assoc context :response (login (assoc form-params :session session))))))})


(def token-interceptor
  {:name ::token
   :enter
   (fn [{request :request :as context}]
     (chain/terminate
       (assoc context :response (token request))))})


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
             [username password] (decode-base64-credentials credentials)]
         (update-in context [:request :params] assoc
                    :client_id username
                    :client_password password))))})


(def routes
  #{["/oauth2/authorize"
     :get [basic-authorization-interceptor
           authorize-request-interceptor]
     :route-name ::authorize-request]
    ["/oauth2/request_error"
     :get [basic-authorization-interceptor
           authorize-request-error-interceptor]
     :route-name ::authorize-request-error]
    ["/oauth2/token"
     :post [basic-authorization-interceptor
            token-interceptor]
     :route-name ::handle-token]
    ["/login/*" :get [spa-interceptor] :route-name ::serve-login]
    ["/login/*" :post [login-interceptor] :route-name ::handle-login]})
