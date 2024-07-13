(ns neyho.eywa.iam.oidc
  (:require
    [clojure.string :as str]
    [clojure.spec.alpha :as s]
    [clojure.data.json :as json]
    [clojure.tools.logging :as log]
    [clojure.java.io :as io]
    [vura.core :as vura]
    [ring.util.codec :as codec]
    [ring.util.response :as response]
    [buddy.core.codecs]
    [buddy.core.hash :as hash]
    [buddy.sign.util :refer [to-timestamp]]
    [io.pedestal.interceptor.chain :as chain]
    [io.pedestal.http.body-params :as bp]
    [neyho.eywa.iam :as iam]
    [neyho.eywa.iam.oauth2-1 :as oauth
     :refer [process-scope
             sign-token]]
    [io.pedestal.http.ring-middlewares :as middleware])
  #_(:import
      [java.security KeyFactory]
      [java.security.interfaces RSAPublicKey]
      [java.security.spec X509EncodedKeySpec]
      [org.bouncycastle.util.encoders Base64]))


(s/def ::iss string?)
(s/def ::sub string?)
(s/def ::aud string?)
(s/def ::exp number?)
(s/def ::iat number?)
(s/def ::auth_time number?)
(s/def ::nonce string?)
(s/def ::acr string?)
(s/def ::amr (s/coll-of string? :kind sequential?))


(comment
  (time (s/valid? ::amr ["jfioq" 100])))



(def explain-id-key
  {::iss "Issuer Identifier"
   ::sub "Subject Identifier"
   ::aud "Audience(s). Must contain client_id of relying party"
   ::exp "Expiration time"
   ::iat "issued at"
   ::auth_time "Tim ewhen the end-user authentication occured"
   ::nonce "String value used to associate a client session with an ID Token to mitigate replay attacks"
   ::acr "Authentication Context Class Reference... WTF"
   ::amr "Authentication Methods References. Array of strings that are identifiers for used methods of authentication. Maybe password + OTP"
   ::azp "Authorized Party. The party to which ID token was issued. Ignore for now"})

(s/def ::id-token (s/keys
                    :req-un [::iss ::sub ::aud ::exp ::iat]
                    :opt-un [::auth_time ::nonce ::act ::amr]))

(s/def ::display #{"page" "popup" "touch" "wap"})


(s/def ::scope set?)

(s/def ::code-response (partial = #{"code"}))
(s/def ::implicit-id-response (partial = #{"id_token"}))
(s/def ::implicit-all-response (partial = #{"id_token" "token"}))
(s/def ::hybrid-id-reponse (partial = #{"code" "id_token"}))
(s/def ::hybrid-token-response (partial = #{"code" "token"}))
(s/def ::hybrid-all-response (partial = #{"code" "id_token" "token"}))


(s/def ::authorization-code-flow ::code-response)
(s/def ::implicit-flow
  (s/or :id ::implicit-id-response
        :id+token ::implicit-all-response))
(s/def ::hybrid-flow
  (s/or :id ::hybrid-id-reponse
        :token ::hybrid-token-response
        :id+token ::hybrid-all-response))


(s/def ::flow
  (s/or
    :code ::authorization-code-flow
    :implicit ::implicit-flow
    :hybrid ::hybrid-flow))


(s/def ::response_type
  (s/or
    :code ::code-response
    :implicit-id  ::implicit-id-response
    :implicit-all ::implicit-all-response
    :hybrid-id ::hybrid-id-reponse
    :hybrid-token-response ::hybrid-token-response
    :hybrid-all-response ::hybrid-all-response))


(s/def ::client_id string?)
(s/def ::redirect_uri (s/and string? not-empty))


(s/def ::prompt #{"login" "page" "popup" "none"})
(s/def ::prompt-is-none #{"none"})


(s/def ::authentication-request-keys
  (s/keys
    :req-un [::scope ::response_type ::client_id ::redirect_uri]
    :opt-un [::state ::response_mode ::nonce ::display ::prompt
             ::max_age ::ui_locales ::id_token_hint
             ::login_hint ::acr_values]))


(s/def ::open-id-scope
  (fn [{:keys [scope]}] (contains? scope "openid")))


(s/def ::authentication-request
  (s/and
    ::authentication-request-keys
    ::open-id-scope))


(comment
  (s/valid? ::display "popup")
  (s/valid? ::open-id-scope? {:scope ["ifejoq"]})
  (def request
    {:scope  #{"openid"}
     :response_type #{"code"}
     :client_id "f019uj391r9231"
     :redirect_uri "http://localhost:8080/eywa"})

  (s/explain ::code-response (:response_type request))
  (s/conform ::flow (:response_type request))
  (s/valid?
    ::authentication-request
    {:scope  #{"openid"}
     :response_type #{"code"}
     :client_id "f019uj391r9231"
     :redirect_uri "http://localhost:8080/eywa"}))




(def authorize-request-interceptor
  {:name ::authorize-request
   :enter
   (fn [{{:keys [params]} :request :as context}]
     (let [response (oauth/authorization-request params)]
       (chain/terminate (assoc context :response response))))})


(def ^:dynamic *protocol* "http")
(def ^:dynamic *domain* "localhost:8080")


(letfn [(domain+
          ([] (domain+ ""))
          ([path]
           (str *protocol* "://" *domain* path)))]
  (let [config
        {:issuer (domain+)
         :authorization_endpoint (domain+ "/oauth2.1/authorize")
         :token_endpoint (domain+ "/oauth2.1/token")
         :userinfo_endpoint (domain+ "/oidc/userinfo")
         :jwks_uri (domain+ "/oauth2.1/jwks")
         :end_session_endpoint (domain+ "/oidc/logout")
         :revocation_endpoint (domain+ "/oauth2.1/revoke")
         :response_types_supported ["code" "token" "id_token"
                                    "code id_token" "token id_token"
                                    "code token id_token"]
         :subject_types_supported ["public"]
         :token_endpoint_auth_methods_supported ["client_secret_basic" "client_secret_post"]
         :scopes_supported ["openid" "profile" "offline_access"
                            "name" "given_name" "family_name" "nickname"
                            "email" "email_verified" "picture"
                            "created_at" "identities" "phone" "address"]}]
    (def open-id-configuration-interceptor
      {:enter
       (fn [ctx]
         (assoc ctx :response
                {:status 200
                 :headers {"Content-Type" "application/json"}
                 :body (json/write-str config :escape-slash false)}))})))


(defn standard-claim
  [session claim]
  (get-in
    (oauth/get-session-resource-owner session)
    [:person_info claim]))


(defn add-standard-claim
  [tokens session claim]
  (assoc-in tokens [:id_token claim] (standard-claim session claim)))


(let [default (vura/minutes 30)]
  (defn id-token-expiry
    [{{{expiry "id"} "token-expiry"} :settings}]
    (or expiry default)))


(defmethod process-scope "openid"
  [session tokens _]
  (let [{:keys [euuid]} (oauth/get-session-resource-owner session)
        {:keys [authorized-at code]} (oauth/get-session session)
        {:keys [nonce]} (oauth/get-code-request code)
        client (oauth/get-session-client session)]
    (update tokens :id_token
            merge
            {:iss oauth/*iss*
             :sub euuid
             :iat (to-timestamp (vura/date))
             :exp (-> (vura/date)
                      vura/date->value
                      (+ (id-token-expiry client))
                      vura/value->date
                      to-timestamp)
             :sid session
             :auth_time authorized-at
             :nonce nonce})))


(defmethod sign-token :id_token
  [session _ data]
  (let [client (oauth/get-session-client session)]
    (iam/sign-data
      (assoc data
             :exp (-> (vura/date)
                      vura/date->value
                      (+ (id-token-expiry client))
                      vura/value->date
                      to-timestamp))
      {:alg :rs256})))


(defmethod process-scope "family_name" [session tokens _] (add-standard-claim tokens session :family_name))
(defmethod process-scope "middle_name" [session tokens _] (add-standard-claim tokens session :middle_name))
(defmethod process-scope "given_name" [session tokens _] (add-standard-claim tokens session :given_name))
(defmethod process-scope "nickname" [session tokens _] (add-standard-claim tokens session :nickname))
(defmethod process-scope "preferred_username" [session tokens _] (add-standard-claim tokens session :preferred_username))
(defmethod process-scope "profile" [session tokens _] (add-standard-claim tokens session :profile))
(defmethod process-scope "picture" [session tokens _] (add-standard-claim tokens session :picture))
(defmethod process-scope "website" [session tokens _] (add-standard-claim tokens session :website))
(defmethod process-scope "email" [session tokens _] (add-standard-claim tokens session :email))
(defmethod process-scope "email_verified" [session tokens _] (add-standard-claim tokens session :email_verified))
(defmethod process-scope "gender" [session tokens _] (add-standard-claim tokens session :gender))
(defmethod process-scope "birthdate" [session tokens _] (add-standard-claim tokens session :birthdate))
(defmethod process-scope "zoneinfo" [session tokens _] (add-standard-claim tokens session :zoneinfo))
(defmethod process-scope "phone_number" [session tokens _] (add-standard-claim tokens session :phone_number))
(defmethod process-scope "phone_number_verified" [session tokens _] (add-standard-claim tokens session :phone_number_verified))
(defmethod process-scope "address" [session tokens _] (add-standard-claim tokens session :address))
(defmethod process-scope "updated_at" [session tokens _] (add-standard-claim tokens session :modified_on))
(defmethod process-scope "auth_time" [session tokens _] (assoc-in tokens [:id_token :auth-at] (:authorized-at (oauth/get-session session))))


(defn get-access-token
  [{{{authorization "authorization"
      :as headers} :headers} :request}]
  (let [access-token (if (.startsWith authorization "Bearer")
                       (subs authorization 7)
                       (throw
                         (ex-info
                           "Authorization header doesn't contain access token"
                           headers)))]
    access-token))


(def user-info-interceptor
  {:enter
   (fn [ctx]
     (assoc ctx :response
            (try
              (let [access-token (get-access-token ctx)
                    session (oauth/get-token-session :access_token access-token)
                    {info :person_info
                     :keys [euuid]} (oauth/get-session-resource-owner session)]
                {:status 200
                 :headers {"Content-Type" "application/json"}
                 :body (json/write-str (assoc info :sub euuid))})
              (catch Throwable _
                {:status 403
                 :body "Not authorized"}))))})


(defn request-error
  [code & description]
  {:status code
   :headers {"Content-Type" "text/html"}
   :body (json/write-str (str/join "\n" description))})


(def idsrv-session-read
  {:enter (fn [ctx]
            (let [{{{{idsrv-session :value} "idsrv.session"} :cookies} :request} ctx]
              (if (empty? idsrv-session) ctx
                (assoc-in ctx [:request :params :idsrv/session] idsrv-session))))})


(def idsrv-session-remove
  {:leave (fn [ctx]
            (assoc-in ctx [:response :cookies "idsrv.session"]
                      {:value ""
                       :path "/"
                       :http-only true
                       :secure true
                       :max-age 0}))})



; (let [invalid-token (request-error 400 "Token is not valid")
;       session-not-found (request-error 400 "Session is not active")
;       invalid-session (request-error 400 "Session is not valid")
;       invalid-issuer (request-error 400 "Issuer is not valid")
;       invalid-redirect (request-error 400 "Provided 'post_logout_redirect_uri' is not valid")]
;   (let [{{{:keys [post_logout_redirect_uri id_token_hint]} :params} :request} ctx
;         session (or (oauth/get-token-session :id_token id_token_hint))
;         {client_id :id} (oauth/get-session-client session)
;         {{valid-redirections "logout-redirections"} :settings} (iam/get-client client_id)
;         {:keys [iss sid] :as token} (try
;                                       (iam/unsign-data id_token_hint)
;                                       (catch Throwable _ nil))
;         post-redirect-ok? (some #(when (= % post_logout_redirect_uri) true) valid-redirections)]
;     [session token sid iss post-redirect-ok? post_logout_redirect_uri]
;     #_(cond
;       (nil? session)
;       (idsrv-session-remove session-not-found)
;       ;; Token couldn't be unsigned
;       (nil? token)
;       invalid-token
;       ;; Session doesn't match
;       (not= sid session)
;       invalid-session
;       ;; Issuer is not the same
;       (not= iss oauth/*iss*)
;       invalid-issuer
;       ;; Redirect uri isn't valid
;       (not post-redirect-ok?)
;       invalid-redirect
;       ;;
;       (some? post_logout_redirect_uri)
;       (do
;         (oauth/kill-session session)
;         {:status 302
;          :headers {"Location" (str post_logout_redirect_uri (when (not-empty state) (str "?" (codec/form-encode {:state state}))))
;                    "Cache-Control" "no-cache"}})
;       ;;
;       :else
;       (do
;         (oauth/kill-session session)
;         {:status 200
;          :headers {"Content-Type" "text/html"}
;          :body "User logged out!"}))))




(let [invalid-token (request-error 400 "Token is not valid")
      session-not-found (request-error 400 "Session is not active")
      invalid-session (request-error 400 "Session is not valid")
      invalid-issuer (request-error 400 "Issuer is not valid")
      invalid-redirect (request-error 400 "Provided 'post_logout_redirect_uri' is not valid")]
  (def logout-interceptor
    {:enter
     (fn [ctx]
       (let [{{{:keys [post_logout_redirect_uri id_token_hint state]
                idsrv-session :idsrv/session} :params} :request} ctx
             session (or (oauth/get-token-session :id_token id_token_hint)
                         idsrv-session)
             {client_id :id} (oauth/get-session-client session)
             {{valid-redirections "logout-redirections"} :settings} (iam/get-client client_id)
             {:keys [iss sid] :as token} (try
                                           (iam/unsign-data id_token_hint)
                                           (catch Throwable _ nil))
             post-redirect-ok? (some #(when (= % post_logout_redirect_uri) true) valid-redirections)]
         (assoc ctx :response
                (cond
                  (nil? session)
                  (idsrv-session-remove session-not-found)
                  ;; Token couldn't be unsigned
                  (and id_token_hint (nil? token))
                  invalid-token
                  ;; Session doesn't match
                  (and id_token_hint (not= sid session))
                  invalid-session
                  ;; Issuer is not the same
                  (and id_token_hint (not= iss oauth/*iss*))
                  invalid-issuer
                  ;; Redirect uri isn't valid
                  (not post-redirect-ok?)
                  invalid-redirect
                  ;;
                  (some? post_logout_redirect_uri)
                  (do
                    (oauth/kill-session session)
                    {:status 302
                     :headers {"Location" (str post_logout_redirect_uri (when (not-empty state) (str "?" (codec/form-encode {:state state}))))
                               "Cache-Control" "no-cache"}})
                  ;;
                  :else
                  (do
                    (oauth/kill-session session)
                    {:status 200
                     :headers {"Content-Type" "text/html"}
                     :body "User logged out!"})))))}))


(defn clients-match? [session {:keys [client_id client_secret]}]
  (let [{known-id :id
         known-secret :secret} (oauth/get-session-client session)]
    (cond
      (not= client_id known-id) false
      (and client_secret (not= client_secret known-secret)) false
      (and known-secret (not= client_secret known-secret)) false
      :else true)))


(def clients-doesnt-match? (complement clients-match?))


(let [invalid-client (oauth/json-error "invalid_client" "Client ID is not valid")
      invalid-token (oauth/json-error "invalid_token" "Token is not valid")]
  (def revoke-token-interceptor
    {:enter
     (fn [{{{:keys [token_type_hint token] :as params} :params} :request :as ctx}]
       (let [token-key (when token_type_hint (keyword token_type_hint))
             tokens @oauth/*tokens*
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
             (clients-doesnt-match? session params) (error invalid-client)
             :else (do
                     (log/debugf "[%s] Revoking token %s %s" session token-key token)
                     (oauth/revoke-token token-key token)
                     (assoc ctx :response
                            {:status 200
                             :headers {"Content-Type" "application/json"
                                       "Cache-Control" "no-store"
                                       "Pragma" "no-cache"}}))))))}))


;; This is not used, because clients not all clients are available from start
;; this will be changed in the future and should be used when iam.oauth2 will
;; track delta events from dataset.core
(let [invalid-client (oauth/json-error
                       "invalid_client"
                       "Provided client_id wasn't found"
                       "Your attempt will be logged and"
                       "processed")
      invalid-secret (oauth/json-error
                       "invalid_secret"
                       "You are using client secret that is invalid."
                       "This request will be logged and processed!")]
  (def authorize-client-interceptor
    {:name ::authentication-basic
     :enter
     (fn [{{{id :client_id
             secret :client_secret} :params} :request
           :as context}]
       (letfn [(error [response]
                 (chain/terminate (assoc context :response response)))]
         (let [{known-id :id
                known-secret :secret} (get @oauth/*clients* id)]
           (cond
             (not= id known-id)
             (error invalid-client)
             ;;
             (or
               (and known-secret (not= known-secret secret))
               (and secret (not= known-secret secret)))
             (error invalid-secret)
             ;; Pass
             :else context))))}))


(defn generate-code-challenge
  ([code-verifier] (generate-code-challenge code-verifier "S256"))
  ([code-verifier code-challenge-method]
   (case code-challenge-method
     "plain" code-verifier
     "S256"
     (let [bs (.getBytes code-verifier)
           hashed (hash/sha256 bs)]
       (-> hashed
           codec/base64-encode
           (.replace "+" "-")
           (.replace "/" "_")
           (.replace "=" ""))))))


(def pkce-interceptor
  {:enter
   (fn [ctx]
     (let [{{{:keys [code code_verifier grant_type]} :params} :request} ctx
           {{:keys [code_challenge code_challenge_method]} :request} (-> code
                                                                       oauth/get-code-session 
                                                                       oauth/get-session)
           is-pkce? (and code_challenge code_challenge_method)]
       (if (or (not is-pkce?) (not= "authorization_code" grant_type)) ctx
         (let [current-challenge (generate-code-challenge code_verifier code_challenge_method)]
           (if (= current-challenge code_challenge) ctx
             (chain/terminate
               (oauth/json-error
                 "invalid_request"
                 "Proof Key for Code Exchange failed")))))))})


(def jwks-interceptor
  {:enter (fn [ctx]
            (assoc ctx :response
                   {:status 200
                    :headers {"Content-Type" "application/json"}
                    :body (json/write-str
                            {:keys
                             (map
                               (fn [{:keys [public]}] (iam/encode-rsa-key public))
                               @iam/encryption-keys)})}))})


(defn get-cookies [{{:keys [headers]} :request}]
  (let [{cookies "cookie"} headers]
    (when cookies
      (reduce
        (fn [r c]
          (let [[k v] (clojure.string/split c #"=")]
            (assoc r k v)))
        nil
        (clojure.string/split cookies #"[;\s]+")))))


(def serve-login-page
  {:enter (fn [{{:keys [uri]
                 {:keys [session]
                  idsrv-session :idsrv/session} :params} :request :as ctx}]
            (let [ext (re-find #"(?<=\.).*?$" uri)
                  path (subs uri 6)
                  {:keys [code]} (oauth/get-session session)
                  {{redirect-uri :redirect_uri
                    :keys [state prompt]} :request} (oauth/get-code-request code)
                  response (letfn [(revoke-idsrv [response]
                                     (assoc-in response [:cookies "idsrv.session"]
                                               {:value ""
                                                :path "/"
                                                :max-age 0}))]
                             (cond
                               ;; First check if there is active session
                               (and (= session idsrv-session) (= prompt "none"))
                               (let [code (oauth/bind-authorization-code session)]
                                 {:status 302
                                  :headers {"Location" (str redirect-uri "?" (codec/form-encode {:state state :code code}))}})
                               ;; If there is session but it wasn't created by EYWA
                               ;; return error
                               ;; (nil? (oauth/get-session session))
                               ;; (request-error 400 "Target session doesn't exist")
                               ;; Then check if some file was requested
                               (and ext (io/resource path))
                               (revoke-idsrv (response/resource-response path))
                               ;; Finally return index.html if you don't know what
                               ;; to do
                               :else
                               (revoke-idsrv (response/resource-response "login/index.html"))))]
              (assoc ctx :response response)))})



(let [;; save-context (fn [context]
      ;;                (def context context)
      ;;                context) 
      common [oauth/basic-authorization-interceptor
              middleware/cookies
              (bp/body-params)
              oauth/keywordize-params]
      ; authorize (conj common save-context)
      authorize (conj common
                      idsrv-session-read
                      authorize-request-interceptor)
      request_error (conj common oauth/authorize-request-error-interceptor)
      token (conj common oauth/scope->set pkce-interceptor oauth/token-interceptor)
      user-info (conj common user-info-interceptor)
      logout (conj common idsrv-session-remove idsrv-session-read logout-interceptor)
      revoke (conj common idsrv-session-read revoke-token-interceptor)]
  (def routes
    #{["/oauth2.1/authorize" :get authorize :route-name ::authorize-request]
      ["/oauth2.1/request_error" :get request_error :route-name ::authorize-request-error]
      ["/oauth2.1/token" :post token :route-name ::handle-token]
      ["/oauth2.1/revoke" :post revoke :route-name ::post-revoke]
      ["/oauth2.1/revoke" :get revoke :route-name ::get-revoke]
      ["/oauth2.1/jwks" :get [jwks-interceptor] :route-name ::get-jwks]
      ["/oidc/login/*" :get (conj common idsrv-session-read serve-login-page) :route-name ::short-login-redirect]
      ["/oidc/login/*" :post [middleware/cookies oauth/login-interceptor] :route-name ::handle-login]
      ["/oidc/login" :get [oauth/redirect-to-login] :route-name ::short-login]
      ["/oidc/userinfo" :get user-info :route-name ::user-info]
      ["/oidc/logout" :post logout :route-name ::get-logout]
      ["/oidc/logout" :get  logout :route-name ::post-logout]
      ["/.well-known/openid-configuration" :get [open-id-configuration-interceptor] :route-name ::open-id-configuration]}))


(comment
  (def client
    {:euuid #uuid "62be820e-379e-11ef-94b3-02a535895d2d",
     :id "MXQUDYKLLJXXSJOODPFEALTNAMJPVWYKSQRIYJFSIGRVGECZ",
     :name "oidc-client-test",
     :type "confidential",
     :active true,
     :secret "dvG99sAMQXlR6PmT7N2-7AD9PNEz3osew7p06QUHEK8hji66",
     :settings
     {"version" 0,
      "login-page" "http://localhost:8080/login/kbdev/",
      "redirections"
      ["http://localhost:8080/eywa/" "http://localhost:8080/app/kbdev"],
      "token-expiry" {"access" 300000, "refresh" 129600000},
      "allowed-grants" ["refresh_token" "code" "token" "id_token"],
      "refresh-tokens" true}})
  (s/valid? ::id-token {:iss 100})
  (s/valid? ::id-token {:iss 100 :sub "09102" :aud "291" :exp "2019" :iat 100}))
