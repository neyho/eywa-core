(ns neyho.eywa.iam.oidc
  (:require
    [clojure.set :as set]
    [clojure.string :as str]
    [clojure.spec.alpha :as s]
    [clojure.data.json :as json]
    [vura.core :as vura]
    [ring.util.codec :as codec]
    [buddy.core.codecs]
    [buddy.core.hash :as hash]
    [buddy.sign.util :refer [to-timestamp]]
    [io.pedestal.interceptor.chain :as chain]
    [neyho.eywa.iam :as iam]
    [neyho.eywa.iam.oauth.core
     :refer [process-scope
             sign-token
             domain+
             get-session
             get-session-client
             get-session-resource-owner
             json-error
             *clients*]]
    [neyho.eywa.iam.oauth.core :as core]
    [neyho.eywa.iam.oauth.login :as login]
    [neyho.eywa.iam.oauth.token
     :refer [get-token-session]]
    [neyho.eywa.iam.oauth.authorization-code :as ac]
    [neyho.eywa.iam.oauth.device-code :as dc]))


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




(let [config
      {:issuer (domain+)
       :authorization_endpoint (domain+ "/oauth/authorize")
       :device_authorization_endpoint (domain+ "/oauth/device")
       :token_endpoint (domain+ "/oauth/token")
       :userinfo_endpoint (domain+ "/oauth/userinfo")
       :jwks_uri (domain+ "/oauth/jwks")
       :end_session_endpoint (domain+ "/oauth/logout")
       :revocation_endpoint (domain+ "/oauth/revoke")
       ; :response_types_supported ["code" "token" "id_token"
       ;                            "code id_token" "token id_token"
       ;                            "code token id_token"]
       :response_types_supported ["urn:ietf:params:oauth:grant-type:device_code"
                                  "code"]
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
               :body (json/write-str config :escape-slash false)}))}))


(defn standard-claim
  [session claim]
  (get-in
    (get-session-resource-owner session)
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
  (let [{:keys [euuid]} (get-session-resource-owner session)
        {:keys [authorized-at code]} (get-session session)
        {:keys [nonce]} (ac/get-code-request code)
        client (get-session-client session)]
    (update tokens :id_token
            merge
            {:iss (domain+)
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
  (let [client (get-session-client session)]
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
(defmethod process-scope "auth_time" [session tokens _] (assoc-in tokens [:id_token :auth-at] (:authorized-at (get-session session))))


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
                    session (get-token-session :access_token access-token)
                    {info :person_info
                     :keys [euuid]} (get-session-resource-owner session)]
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



;; This is not used, because clients not all clients are available from start
;; this will be changed in the future and should be used when iam.oauth2 will
;; track delta events from dataset.core
;; DEPRECATED - OAuth2.1 doesn't support this
(let [invalid-client (json-error
                       "invalid_client"
                       "Provided client_id wasn't found"
                       "Your attempt will be logged and"
                       "processed")
      invalid-secret (json-error
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
                known-secret :secret} (get @*clients* id)]
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
                                                                       ac/get-code-session 
                                                                       get-session)
           is-pkce? (and code_challenge code_challenge_method)]
       (if (or (not is-pkce?) (not= "authorization_code" grant_type)) ctx
         (let [current-challenge (generate-code-challenge code_verifier code_challenge_method)]
           (if (= current-challenge code_challenge) ctx
             (chain/terminate
               (json-error
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


(let [user-info (conj core/oauth-common-interceptor user-info-interceptor)]
  (def routes
    (reduce
      set/union
      [ac/routes
       dc/routes
       login/routes
       #{["/oauth/jwks" :get [jwks-interceptor] :route-name ::get-jwks]
         ["/oauth/userinfo" :get user-info :route-name ::user-info]
         ["/.well-known/openid-configuration" :get [open-id-configuration-interceptor] :route-name ::open-id-configuration]}])))


; (let [common [basic-authorization-interceptor
;               middleware/cookies
;               (bp/body-params)
;               keywordize-params]
;       ; authorize (conj common save-context)
;       authorization-code (conj common
;                                idsrv-session-read
;                                start-authorization-code-flow)
;       request_error (conj common authorize-request-error-interceptor)
;       token (conj common scope->set pkce-interceptor token-interceptor)
;       user-info (conj common user-info-interceptor)
;       logout (conj common idsrv-session-remove idsrv-session-read logout-interceptor)
;       revoke (conj common idsrv-session-read revoke-token-interceptor)]
;   (def routes
;       (into
;         #{["/oauth/authorize" :get authorization-code :route-name ::authorize-request]
;           ["/oauth/status" :get status-page :route-name ::oauth-status]
;           ["/oauth/request_error" :get request_error :route-name ::authorize-request-error]
;           ["/oauth/status" :get status-page :route-name ::oauth-status]
;           ["/oauth/token" :post token :route-name ::handle-token]
;           ["/oauth/revoke" :post revoke :route-name ::post-revoke]
;           ["/oauth/revoke" :get revoke :route-name ::get-revoke]
;           ;; Login authentication logic
;           ["/oauth/login" :get [redirect-to-login] :route-name ::short-login]
;           ["/oauth/login/index.html" :post [middleware/cookies login-interceptor login-page] :route-name ::handle-login]
;           ["/oauth/login/index.html" :get (conj common idsrv-session-read login-interceptor login-page) :route-name ::short-login-redirect]
;           ;; Login resources
;           ["/oauth/login/icons/*" :get [serve-resource] :route-name ::login-images]
;           ["/oauth/icons/*" :get [serve-resource] :route-name ::login-icons]
;           ["/oauth/css/*" :get [serve-resource] :route-name ::login-css]
;           ["/oauth/js/*" :get [serve-resource] :route-name ::login-js]
;           ;; Logout logic
;           ["/oauth/logout" :post logout :route-name ::get-logout]
;           ["/oauth/logout" :get  logout :route-name ::post-logout]
;           ;; OIDC
;           }
;         dc/routes))
;   
;   
;   )
;
;
; (comment
;   (ns-unmap 'neyho.eywa.iam.oidc 'process-scope)
;   (ns-unmap 'neyho.eywa.iam.oidc 'sign-token)
;   (ns-unmap 'neyho.eywa.iam.oidc 'domain+)
;   (ns-unmap 'neyho.eywa.iam.oidc 'device-code-flow-interceptor)
;   (def client
;     {:euuid #uuid "62be820e-379e-11ef-94b3-02a535895d2d",
;      :id "MXQUDYKLLJXXSJOODPFEALTNAMJPVWYKSQRIYJFSIGRVGECZ",
;      :name "oidc-client-test",
;      :type "confidential",
;      :active true,
;      :secret "dvG99sAMQXlR6PmT7N2-7AD9PNEz3osew7p06QUHEK8hji66",
;      :settings
;      {"version" 0,
;       "login-page" "http://localhost:8080/login/kbdev/",
;       "redirections"
;       ["http://localhost:8080/eywa/" "http://localhost:8080/app/kbdev"],
;       "token-expiry" {"access" 300000, "refresh" 129600000},
;       "allowed-grants" ["refresh_token" "code" "token" "id_token"],
;       "refresh-tokens" true}})
;   (s/valid? ::id-token {:iss 100})
;   (s/valid? ::id-token {:iss 100 :sub "09102" :aud "291" :exp "2019" :iat 100}))
