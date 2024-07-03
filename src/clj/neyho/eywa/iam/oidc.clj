(ns neyho.eywa.iam.oidc
  (:require
    [clojure.string :as str]
    [clojure.spec.alpha :as s]
    [clojure.data.json :as json]
    [clojure.tools.logging :as log]
    [vura.core :as vura]
    [ring.util.codec :as codec]
    [buddy.sign.util :refer [to-timestamp]]
    [io.pedestal.interceptor.chain :as chain]
    [io.pedestal.http.body-params :as bp]
    [neyho.eywa.server.interceptors :refer [spa-interceptor]]
    [neyho.eywa.iam :as iam]
    [neyho.eywa.iam.oauth2 :as oauth2
     :refer [process-scope
             sign-token]]))


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


(defn implicit-flow
  [request]
  (oauth2/token-password-grant request))


(defn hybrid-flow
  [request]
  )


(defn authorization-code-flow
  [{:keys [scope prompt] :as request}]
  (if (or
        (empty? scope)
        (not (contains? scope "openid")))
    (oauth2/authorization-code-flow request) 
    (let []
      (cond
        ;; interaction_required
        ; (and prompt (= prompt "none"))
        ; (oauth2/handle-request-error
        ;   {:type "interaction_required"
        ;    :request request})
        ;; login_required
        ;; account_selection_required
        ;; consent_required
        ;; invalid_request_ur
        ;; invalid_request_object
        ;; request_not_supported
        ;; request_uri_not_supported 
        ;; registration_not_supported
        :else
        (oauth2/authorization-code-flow request)))))


(comment
  (def request (:request (oauth2/get-session "IDldMZTkptwpePGdsqoUnuRaiQtXLL"))))

(defn authorization-request
  [request]
  (letfn [(split-spaces [request k]
            (if-some [val (get request k)]
              (assoc request k (set (str/split val #"\s+")))
              request))]
    (let [{:keys [response_type redirect_uri]
           :as request}
          (-> request
              (split-spaces :scope)
              (split-spaces :response_type))
          [flow] (s/conform ::flow response_type)]
      ; (def request request)
      ; (def response_type response_type)
      ; (def redirect_uri (:redirect_uri request))
      (cond
        ;;
        (not (s/valid? ::redirect_uri redirect_uri))
        (oauth2/handle-request-error
          {:type "missing_redirect"
           :request request})
        ;;
        (s/invalid? flow)
        (oauth2/handle-request-error
          {:type "missing_response_type"
           :request request})
        ;;
        :else
        (case flow
          ;;
          :code (authorization-code-flow request) 
          ;;
          :implicit (implicit-flow request)
          ;;
          :hybrid (hybrid-flow request))))))


(def authorize-request-interceptor
  {:name ::authorize-request
   :enter
   (fn [{{:keys [params]} :request :as context}]
     (chain/terminate
       (assoc context :response
              (authorization-request params))))})


(defn login
  [{:keys [username password session]}]
  (let [{{request-type :response_type
          redirect-uri :redirect_uri
          state :state} :request
         :as session-state} (oauth2/get-session session)
        resource-owner (oauth2/validate-resource-owner username password)]
    (cond
      ;;
      (nil? session-state)
      (oauth2/handle-request-error {:type "corrupt_session" :session session}) 
      ;;
      (and resource-owner (= "code" request-type))
      (let [code (oauth2/bind-authorization-code session)]
        (oauth2/set-session-resource-owner session resource-owner)
        {:status 302
         :headers {"Location" (str redirect-uri "?" (codec/form-encode {:state state :code code}))}})
      ;;
      :else
      (let [{{url "login-page"} :settings} (oauth2/get-session-client session)]
        {:status 302
         :headers {"Location" (str url "?" (codec/form-encode
                                             {:session session
                                              :error ["credentials"]}))
                   "Cache-Control" "no-cache"}}))))


(def ^:dynamic *protocol* "http")
(def ^:dynamic *domain* "localhost:8080")


(letfn [(domain+
          ([] (domain+ ""))
          ([path]
           (str *protocol* "://" *domain* path)))]
  (let [config
        {:issuer (domain+)
         :authorization_endpoint (domain+ "/oauth2/authorize")
         :token_endpoint (domain+ "/oauth2/token")
         :userinfo_endpoint (domain+ "/oidc/userinfo")
         :end_session_endpoint (domain+ "/oidc/logout")
         :revocation_endpoint (domain+ "/oauth2/revoke")
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
    (oauth2/get-session-resource-owner session)
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
  (let [{:keys [euuid]} (oauth2/get-session-resource-owner session)
        {:keys [authorized-at]
         {:keys [nonce]} :request} (oauth2/get-session session)
        client (oauth2/get-session-client session)]
    (update tokens :id_token
            merge
            {:iss oauth2/*iss*
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
  (let [client (oauth2/get-session-client session)]
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
(defmethod process-scope "auth_time" [session tokens _] (assoc-in tokens [:id_token :auth-at] (:authorized-at (oauth2/get-session session))))


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
                    session (oauth2/get-token-session :access_token access-token)
                    {info :person_info} (oauth2/get-session-resource-owner session)]
                {:status 200
                 :header {"Content-Type" "application/json"}
                 :body (json/write-str info)})
              (catch Throwable _
                {:status 403
                 :body "Not authorized"}))))})

;; http://localhost:8080/oidc/logout?id_token_hint=eyJhbGciOiJSUzI1NiJ9.eyJwcm9maWxlIjoiaHR0cHM6Ly9iYnVoYS5wYXJ0aXphbmkueXUiLCJlbWFpbCI6bnVsbCwiaXNzIjoiaHR0cDovL2xvY2FsaG9zdDo4MDgwIiwic3ViIjoiYmIzZTRkMWItZDNhMC00MzRhLWE2MGItYmViMGQxYmY1MmFmIiwiaWF0IjoxNzE5OTM5MTc4LCJleHAiOjE3MTk5NDA5NzgsImF1dGhfdGltZSI6MTcxOTkzOTE3OCwibm9uY2UiOiJlWHVHekxBREhHS28ifQ.fKrotSbaPHGns22RaV8A62k73P-hQrcCoaXamvjIW4v8SInPZIY2ER3fyTQoTBec8XofqkNMZvBLHEmIVAFOhc-52rmW4zR6GFn3FNiTrfYbvwTBgXhh8FkXVnBkesUGWBSAlKGXUe31Qf8MFW3n25QbcNgs64VITEctwny8A0mYyaWtlmDWV9GVpG0yavp_zvsCj73EDmlwowN81jc7IzGFeltukiSV-VVovJkwTE82pOdVsqriSh2fjXfkExtcHcimLzRSKUP2XpDlGn9lJzOOengwk-Lf66LRZ7WV2x6AteH5ulJsB8GGZdI8fo-YC_F6JGNAoo-hfTyd-WIicQ&post_logout_redirect_uri=http%3A%2F%2Flocalhost%3A5173%2Fprofile
(comment
  (def id_token_hint "eyJhbGciOiJSUzI1NiJ9.eyJwcm9maWxlIjoiaHR0cHM6Ly9iYnVoYS5wYXJ0aXphbmkueXUiLCJlbWFpbCI6bnVsbCwiaXNzIjoiaHR0cDovL2xvY2FsaG9zdDo4MDgwIiwic3ViIjoiYmIzZTRkMWItZDNhMC00MzRhLWE2MGItYmViMGQxYmY1MmFmIiwiaWF0IjoxNzE5OTM5MTc4LCJleHAiOjE3MTk5NDA5NzgsImF1dGhfdGltZSI6MTcxOTkzOTE3OCwibm9uY2UiOiJlWHVHekxBREhHS28ifQ.fKrotSbaPHGns22RaV8A62k73P-hQrcCoaXamvjIW4v8SInPZIY2ER3fyTQoTBec8XofqkNMZvBLHEmIVAFOhc-52rmW4zR6GFn3FNiTrfYbvwTBgXhh8FkXVnBkesUGWBSAlKGXUe31Qf8MFW3n25QbcNgs64VITEctwny8A0mYyaWtlmDWV9GVpG0yavp_zvsCj73EDmlwowN81jc7IzGFeltukiSV-VVovJkwTE82pOdVsqriSh2fjXfkExtcHcimLzRSKUP2XpDlGn9lJzOOengwk-Lf66LRZ7WV2x6AteH5ulJsB8GGZdI8fo-YC_F6JGNAoo-hfTyd-WIicQ")
  (def post_logout_redirect_uri "post_logout_redirect_uri=http%3A%2F%2Flocalhost%3A5173%2Fprofile")
  (oauth2/get-token-session :id_token token))


(defn request-error [code & description]
  {:status 400
   :headers {"Content-Type" "text/html"}
   :body (json/write-str (str/join "\n" description))})


(let [invalid-token (request-error "Token is not valid")
      session-not-found (request-error "Session is not active")
      invalid-session (request-error "Session is not valid")
      invalid-issuer (request-error "Issuer is not valid")
      invalid-redirect (request-error "Provided 'post_logout_redirect_uri' is not valid")]
  (def logout-interceptor
    {:enter
     (fn [{{{:keys [post_logout_redirect_uri id_token_hint state ui_locales client_id]} :params} :request :as ctx}]
       (let [session (oauth2/get-token-session :id_token id_token_hint)
             {{valid-redirections "logout-redirections"} :settings :as client} (oauth2/get-session-client session)
             {:keys [iss sid] :as token} (try
                                           (iam/unsign-data id_token_hint)
                                           (catch Throwable _ nil))
             redirect-uri (and post_logout_redirect_uri (some #(when (= % post_logout_redirect_uri) %) valid-redirections))]
         (cond
           (nil? session)
           session-not-found
           ;; Token couldn't be unsigned
           (nil? token)
           invalid-token
           ;; Session doesn't match
           (not= sid session)
           invalid-session
           ;; Issuer is not the same
           (not= iss oauth2/*iss*)
           invalid-issuer
           ;; Redirect uri isn't valid
           (and (some? post_logout_redirect_uri) (nil? redirect-uri))
           invalid-redirect
           ;;
           (some? redirect-uri)
           (do
             (oauth2/kill-session session)
             (assoc ctx :response
                    {:status 302
                     :headers {"Location" (str redirect-uri (when (not-empty state) (str "?" (codec/form-encode {:state state}))))
                               "Cache-Control" "no-cache"}}))
           :else
           (do
             (oauth2/kill-session session)
             (assoc ctx :response
                    {:status 200
                     :headers {"Content-Type" "text/html"}
                     :body "User logged out!"})))))}))


(defn clients-match? [session {:keys [client_id client_secret]}]
  (let [{known-id :id
         known-secret :secret} (oauth2/get-session-client session)]
    (cond
      (not= client_id known-id) false
      (and client_secret (not= client_secret known-secret)) false
      (and known-secret (not= client_secret known-secret)) false
      :else true)))


(def clients-doesnt-match? (complement clients-match?))


(let [invalid-client (oauth2/json-error "invalid_client" "Client ID is not valid")
      invalid-token (oauth2/json-error "invalid_token" "Token is not valid")]
  (def revoke-token-interceptor
    {:enter
     (fn [{{{:keys [token_type_hint token] :as params} :params} :request :as ctx}]
       (let [token-key (when token_type_hint (keyword token_type_hint))
             tokens @oauth2/*tokens*
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
                     (oauth2/revoke-token token-key token)
                     (assoc ctx :response
                            {:status 200
                             :headers {"Content-Type" "application/json"
                                       "Cache-Control" "no-store"
                                       "Pragma" "no-cache"}}))))))}))


;; This is not used, because clients not all clients are available from start
;; this will be changed in the future and should be used when iam.oauth2 will
;; track delta events from dataset.core
(let [invalid-client (oauth2/json-error
                       "invalid_client"
                       "Provided client_id wasn't found"
                       "Your attempt will be logged and"
                       "processed")
      invalid-secret (oauth2/json-error
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
                known-secret :secret} (get @oauth2/*clients* id)]
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




(let [common [oauth2/basic-authorization-interceptor
              (bp/body-params)
              oauth2/keywordize-params]
      authorize (conj common authorize-request-interceptor)
      request_error (conj common oauth2/authorize-request-error-interceptor)
      token (conj common oauth2/token-interceptor)
      user-info (conj common user-info-interceptor)
      logout (conj common logout-interceptor)
      revoke (conj common revoke-token-interceptor)]
  (def routes
    #{["/oauth2/authorize" :get authorize :route-name ::authorize-request]
      ["/oauth2/request_error" :get request_error :route-name ::authorize-request-error]
      ["/oauth2/token" :post token :route-name ::handle-token]
      ["/oauth2/revoke" :post revoke :route-name ::post-revoke]
      ["/oauth2/revoke" :get revoke :route-name ::get-revoke]
      ["/oidc/userinfo" :get user-info :route-name ::user-info]
      ["/oidc/logout" :post logout :route-name ::get-logout]
      ["/oidc/logout" :get  logout :route-name ::post-logout]
      ;;
      ["/.well-known/openid-configuration" :get [open-id-configuration-interceptor] :route-name ::open-id-configuration]
      ;;
      ["/login" :get [oauth2/redirect-to-login] :route-name ::short-login]
      ["/login/" :get [oauth2/redirect-to-login] :route-name ::root-login]
      ["/login/*" :get [spa-interceptor] :route-name ::serve-login]
      ["/login/*" :post [oauth2/login-interceptor] :route-name ::handle-login]}))


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
