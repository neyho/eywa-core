(ns neyho.eywa.iam.oauth.token
  (:require
    [clojure.string :as str]
    clojure.java.io
    clojure.pprint
    [clojure.tools.logging :as log]
    [camel-snake-kebab.core :as csk]
    [nano-id.core :as nano-id]
    [vura.core :as vura]
    [clojure.data.json :as json]
    [buddy.sign.util :refer [to-timestamp]]
    [buddy.core.codecs]
    [io.pedestal.interceptor.chain :as chain]
    [neyho.eywa.iam
     :refer [sign-data]]
    [neyho.eywa.iam.access :as access]
    [neyho.eywa.dataset :as dataset]
    [neyho.eywa.iam.uuids :as iu]
    [neyho.eywa.iam.oauth.core :as core
     :refer [pprint
             process-scope
             sign-token]]))


(defonce ^:dynamic *tokens* (atom nil))


(def token
  "eyJhbGciOiJSUzI1NiIsImtpZCI6Imc0MU9yWkVUbVhndmZDTTAvRVo4eGpMSm9KdFlMYlRGeG5YSGhSMUtVNUEiLCJ0eXBlIjoiSldUIn0.eyJhdWQiOiJJUElRS1lZR1RQUlRNWE5ZSUtXS1VETFpaVkxKVFZZVFNKQVlQUFVaVlZGSVBFR1AiLCJzdWIiOiJhNjQzYzIxMS01MWQxLTRjNzktYmRiMS05NDQwZTNjYzYwNGIiLCJpc3MiOiJodHRwOi8vbG9jYWxob3N0OjgwODAiLCJleHAiOjE3Mjc3MTc1MDIsInNjb3BlIjoic3ViIGVtYWlsIG9wZW5pZCIsImNsaWVudF9pZCI6IklQSVFLWVlHVFBSVE1YTllJS1dLVURMWlpWTEpUVllUU0pBWVBQVVpWVkZJUEVHUCIsImlhdCI6MTcyNzcxMzkwMiwic2lkIjoiU0NLdExRcGtVbFlzYnZPamdPaUx0bHd6RGV4WE5ZIiwic2Vzc2lvbiI6IlNDS3RMUXBrVWxZc2J2T2pnT2lMdGx3ekRleFhOWSJ9.Q-K7O-vA9rymat6-YBK5B5KZL_rraDrLSROC--XW-EYNyWOdsMK9YRweld5nVrenmEp-UNzTAOFiestxTA_DD-Atd1R0PI_wMFaCEgVQWjrBofn5UMiUU6hL4BjXGd7nKUwrb4_G37A0wnz1Ud3ptV0gOjVR832IUZ32qy2H-migt_6zRJazQCv9uMji3dztFJOwL1KSgzrzj4pyKddSy87K59tRaX7MQFuk_10eyV25wx5w3kpMJc1f1vnJ2JtGyYAGSvT_ckyN2PFnDl6lZLxDptkKiFr_dLe8X8MUZMGyvAsNIRO7I5OsaaA01gP88sgMeCsOTGvjxU3MtGNRQw")


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


(defn token-error [status code & description]
  (log/debugf "Returning error: %s\n%s" code (str/join "\n" description))
  {:status (if (number? status) status 400)
   :headers {"Content-Type" "application/json;charset=UTF-8"
             "Pragma" "no-cache"
             "Cache-Control" "no-store"}
   :body (json/write-str
           {:error (if (number? status) code status)
            :error_description (str/join "\n"
                                         (if (number? status) description
                                           (conj description code)))})})


(comment
  (keys @core/*sessions*)
  (token-error
    400
    "authorization_pending"
    "Evo nekog opisa"))


(defn set-session-tokens
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
      (:tokens (core/get-session session)))))


(defn get-session-access-token
  ([session] (get-session-access-token session nil))
  ([session audience]
   (get-in @core/*sessions* [session :tokens audience :access_token])))


(defn get-session-refresh-token
  ([session] (get-session-refresh-token session nil))
  ([session audience]
   (get-in @core/*sessions* [session :tokens audience :refresh_token])))


(defn revoke-token
  ([token-key token]
   (let [session (get-token-session token-key token)
         audience (get-token-audience token-key token)]
     (swap! core/*sessions* update-in [session :tokens audience] dissoc token-key)
     (when token
       (swap! *tokens* update token-key dissoc token)
       (core/publish :revoke/token
                     {:token/key token-key
                      :token/data token
                      :audience audience
                      :session session})))
   nil))


(defn revoke-session-tokens
  ([session]
   (doseq [[audience] (core/get-session session)]
     (revoke-session-tokens session audience)))
  ([session audience]
   (let [{{tokens audience} :tokens} (core/get-session session)]
     (doseq [[token-key token] tokens]
       (revoke-token token-key token)))
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


(def authorization-code-not-supported
  (token-error
    "invalid_request"
    "The client configuration does not support"
    "token authorization code requests"))


(def device-code-not-supported
  (token-error
    "invalid_request"
    "The client configuration does not support"
    "token device code requests"))


(def client-credentials-not-supported
  (token-error
    "invalid_request"
    "The client configuration does not support"
    "token client credentials requests"))


(def cookie-session-missmatch
  (token-error
    "invalid_request"
    "You session is not provided by this server."
    "This action will be logged and processed!"))


(defmulti grant-token (fn [{:keys [grant_type]}] grant_type))

  
(defmethod grant-token :default [_] unsupported)


(defn- issued-at? [token] (:at (meta token)))


(defn generate
  [{{allowed-grants "allowed-grants"} :settings :as client} session {:keys [audience scope client_id]}]
  (let [access-exp (->
                     (System/currentTimeMillis)
                     (quot 1000)
                     (+ (access-token-expiry client)))
        {euuid :euuid} (core/get-session-resource-owner session)
        access-token {:session session
                      :aud (or audience client_id)
                      :exp access-exp
                      :iss (core/domain+)
                      :sub euuid
                      :iat (-> (vura/date) to-timestamp)
                      :client_id client_id
                      :sid session
                      :scope (str/join " " scope)}
        refresh? (some #(= "refresh_token" %) allowed-grants)]
    (log/debugf "Generated access token\n%s" (pprint access-token))
    (if (pos? access-exp)
      (let [refresh-token (when (and refresh? session (contains? scope "offline_access"))
                            (log/debugf "Creating refresh token: %s" session)
                            (gen-token))
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
        (when refresh-token (revoke-token :refresh_token refresh-token))
        (when session (set-session-tokens session audience signed-tokens))
        (comment
          (def client (get @core/*clients* #uuid "34c6eea5-3c95-4de1-a547-5e1b34ea16ea")))
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
        (assoc signed-tokens
               :expires_in (access-token-expiry client)
               :scope (str/join " " scope)
               :type "Bearer")))))


(defmethod grant-token "refresh_token"
  [{:keys [refresh_token scope audience]
    cookie-session :idsrv/session
    :as request}]
  ; (comment
  ;   (def refresh_token "rPDPDWYaoCLRdDrFIsdhiaGUMbVqvlXQslKIgoMMISSgQSSKbn")
  ;   (def session "MbLqqFaQUJnrXfdrmsoAOOEbCMswHg"))
  (let [session (get-token-session :refresh_token refresh_token)
        {{:strs [allowed-grants]} :settings :as client} (core/get-session-client session)
        {:keys [active]} (core/get-session-resource-owner session)
        scope (or
                scope
                (core/get-session-audience-scope session audience))
        audience (or
                   audience
                   (get-token-audience :refresh_token refresh_token))
        current-refresh-token (get-in
                                (core/get-session session)
                                [:tokens audience :refresh_token])
        grants (set allowed-grants)]
    (when session (revoke-session-tokens session audience))
    (cond
      ;;
      (not (contains? grants "refresh_token"))
      (do
        (core/kill-session session)
        refresh-not-supported)
      ;;
      (not active)
      (do
        (core/kill-session session)
        owner-not-authorized)
      ;;
      ;;
      (and cookie-session (not= cookie-session session))
      cookie-session-missmatch
      ;;
      (not= refresh_token current-refresh-token)
      (token-error
        400
        "invalid_request"
        "Provided token doesn't match session refresh token"
        "Your request will be logged and processed")
      ;;
      :else
      {:status 200
       :headers {"Content-Type" "application/json;charset=UTF-8"
                 "Pragma" "no-cache"
                 "Cache-Control" "no-store"}
       :body (json/write-str (generate client session (assoc request :scope scope)))})))


(comment
  (def request
    {:grant_type "authorization_code"
     :redirect_uri "http://localhost:3000/auth/callback",
     :client_id "GMTXXCLYXPIJHCLYHPVZDWWARYPMVLGIKFXKFAZNIGKLIDNJ"
     :client_secret "3yaUy6VkEYPxaAzHjJcLmWUVlRUGU89qpU6m07f-CWZZC1c8"
     :code "PEMECARHWZOMRAPYZMASEFGLFFRVZV"
     :scope nil}))

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


(defn delete [tokens]
  (swap! *tokens*
         (fn [state]
           (reduce-kv
             (fn [state token-type tokens-to-delete]
               (update state token-type #(apply dissoc % tokens-to-delete)))
             state
             tokens))))


;; SCOPES
(defmethod process-scope "roles"
  [session tokens _]
  (let [{:keys [roles]} (core/get-session-resource-owner session)
        dataset-roles (dataset/search-entity
                        iu/user-role
                        {:euuid {:_in roles}}
                        ; {:_where {:euuid {:_in roles}}}
                        {:name nil})]
    (assoc-in tokens [:access_token :roles] (map (comp csk/->snake_case_keyword :name) dataset-roles))))


(defmethod process-scope "roles:uuid"
  [session tokens _]
  (let [{:keys [roles]} (core/get-session-resource-owner session)]
    (assoc-in tokens [:access_token :roles] roles)))


(defmethod process-scope "permissions"
  [session tokens _]
  (let [{:keys [roles]} (core/get-session-resource-owner session)]
    (assoc-in tokens [:access_token :permissions] (access/roles-scopes roles))))

(comment
  (def tokens nil)
  (access/roles-scopes #{#uuid "8ebc60f1-8df0-48c8-a9b6-747a140df021"})
  (def session "RkJDHRzznXwlkatsVQnLWMmJHRWdyg"))

; (defn generate-code-challenge
;   ([code-verifier] (generate-code-challenge code-verifier "S256"))
;   ([code-verifier code-challenge-method]
;    (case code-challenge-method
;      "plain" code-verifier
;      "S256"
;      (let [bs (.getBytes code-verifier)
;            hashed (hash/sha256 bs)]
;        (-> hashed
;            codec/base64-encode
;            (.replace "+" "-")
;            (.replace "/" "_")
;            (.replace "=" ""))))))
;
;
; (def pkce-interceptor
;   {:enter
;    (fn [ctx]
;      (let [{{{:keys [code code_verifier grant_type]} :params} :request} ctx
;            {{:keys [code_challenge code_challenge_method]}
;             :request} (-> code
;                           ac/get-code-session 
;                           core/get-session)
;            is-pkce? (and code_challenge code_challenge_method)]
;        (if (or (not is-pkce?) (not= "authorization_code" grant_type)) ctx
;          (let [current-challenge (generate-code-challenge code_verifier code_challenge_method)]
;            (if (= current-challenge code_challenge) ctx
;              (chain/terminate
;                (core/json-error
;                  "invalid_request"
;                  "Proof Key for Code Exchange failed")))))))})
;
;
; (let [token (conj core/oauth-common-interceptor core/scope->set pkce-interceptor token-interceptor)
;       revoke (conj core/oauth-common-interceptor core/idsrv-session-read revoke-token-interceptor)]
;   (def routes
;     #{["/oauth/token" :post token :route-name ::handle-token]
;       ["/oauth/revoke" :post revoke :route-name ::post-revoke]
;       ["/oauth/revoke" :get revoke :route-name ::get-revoke]}))
