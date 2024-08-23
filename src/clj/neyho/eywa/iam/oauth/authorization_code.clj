(ns neyho.eywa.iam.oauth.authorization-code
  (:require
    [clojure.string :as str]
    clojure.java.io
    clojure.pprint
    [clojure.data.json :as json]
    [clojure.tools.logging :as log]
    [nano-id.core :as nano-id]
    [vura.core :as vura]
    [buddy.core.hash :as hash]
    [ring.util.codec :as codec]
    [io.pedestal.interceptor.chain :as chain]
    [neyho.eywa.iam
     :refer [get-client
             validate-password]]
    [neyho.eywa.iam.oauth.core :as core
     :refer [pprint
             publish]]
    [neyho.eywa.iam.oauth.token :as token
     :refer [grant-token
             token-error
             token-interceptor
             revoke-token-interceptor
             client-id-missmatch
             owner-not-authorized]]
    [neyho.eywa.iam.oauth.page.status :refer [status-page]]))


(defonce ^:dynamic *authorization-codes* (atom nil))


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
    (publish :grant/code {:session session :code code})
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


(defn get-code-session
  [code]
  (get-in @*authorization-codes* [code :session]))


(defn revoke-authorization-code
  ([code]
   (when code
     (let [session (get-code-session code)]
       (swap! *authorization-codes* dissoc code)
       (swap! core/*sessions* update session dissoc :code)
       (publish :revoke/code {:code code :session session})))))


(defn get-code-request
  [code]
  (get-in @*authorization-codes* [code :request]))





(defn code-was-issued? [code] (true? (get-in @*authorization-codes* [code :issued?])))


(defn validate-client [request]
  (let [{:keys [client_id state redirect_uri]
          request-secret :client_secret} request 
        base-redirect-uri (core/get-base-uri redirect_uri)
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
        (do
          (swap! core/*clients* assoc (:euuid client) client)
          client)
        (throw
          (ex-info
            "Client secret missmatch"
            {:type "access_denied"
             :request request
             :state state})))
      ;;
      (and (= type "public") (nil? secret))
      (do
        (swap! core/*clients* assoc (:euuid client) client)
        client)
      ;;
      :else
      (do
        (log/errorf "Couldn't validate client\n%s" (pprint request))
        (throw
          (ex-info "Unknown client error"
                   {:request request
                    :type "server_error"}))))))


; (defn prepare-code
;   ([session request]
;    (let [{:keys [code]} (core/get-session session)]
;      ;; Session has code bount
;      (if (and code
;               ;; and this code is valid
;               (contains? @*authorization-codes* code)
;               ;; and hasn't been shared to user
;               (not (code-was-issued? code)))
;        code
;        (let [new-code (gen-authorization-code)]
;          (swap! *authorization-codes*
;                 (fn [codes]
;                   (->
;                     codes
;                     (assoc new-code {:request request
;                                      :session session
;                                      :at (System/currentTimeMillis)})
;                     (dissoc code))))
;          (swap! core/*sessions* assoc-in [session :code] new-code)
;          code)))))
;
;
; (defn _authorization-code-flow
;   [{cookie-session :idsrv/session :as request
;     :keys [prompt redirect_uri state]}]
;   (let [now (System/currentTimeMillis)
;         session (if (core/get-session cookie-session) cookie-session
;                   (let [session (core/gen-session-id)]
;                     (core/set-session session {:flow "authorization_code" :at now})
;                     session))
;         silent? (and (some? cookie-session) (= prompt "none"))]
;     (cond
;       ;; Check that cookie session and session match
;       (and silent? cookie-session (not= cookie-session session))
;       (core/handle-request-error
;         {:request request
;          :type "login_required"
;          :state state
;          :description "Your session isn't authenticated. First log in"})
;       ;; Check that there isn't some other code active
;       (and silent? (contains? (core/get-session session) :code)) 
;       (core/handle-request-error
;         {:request request
;          :type "invalid_request"
;          :description "Your session has unused access code active"})
;       ;; When silent and above checks passed, than return directly to
;       ;; requested redirect_uri, with prepared authorization code
;       silent?
;       (try
;         (validate-client request)
;         (let [code (prepare-code session request)]
;           (mark-code-issued code)
;           {:status 302
;            :headers {"Location" (str redirect_uri "?" (codec/form-encode {:state state :code code}))}})
;         (catch clojure.lang.ExceptionInfo ex
;           (core/handle-request-error (ex-data ex))))
;       ;;
;       :else
;       (try
;         (let [client (validate-client request)]
;           ;; Proper implementation
;           (core/set-session-client session client)
;           ;; Prepare session code
;           (prepare-code session request)
;           (let [{{url "login-page"} :settings} (core/get-session-client session)]
;             {:status 302
;              :headers {"Location" (str url "?" (codec/form-encode
;                                                  {:state (core/encrypt
;                                                            {:session session
;                                                             :flow "authorization_code"})}))
;                        "Cache-Control" "no-cache"}}))
;         (catch clojure.lang.ExceptionInfo ex
;           (core/handle-request-error (ex-data ex)))))))


; (defn authorization-code-flow
;   [{cookie-session :idsrv/session :as request
;     :keys [prompt redirect_uri state]}]
;   (let [silent? (and (some? cookie-session) (= prompt "none"))]
;     (letfn [(save
;               ([code] (save code false))
;               ([code issued?]
;                (swap! *authorization-codes*
;                       (fn [codes]
;                         (assoc codes code {:request request
;                                            :issued? issued? 
;                                            :at (System/currentTimeMillis)})))))]
;       (cond
;         ;; Check that there isn't some other code active
;         (and silent? (contains? (core/get-session cookie-session) :code)) 
;         (core/handle-request-error
;           {:request request
;            :type "invalid_request"
;            :description "Your session has unused access code active"})
;         ;; When silent and above checks passed, than return directly to
;         ;; requested redirect_uri, with prepared authorization code
;         silent?
;         (try
;           (validate-client request)
;           (let [code (gen-authorization-code)]
;             (save code true)
;             {:status 302
;              :headers {"Location" (str redirect_uri "?" (codec/form-encode {:state state :code code}))}})
;           (catch clojure.lang.ExceptionInfo ex
;             (core/handle-request-error (ex-data ex))))
;         ;;
;         :else
;         (try
;           (validate-client request)
;           (let [code (gen-authorization-code)]
;             (save code)
;             {:status 302
;              :headers {"Location" (str "/oauth/login?" (codec/form-encode
;                                                          {:state (core/encrypt
;                                                                    {:authorization_code code
;                                                                     :flow "authorization_code"})}))
;                        "Cache-Control" "no-cache"}})
;           (catch clojure.lang.ExceptionInfo ex
;             (core/handle-request-error (ex-data ex))))))))


; (defn authorization-request
;   [request]
;   (log/debugf "Authorizing request:\n%s" request)
;   (letfn [(split-spaces [request k]
;             (if-some [val (get request k)]
;               (assoc request k (set (str/split val #"\s+")))
;               request))]
;     (let [{:keys [response_type redirect_uri]
;            :as request}
;           (-> request
;               (split-spaces :scope)
;               (split-spaces :response_type))]
;       (cond
;         (empty? redirect_uri)
;         (core/handle-request-error
;           {:type "missing_redirect"
;            :request request})
;         ;;
;         (empty? response_type)
;         (core/handle-request-error
;           {:type "missing_response_type"
;            :request request})
;         ;;
;         (contains? response_type "code")
;         (authorization-code-flow request)
;         ;;
;         :else
;         (core/handle-request-error
;           {:type "server_error"
;            :request request})))))


(defmethod grant-token "authorization_code"
  [request]
  (let [{:keys [code redirect_uri client_id client_secret grant_type]} request
        {{request-redirect-uri :redirect_uri
          :as original-request} :request
         client-euuid :client
         :keys [:session expires-at]} (get @*authorization-codes* code) 
        {id :id :as client} (get @core/*clients* client-euuid)
        {:keys [active]} (core/get-session-resource-owner session)]
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
          ;;
          (< expires-at (System/currentTimeMillis))
          (token-error
            "invalid_request"
            "This device code has expired. Restart"
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
            (delete code)
            (core/kill-session session)
            owner-not-authorized)
          ;; Issue that token
          :else
          (let [response (json/write-str (token/generate client session original-request))]
            (revoke-authorization-code code)
            {:status 200
             :headers {"Content-Type" "application/json;charset=UTF-8"
                       "Pragma" "no-cache"
                       "Cache-Control" "no-store"}
             :body response}))))))



(defn mark-code-issued [session code]
  (swap! *authorization-codes* update code
         (fn [data]
           (assoc data
                  :issued? true
                  :session session
                  :expires-at (-> 
                                (System/currentTimeMillis)
                                (+ (vura/minutes 5)))))))


(def start-authorization-code-flow
  {:name ::authorize-request
   :enter
   (fn [{{request :params
          :keys [remote-addr]
          {:keys [user-agent]} :headers} :request :as ctx}]
     (log/debugf "Authorizing request:\n%s" request)
     (letfn [(split-spaces [request k]
               (if-some [val (get request k)]
                 (assoc request k (set (str/split val #"\s+")))
                 request))
             (error [data]
               (chain/terminate
                 (assoc ctx :response (core/handle-request-error data))))]
       (let [{:keys [response_type redirect_uri]
              :as request}
             (-> request
                 (split-spaces :scope)
                 (split-spaces :response_type))]
         (cond
           (empty? redirect_uri)
           (error {:type "missing_redirect"
                   :request request})
           ;;
           (empty? response_type)
           (error {:type "missing_response_type"
                   :request request})
           ;;
           (contains? response_type "code")
           (let [{cookie-session :idsrv/session :as request
                  :keys [prompt redirect_uri state]} request
                 ;;
                 silent? (and (some? cookie-session) (= prompt "none"))]
             (letfn [(save
                       ([code data]
                        (swap! *authorization-codes*
                               (fn [codes]
                                 (assoc codes code (merge data {:request request}))))))]
               (cond
                 ;; Check that there isn't some other code active
                 (and silent? (contains? (core/get-session cookie-session) :code)) 
                 (error
                   {:request request
                    :type "invalid_request"
                    :description "Your session has unused access code active"})
                 ;; When silent and above checks passed, than return directly to
                 ;; requested redirect_uri, with prepared authorization code
                 silent?
                 (try

                   (let [{client-euuid :euuid} (validate-client request)
                         code (gen-authorization-code)]
                     (save code {:issued? true
                                 :client client-euuid
                                 :user/agent user-agent
                                 :user/ip remote-addr})
                     (mark-code-issued cookie-session code)
                     (assoc ctx
                            ::code code
                            :respone {:status 302
                                      :headers {"Location" (str redirect_uri "?" (codec/form-encode {:state state :code code}))}}))
                   (catch clojure.lang.ExceptionInfo ex
                     (core/handle-request-error (ex-data ex))))
                 ;;
                 :else
                 (try
                   (let [code (gen-authorization-code)
                         {client-euuid :euuid} (validate-client request)]
                     (save code {:client client-euuid
                                 :user/agent user-agent
                                 :user/ip remote-addr})
                     (assoc ctx
                            :code code
                            :response {:status 302
                                       :headers {"Location" (str "/oauth/login?" (codec/form-encode
                                                                                   {:state (core/encrypt
                                                                                             {:authorization-code code
                                                                                              :evo "ti kurac"
                                                                                              :flow "authorization_code"})}))
                                                 "Cache-Control" "no-cache"}}))
                   (catch clojure.lang.ExceptionInfo ex
                     (error (ex-data ex)))))))
           ;;
           :else
           (error {:type "server_error"
                   :request request})))))})


;; TODO - change this to point to /oauth/status
(def authorize-request-error-interceptor
  {:name ::authorize-error
   :enter
   (fn [{{:keys [params]} :request :as context}]
     (chain/terminate
       (assoc context :response
              {:status 200
               :headers {"Content-Type" "text/html"}
               :body (if-some [description (get core/request-errors (:type params))]
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
           {{:keys [code_challenge code_challenge_method]}
            :request} (-> code
                          get-code-session 
                          core/get-session)
           is-pkce? (and code_challenge code_challenge_method)]
       (if (or (not is-pkce?) (not= "authorization_code" grant_type)) ctx
         (let [current-challenge (generate-code-challenge code_verifier code_challenge_method)]
           (if (= current-challenge code_challenge) ctx
             (chain/terminate
               (core/json-error
                 "invalid_request"
                 "Proof Key for Code Exchange failed")))))))})


(let [token (conj core/oauth-common-interceptor core/scope->set pkce-interceptor token-interceptor)
      revoke (conj core/oauth-common-interceptor core/idsrv-session-read revoke-token-interceptor)]
  (def routes
    #{["/oauth/token" :post token :route-name ::handle-token]
      ["/oauth/revoke" :post revoke :route-name ::post-revoke]
      ["/oauth/revoke" :get revoke :route-name ::get-revoke]
      ["/oauth/authorize" :get (conj core/oauth-common-interceptor core/idsrv-session-read start-authorization-code-flow)
     :route-name ::authorize-request]
      ["/oauth/status" :get status-page :route-name ::oauth-status]}))






