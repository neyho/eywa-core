(ns neyho.eywa.iam.oauth
  (:require
   [clojure.string :as str]
   clojure.java.io
   clojure.pprint
   [clojure.tools.logging :as log]
   [vura.core :as vura]
   [buddy.core.hash :as hash]
   [ring.util.codec :as codec]
   [io.pedestal.interceptor.chain :as chain]
   [io.pedestal.http.body-params :as bp]
   [neyho.eywa.iam.oauth.core :as core]
   [neyho.eywa.iam.oauth.token :as token
    :refer [token-interceptor
            revoke-token-interceptor]]
   [neyho.eywa.iam.oauth.authorization-code
    :as authorization-code
    :refer [*authorization-codes*
            gen-authorization-code
            get-code-session
            validate-client
            mark-code-issued]]
   [neyho.eywa.iam.oauth.device-code :as device-code]
   [neyho.eywa.iam.oauth.page.status :refer [status-page]]))

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
           {{:keys [code_challenge code_challenge_method] :as request}
            :request} (-> code
                          get-code-session
                          core/get-session)
           is-pkce? (and code_challenge code_challenge_method)]
       (when is-pkce?
         (log/debug "Noticed PKCE in requst: %s" request))
       (if (or (not is-pkce?) (not= "authorization_code" grant_type)) ctx
           (let [current-challenge (generate-code-challenge code_verifier code_challenge_method)]
             (if (= current-challenge code_challenge) ctx
                 (chain/terminate
                  (core/json-error
                   "invalid_request"
                   "Proof Key for Code Exchange failed")))))))})

(def authorization-request
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
                                 :created-on (System/currentTimeMillis)
                                 :user/agent user-agent
                                 :user/ip remote-addr})
                     (mark-code-issued cookie-session code)
                     (assoc ctx
                       ::code code
                       :respone {:status 302
                                 :headers {"Location" (str redirect_uri "?"
                                                           (codec/form-encode
                                                            (cond->
                                                             {:code code}
                                                              (not-empty state) (assoc :state state))))}}))
                   (catch clojure.lang.ExceptionInfo ex
                     (core/handle-request-error (ex-data ex))))
                 ;;
                 :else
                 (try
                   (let [code (gen-authorization-code)
                         {client-euuid :euuid} (validate-client request)
                         location (str "/oauth/login?"
                                       (codec/form-encode
                                        {:state (core/encrypt
                                                 {:authorization-code code
                                                  :flow "authorization_code"})}))]
                     (save code {:client client-euuid
                                 :created-on (System/currentTimeMillis)
                                 :user/agent user-agent
                                 :user/ip remote-addr})
                     (assoc ctx
                       :code code
                       :response {:status 302
                                  :headers {"Location" location
                                            "Cache-Control" "no-cache"}}))
                   (catch clojure.lang.ExceptionInfo ex
                     (error (ex-data ex)))))))
           ;;
           :else
           (error {:type "server_error"
                   :request request})))))})

(defonce maintenance-agent (agent {:running true :period (vura/seconds 30)}))

(comment
  (agent-error maintenance-agent)
  (restart-agent maintenance-agent @maintenance-agent)
  (start)
  (stop))

(defn maintenance
  [{:keys [running period] :as data}]
  (when (and running period)
    (log/debug "[OAuth] Maintenance start")
    (send-off *agent* maintenance)
    (core/clean-sessions)
    (authorization-code/clean-codes)
    (device-code/clean-expired-codes)
    (core/monitor-client-change)
    (log/debug "[OAuth] Maintenance finish")
    (Thread/sleep period))
  data)

(defn start
  []
  (send-off maintenance-agent assoc :running true :period (vura/seconds 30))
  (send-off maintenance-agent maintenance))

(defn stop
  []
  (send-off maintenance-agent assoc :running false)
  (doseq [x [core/*resource-owners* core/*clients*
             core/*sessions* token/*tokens*]]
    (reset! x nil)))

(def state-interceptor
  {:name ::state
   :enter
   (fn [{{params :params :as request}
         :request :as ctx}]
     (let [{:keys [form-params]} (bp/form-parser request)
           data (merge params form-params)
           ;;
           {state :state} (update data :state (fn [x] (when x (core/decrypt x))))]
       (assoc ctx ::state state)))})

(let [token (conj core/oauth-common-interceptor core/scope->set pkce-interceptor token-interceptor)
      revoke (conj core/oauth-common-interceptor core/idsrv-session-read revoke-token-interceptor)]
  (def routes
    #{["/oauth/token" :post token :route-name ::handle-token]
      ["/oauth/revoke" :post revoke :route-name ::post-revoke]
      ["/oauth/revoke" :get revoke :route-name ::get-revoke]
      ["/oauth/authorize" :get (conj core/oauth-common-interceptor core/idsrv-session-read authorization-request) :route-name ::authorize-request]
      ["/oauth/status" :get (conj state-interceptor status-page) :route-name ::oauth-status]}))
