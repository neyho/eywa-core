(ns neyho.eywa.iam.oauth.login
  (:require
    clojure.java.io
    clojure.pprint
    [clojure.string :as str]
    [clojure.set :as set]
    [clojure.tools.logging :as log]
    [vura.core :as vura]
    [ring.util.codec :as codec]
    [clojure.data.json :as json]
    [neyho.eywa.iam :as iam]
    [neyho.eywa.iam.oauth.core :as core]
    [neyho.eywa.iam.oauth.authorization-code :as ac]
    [neyho.eywa.iam.oauth.device-code :as dc]
    [neyho.eywa.iam.oauth.page.login :refer [login-html]]
    [neyho.eywa.iam.oauth.token :as token]
    [io.pedestal.interceptor.chain :as chain]
    [io.pedestal.http.body-params :as bp]
    [io.pedestal.http.ring-middlewares :as middleware]))


(def redirect-to-login
  {:enter (fn [{{query :query-string} :request :as ctx}]
            (chain/terminate
              (assoc ctx :response
                     {:status 302
                      :headers {"Location" (str "/oauth/login/index.html" (when query (str "?" query)))
                                "Cache-Control" "no-cache"}})))})


(defn security-check
  [{{params :params
     current-ip :remote-addr
     {current-user-agent "user-agent"} :headers
     :as request} :request}]
  (let [{:keys [form-params]} (bp/form-parser request)
        data (merge params form-params)
        ;;
        {{:keys [flow ip user-agent challenge device-code]} :state}
        (update data :state (fn [x] (when x (core/decrypt x))))]
    (cond
      ;;
      (not= ip current-ip)
      "ip_address"
      ;;
      (not= user-agent current-user-agent)
      "user_agent"
      ;;
      (not
        (contains?
          (case flow
            "device_code" (get-in @dc/*device-codes* [device-code :challenges])
            nil)
          challenge))
      "challenge"
      ;;
      :else nil)))



(def login-interceptor
  {:name ::login
   :enter
   (fn [{{{idsrv-session :idsrv/session
           :as params} :params
          method :request-method :as request}
         :request :as ctx}]
     (let [{:keys [form-params]} (bp/form-parser request)
           data (merge params form-params)
           ;;
           {:keys [username password]
            {:keys [session flow device-code]} :state}
           (update data :state (fn [x] (when x (core/decrypt x))))]
       (case method
         ;;
         :get
         (let [{:keys [code]} (core/get-session session)
               {{redirect-uri :redirect_uri
                 :keys [state prompt]} :request} (ac/get-code-request code)]
           (letfn [(revoke-idsrv [response]
                     (assoc-in response [:cookies "idsrv.session"]
                               {:value ""
                                :path "/"
                                :max-age 0}))]
             ;; First check if there is active session
             (if (and (= flow "authorization_code") (= session idsrv-session) (= prompt "none"))
               (let [code (ac/bind-authorization-code session)]
                 (chain/terminate
                   (assoc ctx :response
                          {:status 302
                           :headers {"Location" (str redirect-uri "?" (codec/form-encode {:state state :code code}))}})))
               ctx)))
         ;;
         :post
         (let [resource-owner (core/validate-resource-owner username password)]
           (case flow
             "authorization_code"
             (let [{:keys [code] :as session-state} (core/get-session session)
                   {response_type :response_type
                    redirect-uri :redirect_uri
                    :keys [state]} (ac/get-code-request code)]
               (letfn [(attach-session-cookie [ctx]
                         (assoc-in ctx [:cookies "idsrv.session"]
                                   {:value session
                                    :path "/"
                                    :http-only true
                                    :secure true
                                    :expires "Session"}))]
                 (cond
                   ;; When session is wrong or corrupted
                   (nil? session-state)
                   (core/handle-request-error {:type "corrupt_session" :session session})
                   ;; When user isn't authenticated, leave for some other interceptor
                   ;; to return response
                   (nil? resource-owner)
                   (assoc ctx ::error :credentials)
                   ;; When everything is OK
                   (and resource-owner (set/intersection #{"code" "authorization_code"} response_type))
                   (do
                     (log/debugf "[%s] Binding code to session" code)
                     (core/set-session-resource-owner session resource-owner)
                     (core/set-session-authorized-at session (vura/date))
                     (ac/mark-code-issued code)
                     (log/debugf "[%s] Validating resource owner: %s" session username)
                     (chain/terminate
                       (-> ctx
                           attach-session-cookie
                           (assoc :response
                                  {:status 302
                                   :headers {"Location" (str redirect-uri "?" (codec/form-encode {:state state :code code}))}}))))
                   ;; Otherwise let someone after handle error
                   :else
                   (assoc ctx ::error :unknown))))
             "device_code"
             ;; If user authenticated than do your stuff
             (let [{:keys [session client scope expires-at]
                    {:keys [audience]} :request} (get @dc/*device-codes* device-code)
                   security-error (security-check ctx)]
               (letfn [(error [value]
                         (chain/terminate
                           (assoc ctx :response
                                  {:status 302
                                   :headers {"Location" (str "/oauth/status" "?" (codec/form-encode
                                                                                   {:value "error"
                                                                                    :flow "device_code"
                                                                                    :error value}))}})))]
                 (cond
                   (some? session)
                   (error "already_authorized")
                   ;;
                   (< expires-at (System/currentTimeMillis))
                   (error "device_code_expired")
                   ;;
                   (some? security-error)
                   (error security-error)
                   ;;
                   (nil? resource-owner)
                   (assoc ctx ::error :credentials)
                   ;;
                   :else
                   ;; TODO - this should actually redirect to confirm page
                   ;; but for now implicit confirm is default
                   (let [session (core/gen-session-id)
                         now (vura/date)]
                     (log/debugf "[%s] Device authorized for session id %s" device-code session)
                     (swap! dc/*device-codes* update device-code
                            (fn [data]
                              (->
                                data
                                (assoc 
                                  :confirmed false
                                  :session session)
                                (dissoc :challenges))))
                     ;;
                     (core/set-session session {:flow "device_code"
                                                :code device-code
                                                :client client
                                                :last-active now})
                     (core/set-session-audience-scope session audience scope)
                     (core/set-session-resource-owner session resource-owner)
                     (core/set-session-authorized-at session now)
                     (chain/terminate
                       (assoc ctx :response
                              {:status 302
                               :headers {"Location" "/oauth/device/activate/success"}})))))))))))})


(def login-page
  {:enter (fn [ctx]
            (assoc ctx :response {:status (if (::error ctx) 400 200)
                                  :headers {"Content-Type" "text/html"}
                                  :body (str (login-html ctx))}))})


(defn request-error
  [code & description]
  {:status code
   :headers {"Content-Type" "text/html"}
   :body (json/write-str (str/join "\n" description))})


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
             session (or (token/get-token-session :id_token id_token_hint)
                         idsrv-session)
             {client_id :id} (core/get-session-client session)
             {{valid-redirections "logout-redirections"} :settings} (iam/get-client client_id)
             {:keys [iss sid] :as token} (try
                                           (iam/unsign-data id_token_hint)
                                           (catch Throwable _ nil))
             post-redirect-ok? (some #(when (= % post_logout_redirect_uri) true) valid-redirections)]
         (assoc ctx :response
                (cond
                  (nil? session)
                  (core/idsrv-session-remove session-not-found)
                  ;; Token couldn't be unsigned
                  (and id_token_hint (nil? token))
                  invalid-token
                  ;; Session doesn't match
                  (and id_token_hint (not= sid session))
                  invalid-session
                  ;; Issuer is not the same
                  (and id_token_hint (not= iss (core/domain+)))
                  invalid-issuer
                  ;; Redirect uri isn't valid
                  (not post-redirect-ok?)
                  invalid-redirect
                  ;;
                  (some? post_logout_redirect_uri)
                  (do
                    (core/kill-session session)
                    {:status 302
                     :headers {"Location" (str post_logout_redirect_uri (when (not-empty state) (str "?" (codec/form-encode {:state state}))))
                               "Cache-Control" "no-cache"}})
                  ;;
                  :else
                  (do
                    (core/kill-session session)
                    {:status 200
                     :headers {"Content-Type" "text/html"}
                     :body "User logged out!"})))))}))


(let [logout (conj core/oauth-common-interceptor core/idsrv-session-remove core/idsrv-session-read logout-interceptor)]
  (def routes
    #{["/oauth/login" :get [redirect-to-login] :route-name ::short-login]
      ["/oauth/login/index.html" :post [middleware/cookies login-interceptor login-page] :route-name ::handle-login]
      ["/oauth/login/index.html" :get (conj core/oauth-common-interceptor core/idsrv-session-read login-interceptor login-page) :route-name ::short-login-redirect]
      ;; Login resources
      ["/oauth/login/icons/*" :get [core/serve-resource] :route-name ::login-images]
      ["/oauth/icons/*" :get [core/serve-resource] :route-name ::login-icons]
      ["/oauth/css/*" :get [core/serve-resource] :route-name ::login-css]
      ["/oauth/js/*" :get [core/serve-resource] :route-name ::login-js]
      ;; Logout logic
      ["/oauth/logout" :post logout :route-name ::get-logout]
      ["/oauth/logout" :get  logout :route-name ::post-logout]}))

(comment
  (ns-unmap 'neyho.eywa.iam.oauth.login 'login-page))


; (defn build-login-interceptor
;   [{:keys [on-credentials-error]}]
;   {:name ::login
;    :enter
;    (fn [{{params :params :as request} :request :as ctx}]
;      (let [{:keys [form-params]} (bp/form-parser request)
;            data (merge params form-params)
;            {:keys [username password]
;             {:keys [session flow]} :state} (update data :state (fn [x] (when x (core/decrypt x))))]
;        (chain/terminate
;          (assoc ctx :response
;                 (let [{:keys [code] :as session-state} (core/get-session session)
;                       resource-owner (core/validate-resource-owner username password)]
;                   (case flow
;                     "authorization_code"
;                     (let [{response_type :response_type
;                            redirect-uri :redirect_uri
;                            :keys [state]} (ac/get-code-request code)]
;                       (letfn [(attach-session-cookie [ctx]
;                                 (assoc-in ctx [:cookies "idsrv.session"]
;                                           {:value session
;                                            :path "/"
;                                            :http-only true
;                                            :secure true
;                                            :expires "Session"}))]
;                         (cond
;                           ;;
;                           (nil? session-state)
;                           (core/handle-request-error {:type "corrupt_session" :session session})
;                           ;;
;                           (and resource-owner (set/intersection #{"code" "authorization_code"} response_type))
;                           (do
;                             (log/debugf "[%s] Binding code to session" code)
;                             (core/set-session-resource-owner session resource-owner)
;                             (core/set-session-authorized-at session (vura/date))
;                             (ac/mark-code-issued code)
;                             (log/debugf "[%s] Validating resource owner: %s" session username)
;                             (attach-session-cookie
;                               {:status 302
;                                :headers {"Location" (str redirect-uri "?" (codec/form-encode {:state state :code code}))}}))
;                           ;;
;                           :else (on-credentials-error)
;                           #_(login-html {:error "Invalid credentials"})
;                           #_(let [{{url "login-page"} :settings} (core/get-session-client session)]
;                               {:status 302
;                                :headers {"Location" (str url "?" (codec/form-encode
;                                                                    {:session session
;                                                                     :error ["credentials"]}))
;                                          "Cache-Control" "no-cache"}}))))
;                     "device_code"
;                     nil))))))})


; (defn build-login-page-interceptor
;   ([] (build-login-page-interceptor (login-html)))
;   ([page]
;    {:enter (fn [{{:keys [uri]
;                   {:keys [session]
;                    idsrv-session :idsrv/session} :params} :request :as ctx}]
;              (let [ext (re-find #"(?<=\.).*?$" uri)
;                    {:keys [code]} (core/get-session session)
;                    {{redirect-uri :redirect_uri
;                      :keys [state prompt]} :request} (ac/get-code-request code)
;                    response (letfn [(revoke-idsrv [response]
;                                       (assoc-in response [:cookies "idsrv.session"]
;                                                 {:value ""
;                                                  :path "/"
;                                                  :max-age 0}))]
;                               (cond
;                                 ;; First check if there is active session
;                                 (and (= session idsrv-session) (= prompt "none"))
;                                 (let [code (ac/bind-authorization-code session)]
;                                   {:status 302
;                                    :headers {"Location" (str redirect-uri "?" (codec/form-encode {:state state :code code}))}})
;                                 ;; If there is session but it wasn't created by EYWA
;                                 ;; return error
;                                 ;; (nil? (oauth/get-session session))
;                                 ;; (request-error 400 "Target session doesn't exist")
;                                 ;; Then check if some file was requested
;                                 (and ext (not= uri "/oauth/login/index.html")) 
;                                 (revoke-idsrv (response/resource-response uri))
;                                 ;; Finally return index.html if you don't know what
;                                 ;; to do
;                                 :else
;                                 (revoke-idsrv
;                                   {:status 200
;                                    :headers {"Content-Type" "text/html"}
;                                    :body (str page)})))]
;                (assoc ctx :response response)))}))
;
;
; (def serve-login-page (build-login-page-interceptor (login-html)))
