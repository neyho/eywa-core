(ns neyho.eywa.iam.oauth.login
  (:require
   clojure.java.io
   clojure.pprint
   [environ.core :refer [env]]
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
   [io.pedestal.http.cors :refer [allow-origin]]
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
   (fn [{{params :params
          method :request-method :as request}
         :request :as ctx}]
     (let [{:keys [form-params]} (bp/form-parser request)
           data (merge params form-params)
           ;;
           {:keys [username password]
            {:keys [flow device-code authorization-code]
             :as flow-state} :state}
           (update data :state (fn [x] (when x (core/decrypt x))))]
       (case method
         ;; Passthrough
         :get (assoc ctx ::state flow-state)
         ;;
         :post
         (let [resource-owner (core/validate-resource-owner username password)]
           (case flow
             "authorization_code"
             (let [{{response_type :response_type
                     redirect-uri :redirect_uri
                     :keys [state audience scope]} :request
                    :keys [client] :as prepared-code}
                   (get @ac/*authorization-codes* authorization-code)
                   ;;
                   session (core/gen-session-id)
                   now (vura/date)]
               (cond
                 ;;
                 (nil? prepared-code)
                 (chain/terminate
                  (assoc ctx :response
                         {:status 302
                          :headers {"Location" (str "/oauth/status" "?" (codec/form-encode
                                                                         {:value "error"
                                                                          :flow "authorization_code"
                                                                          :error "expired_code"
                                                                          :error_description "Your login grace period has expired. Return to your client application and retry login procedure"}))}}))
                 ;; When user isn't authenticated, leave for some other interceptor
                 ;; to return response
                 (nil? resource-owner)
                 (assoc ctx ::state flow-state ::error :credentials)
                 ;;
                 ;; When everything is OK
                 ;;
                 (and resource-owner (set/intersection #{"code" "authorization_code"} response_type))
                 (do
                   (log/debugf "[%s] Binding code %s to session" session authorization-code)
                   (core/set-session session {:flow "authorization_code"
                                              :code  authorization-code
                                              :client client
                                              :last-active now})
                   (core/set-session-audience-scope session audience scope)
                   (core/set-session-resource-owner session resource-owner)
                   (core/set-session-authorized-at session now)
                   (ac/mark-code-issued session authorization-code)
                   (log/debugf "[%s] Validating resource owner: %s" session username)
                   (iam/publish
                    :oauth.session/created
                    {:session session
                     :code authorization-code
                     :client client
                     :audience audience
                     :scope scope
                     :user resource-owner})
                   (chain/terminate
                    (->
                     (assoc ctx
                       ::state flow-state
                       ::session session
                       :response {:status 302
                                  :headers {"Location" (str redirect-uri "?"
                                                            (codec/form-encode
                                                             (if (empty? state) {:code authorization-code}
                                                                 {:state state
                                                                  :code authorization-code})))}}))))
                 ;; Otherwise let someone after handle error
                 :else
                 (assoc ctx ::state flow-state ::error :unknown)))
             "device_code"
             ;; If user authenticated than do your stuff
             (let [{:keys [session client expires-at]
                    {:keys [audience scope]} :request} (get @dc/*device-codes* device-code)
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
                     (iam/publish
                      :oauth.session/created
                      {:session session
                       :code device-code
                       :client client
                       :audience audience
                       :scope scope
                       :user resource-owner})
                     (chain/terminate
                      (assoc ctx :response
                             {:status 302
                              :headers {"Location" (format
                                                    "/oauth/status?value=success&client=%s&user=%s"
                                                    client (:name resource-owner))}}))))))
             (assoc ctx :response
                    {:status 302
                     :headers {"Location" "/oauth/status?value=error&error=broken_flow"}}))))))
   ;; TODO - decide about this idsrv.session in cookie
   ;; If user has multiple sessions in same browser this doesn't make any sense
   :leave (fn [{:keys [::session] :as ctx}]
            (if-not session ctx
                    (assoc-in ctx [:response :cookies "idsrv.session"]
                              {:value session
                               :path "/"
                               :http-only true
                               :secure true
                               :same-site :none
                               :expires "Session"})))})
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
      ; invalid-session (request-error 400 "Session is not valid")
      invalid-redirect (request-error 400 "Provided 'post_logout_redirect_uri' is not valid")]
  (def logout-interceptor
    {:enter
     (fn [ctx]
       (let [{{{:keys [post_logout_redirect_uri id_token_hint state]
                idsrv-session :idsrv/session} :params} :request} ctx
             session (or (token/get-token-session :id_token id_token_hint)
                         idsrv-session)
             {client_id :id} (core/get-session-client session)
             {{valid-redirections "logout-redirections"} :settings} (core/get-client client_id)
             post-redirect-ok? (some #(when (= % post_logout_redirect_uri) true) valid-redirections)
             response (cond
                        (nil? session)
                        session-not-found
                        ;; Token couldn't be unsigned
                        (and (nil? id_token_hint) (nil? idsrv-session))
                        invalid-token
                        ;; Session doesn't match
                        ; (and id_token_hint (not= idsrv-session session))
                        ; invalid-session
                        ;; Redirect uri isn't valid
                        (not post-redirect-ok?)
                        invalid-redirect
                        ;;
                        (some? post_logout_redirect_uri)
                        (let [{:keys [code flow]} (core/get-session session)
                              tokens (core/get-session-tokens session)]
                          ((case flow
                             "authorization_code"
                             ac/delete
                             "device_code"
                             dc/delete)
                           code)
                          (token/delete tokens)
                          (core/kill-session session)
                          {:status 302
                           :headers {"Location" (str post_logout_redirect_uri
                                                     (when (not-empty state)
                                                       (str "?" (codec/form-encode {:state state}))))
                                     "Cache-Control" "no-cache"}})
                        ;;
                        :else
                        (let [{:keys [code flow]} (core/get-session session)
                              tokens (core/get-session-tokens session)]
                          ((case flow
                             "authorization_code"
                             ac/delete
                             "device_code"
                             dc/delete) code)
                          (token/delete tokens)
                          (core/kill-session session)
                          {:status 200
                           :headers {"Content-Type" "text/html"}
                           :body "User logged out!"}))]
         ; (def session session)
         ; (def id_token_hint id_token_hint)
         ; (def iss iss)
         ; (def sid sid)
         ; (def post-redirect-ok? post-redirect-ok?)
         ; (def post_logout_redirect_uri)
         ; (def response response)
         (assoc ctx :response response)))}))

(let [logout (conj core/oauth-common-interceptor core/idsrv-session-remove core/idsrv-session-read logout-interceptor)
      only-identity-provider (allow-origin {:allowed-origins
                                            (conj
                                             (remove empty? (str/split (env :eywa-allowed-origins "") #"\s*,\s*"))
                                              ;; Bellow is compatibility
                                             (env :eywa-iam-root-url "http://localhost:8080"))})]
  (def routes
    #{["/oauth/login" :get [only-identity-provider redirect-to-login] :route-name ::short-login]
      ["/oauth/login/index.html" :post [only-identity-provider middleware/cookies login-interceptor login-page] :route-name ::handle-login]
      ["/oauth/login/index.html" :get (conj core/oauth-common-interceptor only-identity-provider login-interceptor login-page) :route-name ::short-login-redirect]
      ;; Login resources
      ["/oauth/login/icons/*" :get [only-identity-provider core/serve-resource] :route-name ::login-images]
      ["/oauth/icons/*" :get [only-identity-provider core/serve-resource] :route-name ::login-icons]
      ["/oauth/css/*" :get [only-identity-provider core/serve-resource] :route-name ::login-css]
      ["/oauth/js/*" :get [only-identity-provider core/serve-resource] :route-name ::login-js]
      ;; Logout logic
      ["/oauth/logout" :post logout :route-name ::post-logout]
      ["/oauth/logout" :get  logout :route-name ::get-logout]}))
