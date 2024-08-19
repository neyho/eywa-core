(ns neyho.eywa.iam.oauth.login
  (:require
    clojure.java.io
    clojure.pprint
    [clojure.set :as set]
    [clojure.tools.logging :as log]
    [clojure.java.io :as io]
    [vura.core :as vura]
    [ring.util.codec :as codec]
    [ring.util.response :as response]
    [io.pedestal.interceptor.chain :as chain]
    [io.pedestal.http.body-params :as bp]
    [neyho.eywa.iam.oauth.core :as core]
    [neyho.eywa.iam.oauth.authorization-code :as ac]
    [neyho.eywa.iam.oauth.device-code :as dc]
    [neyho.eywa.iam.oauth.page.login :refer [login-html]]))


(def redirect-to-login
  {:enter (fn [ctx]
            (chain/terminate
              (assoc ctx :response
                     {:status 302
                      :headers {"Location" (str "/oauth/login/index.html")
                                "Cache-Control" "no-cache"}})))})


(defn build-login-interceptor
  [{:keys [on-credentials-error]}]
  {:name ::login
   :enter
   (fn [{{params :params :as request} :request :as context}]
     (let [{:keys [form-params]} (bp/form-parser request)
           data (merge params form-params)
           {:keys [username password]
            {:keys [session flow]} :state} (update data :state (fn [x] (when x (core/decrypt x))))]
       (chain/terminate
         (assoc context :response
                (let [{:keys [code] :as session-state} (core/get-session session)
                      resource-owner (core/validate-resource-owner username password)]
                  (case flow
                    "authorization_code"
                    (let [{response_type :response_type
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
                          ;;
                          (nil? session-state)
                          (core/handle-request-error {:type "corrupt_session" :session session})
                          ;;
                          (and resource-owner (set/intersection #{"code" "authorization_code"} response_type))
                          (do
                            (log/debugf "[%s] Binding code to session" code)
                            (core/set-session-resource-owner session resource-owner)
                            (core/set-session-authorized-at session (vura/date))
                            (ac/mark-code-issued code)
                            (log/debugf "[%s] Validating resource owner: %s" session username)
                            (attach-session-cookie
                              {:status 302
                               :headers {"Location" (str redirect-uri "?" (codec/form-encode {:state state :code code}))}}))
                          ;;
                          :else (on-credentials-error)
                          #_(login-html {:error "Invalid credentials"})
                          #_(let [{{url "login-page"} :settings} (core/get-session-client session)]
                              {:status 302
                               :headers {"Location" (str url "?" (codec/form-encode
                                                                   {:session session
                                                                    :error ["credentials"]}))
                                         "Cache-Control" "no-cache"}}))))
                    "device_code"
                    nil))))))})


(def login-interceptor
  (build-login-interceptor
    {:on-credentials-error (fn []
                             {:status 400
                              :headers {"Content-Type" "text/html"}
                              :body (str (login-html {:error "Credentials invalid. Check username and password and try again"}))})}))


(defn build-login-page-interceptor
  ([] (build-login-page-interceptor (login-html)))
  ([page]
   {:enter (fn [{{:keys [uri]
                  {:keys [session]
                   idsrv-session :idsrv/session} :params} :request :as ctx}]
             (let [ext (re-find #"(?<=\.).*?$" uri)
                   {:keys [code]} (core/get-session session)
                   {{redirect-uri :redirect_uri
                     :keys [state prompt]} :request} (ac/get-code-request code)
                   response (letfn [(revoke-idsrv [response]
                                      (assoc-in response [:cookies "idsrv.session"]
                                                {:value ""
                                                 :path "/"
                                                 :max-age 0}))]
                              (cond
                                ;; First check if there is active session
                                (and (= session idsrv-session) (= prompt "none"))
                                (let [code (ac/bind-authorization-code session)]
                                  {:status 302
                                   :headers {"Location" (str redirect-uri "?" (codec/form-encode {:state state :code code}))}})
                                ;; If there is session but it wasn't created by EYWA
                                ;; return error
                                ;; (nil? (oauth/get-session session))
                                ;; (request-error 400 "Target session doesn't exist")
                                ;; Then check if some file was requested
                                (and ext (not= uri "/oauth/login/index.html")) 
                                (revoke-idsrv (response/resource-response uri))
                                ;; Finally return index.html if you don't know what
                                ;; to do
                                :else
                                (revoke-idsrv
                                  {:status 200
                                   :headers {"Content-Type" "text/html"}
                                   :body (str page)})))]
               (assoc ctx :response response)))}))


(def serve-login-page (build-login-page-interceptor (login-html)))
