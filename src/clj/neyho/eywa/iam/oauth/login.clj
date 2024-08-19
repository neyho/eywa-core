(ns neyho.eywa.iam.oauth.login
  (:require
    clojure.java.io
    clojure.pprint
    [clojure.set :as set]
    [clojure.tools.logging :as log]
    [vura.core :as vura]
    [ring.util.codec :as codec]
    [io.pedestal.interceptor.chain :as chain]
    [io.pedestal.http.body-params :as bp]
    [neyho.eywa.iam.oauth.core :as core]
    [neyho.eywa.iam.oauth.authorization-code :as ac]
    [neyho.eywa.iam.oauth.device-code :as dc]))


(def redirect-to-login
  {:enter (fn [ctx]
            (chain/terminate
              (assoc ctx :response
                     {:status 302
                      :headers {"Location" (str "/oidc/login/index.html")
                                "Cache-Control" "no-cache"}})))})


(defn login
  [{:keys [username password]
    {:keys [session flow]} :state}]
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
            :else
            (let [{{url "login-page"} :settings} (core/get-session-client session)]
              {:status 302
               :headers {"Location" (str url "?" (codec/form-encode
                                                   {:session session
                                                    :error ["credentials"]}))
                         "Cache-Control" "no-cache"}}))))
      "device_code"
      nil)))


(def login-interceptor
  {:name ::login
   :enter
   (fn [{{params :params :as request} :request :as context}]
     (let [{:keys [form-params]} (bp/form-parser request)
           data (merge params form-params)
           data (update data :state (fn [x] (when x (core/decrypt x))))]
       (chain/terminate
         (assoc context :response (login data)))))})
