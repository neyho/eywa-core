(ns neyho.eywa.iam.oauth.device-code
  (:require
    [clojure.string :as str]
    clojure.java.io
    clojure.pprint
    [vura.core :as vura]
    [clojure.tools.logging :as log]
    [clojure.data.json :as json]
    [nano-id.core :as nano-id]
    [io.pedestal.interceptor.chain :as chain]
    [io.pedestal.http.body-params :as bp]
    [neyho.eywa.iam.oauth.core :as core
     :refer [pprint
             basic-authorization-interceptor
             keywordize-params]]
    [neyho.eywa.iam
     :refer [get-client
             validate-password]]
    [neyho.eywa.iam.oauth.page.device :as device]
    [io.pedestal.http.ring-middlewares :as middleware]))


(defonce ^:dynamic *device-codes* (atom nil))


(def gen-device-code (nano-id/custom "ACDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz" 20))
; (def gen-user-code (nano-id/custom "0123456789" 6))
(let [gen-par (nano-id/custom "ACDEFGHIJKLMNOPQRSTUVWXYZ" 4)]
  (defn gen-user-code []
    (str (gen-par) \- (gen-par))))


(defn validate-client [request]
  (let [{:keys [client_id]
         request-secret :client_secret} request 
        {:keys [euuid secret type]
         :as client} (get-client client_id)]
    (log/debugf "Validating client: %s" (pprint client))
    (cond
      ;;
      (nil? euuid)
      (throw
        (ex-info
          "Client not registered"
          {:type "client_not_registered"
           :request request}))
      ;;
      (or (some? request-secret) (some? secret))
      (if (validate-password request-secret secret)
        client
        (throw
          (ex-info
            "Client secret missmatch"
            {:type "access_denied"
             :request request})))
      ;;
      (and (= type "public") (nil? secret))
      client
      ;;
      :else
      (do
        (log/errorf "Couldn't validate client\n%s" (pprint request))
        (throw
          (ex-info "Unknown client error"
                   {:request request
                    :type "server_error"}))))))


(def activate-device-code
  {:name ::activate-device-code
   :enter
   (fn [])})


(defn code-expired? [code]
  (when-some [{:keys [expires-at]} (get-in @*device-codes* code)]
    (< expires-at (System/currentTimeMillis))))


(defn clean-expired-codes
  []
  (let [now (System/currentTimeMillis)
        expired (keep
                  (fn [[device-code {:keys [expires-at]}]]
                    (when (< expires-at now) device-code))
                  @*device-codes*)]
    (log/infof
      "Revoking expired device codes:\n%s"
      (str/join "\n" expired))
    (swap! *device-codes* (fn [codes] (apply dissoc codes expired)))))


(def device-code-flow-interceptor
  {:enter
   (letfn [(split-spaces [request k]
             (if-some [val (get request k)]
               (assoc request k (set (str/split val #"\s+")))
               request))]
     (fn [{{:keys [params remote-address]
            {:strs [user-agent]} :headers
            :as request} :request :as context}]
       (let [{:keys [client_id]} params
             params (-> params (split-spaces :scope))
             device-code (gen-device-code)
             user-code (gen-user-code)]
         (log/debugf "Device code request:\n%s" request)
         (chain/terminate
           (assoc context :response
                  (try
                    (validate-client params)
                    (let [expires-at (-> 
                                       (System/currentTimeMillis)
                                       (+ (vura/minutes 5)))]
                      (swap! *device-codes* assoc device-code
                             {:user-code user-code
                              :expires-at expires-at
                              :user/agent user-agent
                              :user/ip remote-address
                              :interval 5
                              :client client_id})
                      {:status 200
                       :headers {"Content-Type" "application/json"}
                       :body (json/write-str
                               {:device_code device-code
                                :user_code user-code
                                :verification_uri (core/domain+ "/oauth/activate-device")
                                :verification_uri_complete (core/domain+ (str "/oauth/activate-device?user-code=" user-code))
                                :interval 5
                                :expires_in 900})})
                    (catch clojure.lang.ExceptionInfo ex
                      (core/handle-request-error (ex-data ex)))))))))})


(def check-user-code-interceptor
  {:enter (fn [ctx]
            (assoc ctx :response {:status 200 :body "hi"}))})


(def confirm-device-interceptor
  {:enter (fn [ctx]
            (assoc ctx :response {:status 200 :body "hi"}))})


(defn build-user-code-page
  []
  {:enter (fn [ctx]
            (assoc ctx :response {:status 200
                                  :headers {"Content-Type" "text/html"}
                                  :body (str (device/authorize))}))})

(def serve-user-code-page
  (build-user-code-page))

(def serve-confirm-page
  {:enter (fn [ctx]
            (assoc ctx :response {:status 200 :body "hi"}))})


(let [common [basic-authorization-interceptor
              middleware/cookies
              (bp/body-params)
              keywordize-params]
      device-code (conj common device-code-flow-interceptor)]
  (def routes
    #{["/oauth/device/auth" :post device-code :route-name ::get-device-flow]
      ["/oauth/device/activate" :get [serve-user-code-page] :route-name ::serve-check-user-code]
      ["/oauth/device/activate" :post [check-user-code-interceptor] :route-name ::check-user-code]
      ["/oauth/device/confirm" :get [serve-confirm-page] :route-name ::serve-confirm-page]
      ["/oauth/device/confirm" :post [confirm-device-interceptor] :route-name ::confirm-device]}))
