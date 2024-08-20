(ns neyho.eywa.iam.oauth.device-code
  (:require
    [clojure.string :as str]
    clojure.java.io
    clojure.pprint
    [vura.core :as vura]
    [clojure.tools.logging :as log]
    [clojure.data.json :as json]
    [nano-id.core :as nano-id]
    [ring.util.codec :as codec]
    [io.pedestal.interceptor.chain :as chain]
    [io.pedestal.http.body-params :as bp]
    [neyho.eywa.iam.oauth.core :as core
     :refer [pprint
             basic-authorization-interceptor
             keywordize-params
             encrypt
             decrypt]]
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
     (fn [{{:keys [params remote-addr]
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
                              :user/ip remote-addr
                              :interval 5
                              :client client_id})
                      {:status 200
                       :headers {"Content-Type" "application/json"}
                       :body (json/write-str
                               {:device_code device-code
                                :user_code user-code
                                :verification_uri (core/domain+ "/oauth/device/activate")
                                :verification_uri_complete (core/domain+ (str "/oauth/device/activate?user_code=" user-code))
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



(def ^{:doc "This interceptor should be used for device activation. Both
            for GET and POST methods. When GET is intercepted ctx will
            be processed to forward device code data for further processing
            that is rendering.

            Keys:
              ::complete?    - varification_url complete or not
              ::device-code  - found device code
              ::challenge    - challenge that should be served to user when
                               verification_url_complete is handled. This
                               challenge will be in POST processing
              ::error        - found error. One of
                               [:not-available
                                :malicius-code
                                :malicius-ip
                                :malicius-user-agent
                                :unknown-action
                                :no-challenge]
              ::canceled?    - in case of validation_uri_complete, user has
                               confirmed that it was he that requested device
                               code flow
            
            When POST method is intercepted, data is preprocessed and in case
            of valid verification_uri request chain is terminated. Device code
            flow is redirected to confirm page.
            
            If request isn't valid ctx will be associated"}
  device-activate-interceptor
  {:enter (fn [{{:keys [remote-addr]
                 {:keys [challenge action user_code]} :params
                 {:strs [user-agent]} :headers
                 method :request-method} :request :as ctx}]
            (letfn [(find-device-code []
                      (some
                        (fn [[device-code {:keys [user-code]}]]
                          (when (= user_code user-code)
                            device-code))
                        @*device-codes*))
                    (redirect-to-login [{:keys [device-code] :as state}]
                      (let [challenge (nano-id/nano-id 20)]
                        (swap! *device-codes* update device-code
                               (fn [data]
                                 (assoc-in data [:challenges challenge] state)))
                        (assoc ctx :response
                               {:status 302
                                :headers {"Location" (str "/oauth/login?"
                                                          (codec/form-encode
                                                            {:state (encrypt
                                                                      (assoc state
                                                                             :flow :device-code
                                                                             :challenge challenge))}))
                                          "Cache-Control" "no-cache"}})))]
              (case method
                ;;
                :get
                ;; If there is some user_code provided
                (if (some? user_code)
                  ;; Then try to find device code for given user code
                  (if-let [device-code (find-device-code)]
                    ;; if device code is found, than create challenge that will
                    ;; be served to end user for confirm action
                    (assoc ctx
                           ::complete? true
                           ::device-code device-code
                           ::user-code user_code
                           ::challenge (encrypt {:user-code user_code
                                                 :device-code device-code
                                                 :ip remote-addr
                                                 :user-agent user-agent}))
                    ;; when device code isn't found associate ctx with error
                    (assoc ctx ::error :device-code/not-available))
                  ;; If complete validation uri isn't target, just mark ctx
                  ;; so that page render can know what to display
                  (assoc ctx ::complete? false))
                ;;
                :post
                (cond
                  ;; Missing challenge... Handle error in next interceptor
                  (and (some? user_code) (nil? challenge))
                  (assoc ctx ::error :no-challenge)
                  ;; There is user code so it is verification_uri_complete
                  (some? user_code)
                  (let [{:keys [device-code user-code ip]
                         :as decrypted-challenge} (decrypt challenge)
                        real-code (get-in @*device-codes* [device-code :user-code])]
                    (cond
                      ;; Check user code
                      (not= real-code user-code user_code)
                      (assoc ctx ::error :malicius-code)
                      ;; Check IP address
                      (not= remote-addr ip)
                      (assoc ctx ::erorr :malicious-ip)
                      ;; Check user agent
                      (not= user-agent (:user-agent decrypted-challenge))
                      (assoc ctx ::error :malicius-user-agent)
                      ;; If everything was ok than and confirmed
                      ;; Than handle device session creation and remove device
                      ;; code so that it won't be used again
                      (= action "confirm")
                      (redirect-to-login
                        {:device-code device-code
                         :ip ip
                         :user-agent user-agent})
                      ;; If canceled than remove device code and forward ctx
                      ;; to render
                      (= action "cancel")
                      (do
                        (assoc ctx ::user-code user-code ::canceled? true))
                      ;;
                      :else (assoc ctx
                                   ::user-code user-code
                                   ::error :unknown-action)))
                  ;; Otherwise it is verification_uri, with user typeing in
                  ;; URI manually
                  :else
                  (if-let [device-code (find-device-code)]
                    (redirect-to-login
                      {:device-code device-code
                       :ip remote-addr
                       :user-agent user-agent})
                    (assoc ctx
                           ::user-code user_code
                           ::error :not-available))))))})


(def user-code-page
  {:enter (fn [ctx]
            (assoc ctx :response {:status (if (::error ctx) 400 200)
                                  :headers {"Content-Type" "text/html"}
                                  :body (str (device/authorize ctx))}))})



(def user-code-confirm-page
  {:enter (fn [ctx]
            (assoc ctx :response {:status (if (::error ctx) 400 200) :body "hi"}))})


(let [common [basic-authorization-interceptor
              middleware/cookies
              (bp/body-params)
              keywordize-params]
      device-code (conj common device-code-flow-interceptor)]
  (def routes
    #{["/oauth/device/auth" :post device-code :route-name ::get-device-flow]
      ["/oauth/device/activate" :get [device-activate-interceptor user-code-page] :route-name ::user-code-page]
      ["/oauth/device/activate" :post  (conj common device-activate-interceptor user-code-page) :route-name ::user-code-check]
      ["/oauth/device/activate/success" :get [device-activate-interceptor user-code-page] :route-name ::user-code-activated]
      ["/oauth/device/activate/canceled" :get [device-activate-interceptor check-user-code-interceptor] :route-name ::user-code-canceled]
      ;;
      ["/oauth/device/confirm" :get [user-code-confirm-page] :route-name ::serve-confirm-page]
      ["/oauth/device/confirm" :post [confirm-device-interceptor] :route-name ::confirm-device]}))
