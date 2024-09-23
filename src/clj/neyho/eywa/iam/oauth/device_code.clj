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
    [neyho.eywa.iam.oauth.core :as core
     :refer [pprint
             get-client
             encrypt
             decrypt]]
    [neyho.eywa.iam
     :refer [validate-password]]
    [neyho.eywa.iam.oauth.token :as token
     :refer [grant-token
             token-error
             client-id-missmatch
             owner-not-authorized]]
    [neyho.eywa.iam.oauth.page.device :as device]))


(defonce ^:dynamic *device-codes* (atom nil))


(defn delete [code]
  (swap! *device-codes* dissoc code))


(def gen-device-code (nano-id/custom "ACDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz" 20))
; (def gen-user-code (nano-id/custom "0123456789" 6))
(let [gen-par (nano-id/custom "ACDEFGHIJKLMNOPQRSTUVWXYZ" 4)]
  (defn gen-user-code []
    (str (gen-par) \- (gen-par))))


(defn get-device-code-data [device-code]
  (get @*device-codes* device-code))


(def grant "urn:ietf:params:oauth:grant-type:device_code")


(defn validate-client [request]
  (let [{:keys [client_id]
         request-secret :client_secret} request 
        {:keys [euuid secret type]
         {:strs [allowed-grants]} :settings
         :as client} (get-client client_id)
        grants (set allowed-grants)]
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
      (not (contains? grants grant))
      (throw
        (ex-info
          "Client doesn't support device_code flow"
          {:type "access_denied"
           :request request}))
      ;;
      :else
      (do
        (log/errorf "Couldn't validate client\n%s" (pprint request))
        (throw
          (ex-info "Unknown client error"
                   {:request request
                    :type "server_error"}))))))



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


(defmethod grant-token "urn:ietf:params:oauth:grant-type:device_code"
  [request]
  (let [{:keys [device_code client_secret]} request
        {{:keys [client_id] :as original-request
          id :client_id} :request
         :keys [session]} (get @*device-codes* device_code)
        client (core/get-client client_id)]
    (log/debugf
      "[%s] Processing token code grant for code: %s\n%s"
      id device_code (pprint request))
    (def request request)
    (def device_code (:device_code request))
    (def client_id (:client_id request))
    (let [{_secret :secret
           {:strs [allowed-grants]} :settings} (core/get-client client_id) 
          grants (set allowed-grants)]
      (cond
        ;;
        (not (contains? @*device-codes* device_code))
        (token-error
          "invalid_request"
          "Provided device code is illegal!"
          "Your request will be logged"
          "and processed")
        ;;
        (not (contains? grants grant))
        (token-error
          "unauthorized_grant"
          "Client sent access token request"
          "for grant type that is outside"
          "of client configured privileges")
        ;;
        (nil? session)
        (token-error
          403
          "authorization_pending"
          "The authorization request is still pending as"
          "the end user hasn't yet completed the user-interaction steps")
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
        ;;
        (not= id client_id)
        client-id-missmatch
        ;;
        (nil? session)
        {:status 403
         :headers {"Content-Type" "application/json"}
         :body (json/write-str
                 {:error "authorization_pending"
                  :error_description "The authorization request is still pending as the end user hasn't yet completed the user-interaction steps"})}
        ;; Issue that token
        :else
        (let [response (json/write-str (token/generate client session original-request))]
          (swap! *device-codes* dissoc device_code)
          ; (core/set-session-audience-scope session audience scope)
          {:status 200
           :headers {"Content-Type" "application/json;charset=UTF-8"
                     "Pragma" "no-cache"
                     "Cache-Control" "no-store"}
           :body response})))))


(def ^{:doc "This interceptor will start device code flow. It will create device code
            and store it for latter usage. Also other info is stored, like:

              :device/agent
              :device/ip
              :client
              :request
              :user-code
              :expires-at
              :interval
            
            You can get this information by calling (get-device-code-data)"}
  start-device-flow
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
                    (let [client (validate-client params)
                          expires-at (-> 
                                       (System/currentTimeMillis)
                                       (+ (vura/minutes 5)))]
                      (swap! core/*clients* assoc (:euuid client) client)
                      (swap! *device-codes* assoc device-code
                             {:user-code user-code
                              :expires-at expires-at
                              :request params
                              :device/agent user-agent
                              :device/ip remote-addr
                              :interval 5
                              :client (:euuid client)})
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
                                :malicous-code
                                :malicous-ip
                                :malicous-user-agent
                                :unknown-action
                                :no-challenge]
              ::canceled?    - in case of validation_uri_complete, user has
                               confirmed that it was he that requested device
                               code flow
            
            When POST method is intercepted, data is preprocessed and in case
            of valid verification_uri request chain is terminated. Device code
            flow is redirected to confirm page.
            
            If request isn't valid ctx will be associated"}
  device-interceptor
  {:enter (fn [{{:keys [remote-addr query-params]
                 {:keys [challenge action user_code]} :params
                 {:strs [user-agent]} :headers
                 method :request-method} :request :as ctx}]
            (letfn [(find-device-code [user_code]
                      (some
                        (fn [[device-code {:keys [user-code]}]]
                          (when (= user_code user-code)
                            device-code))
                        @*device-codes*))
                    (redirect-to-login [{:keys [device-code] :as state}]
                      (let [challenge (nano-id/nano-id 20)]
                        (swap! *device-codes* update device-code
                               (fn [data]
                                 (assoc-in data [:challenges challenge] (dissoc state :device-code))))
                        (assoc ctx :response
                               {:status 302
                                :headers {"Location" (str "/oauth/login?"
                                                          (codec/form-encode
                                                            {:state (encrypt
                                                                      (assoc state
                                                                             :flow :device_code
                                                                             :challenge challenge))}))
                                          "Cache-Control" "no-cache"}})))
                    (redirect-to-canceled
                      [{:keys [user-code device-code]}]
                      (swap! *device-codes* dissoc device-code)
                      (chain/terminate
                        (assoc :response
                               {:status 302
                                :headers {"Location" (str "/oauth/device/status?value=canceled&user_code=" user-code)
                                          "Cache-Control" "no-cache"}})))]
              (let [complete? (boolean (:user_code query-params))]
                (case method
                  ;;
                  :get
                  ;; If there is some user_code provided
                  (if (some? user_code)
                    ;; Then try to find device code for given user code
                    (if-let [device-code (find-device-code user_code)]
                      ;; if device code is found, than create challenge that will
                      ;; be served to end user for confirm action
                      (assoc ctx
                             ::complete? complete?
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
                    (assoc ctx ::complete? complete?))
                  ;;
                  :post
                  (cond
                    ;; Missing challenge... Handle error in next interceptor
                    (and complete? (nil? challenge))
                    (assoc ctx ::error :no-challenge)
                    ;; There is user code so it is verification_uri_complete
                    complete?
                    (let [{:keys [device-code user-code ip]
                           :as decrypted-challenge} (decrypt challenge)
                          {real-code :user-code
                           :keys [expires-at]} (get @*device-codes* device-code)
                          now (System/currentTimeMillis)]
                      (cond
                        ;;
                        (< expires-at now)
                        (assoc ctx ::error :expired)
                        ;; Check user code
                        (not= real-code user-code user_code)
                        (assoc ctx ::error :malicous-code)
                        ;; Check IP address
                        (not= remote-addr ip)
                        (assoc ctx ::erorr :malicious-ip)
                        ;; Check user agent
                        (not= user-agent (:user-agent decrypted-challenge))
                        (assoc ctx ::error :malicous-user-agent)
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
                        (redirect-to-canceled
                          {:user-code user-code
                           :device-code device-code})
                        ;;
                        :else (assoc ctx
                                     ::user-code user-code
                                     ::error :unknown-action)))
                    ;; Otherwise it is verification_uri, with user typeing in
                    ;; URI manually
                    :else
                    (if-let [device-code (find-device-code user_code)]
                      (redirect-to-login
                        {:device-code device-code
                         :ip remote-addr
                         :user-agent user-agent})
                      (assoc ctx
                             ::user-code user_code
                             ::error :not-available)))))))})


(def user-code-page
  {:enter (fn [ctx]
            (assoc ctx :response {:status (if (::error ctx) 400 200)
                                  :headers {"Content-Type" "text/html"}
                                  :body (str (device/authorize ctx))}))})



(def user-code-confirm-page
  {:enter (fn [ctx]
            (assoc ctx :response {:status (if (::error ctx) 400 200) :body "hi"}))})


(let [device-code (conj core/oauth-common-interceptor start-device-flow)]
  (def routes
    #{["/oauth/device/auth" :post device-code :route-name ::get-device-flow]
      ["/oauth/device/activate" :get [device-interceptor user-code-page] :route-name ::user-code-page]
      ["/oauth/device/activate" :post  (conj core/oauth-common-interceptor device-interceptor user-code-page) :route-name ::user-code-check]
      ;;
      ["/oauth/device/confirm" :get [user-code-confirm-page] :route-name ::serve-confirm-page]
      ["/oauth/device/confirm" :post [user-code-confirm-page] :route-name ::confirm-device]}))



