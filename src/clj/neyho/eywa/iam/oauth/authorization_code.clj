(ns neyho.eywa.iam.oauth.authorization-code
  (:require
    [clojure.string :as str]
    clojure.java.io
    clojure.pprint
    [clojure.tools.logging :as log]
    [nano-id.core :as nano-id]
    [ring.util.codec :as codec]
    [io.pedestal.interceptor.chain :as chain]
    [neyho.eywa.iam
     :refer [get-client
             validate-password]]
    [neyho.eywa.iam.oauth.core :as core
     :refer [pprint publish]]))


(defonce ^:dynamic *authorization-codes* (atom nil))


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



(defn mark-code-issued [code] (swap! *authorization-codes* assoc-in [code :issued?] true))


(defn code-was-issued? [code] (true? (get-in @*authorization-codes* [code :issued?])))


(defn prepare-code
  ([session request]
   (let [{:keys [code]} (core/get-session session)]
     ;; Session has code bount
     (if (and code
              ;; and this code is valid
              (contains? @*authorization-codes* code)
              ;; and hasn't been shared to user
              (not (code-was-issued? code)))
       code
       (let [new-code (gen-authorization-code)]
         (swap! *authorization-codes*
                (fn [codes]
                  (->
                    codes
                    (assoc new-code {:request request
                                     :session session
                                     :at (System/currentTimeMillis)})
                    (dissoc code))))
         (swap! core/*sessions* assoc-in [session :code] new-code)
         code)))))



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
        client
        (throw
          (ex-info
            "Client secret missmatch"
            {:type "access_denied"
             :request request
             :state state})))
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


(defn authorization-code-flow
  [{cookie-session :idsrv/session :as request
    :keys [prompt redirect_uri state]}]
  ; (def request request)
  ; (def cookie-session (get request :idsrv/session))
  ; (def now (System/currentTimeMillis))
  (let [now (System/currentTimeMillis)
        session (if (core/get-session cookie-session) cookie-session
                  (let [session (core/gen-session-id)]
                    (core/set-session session {:flow "authorization_code" :at now})
                    session))
        silent? (and (some? cookie-session) (= prompt "none"))]
    (cond
      ;; Check that cookie session and session match
      (and silent? cookie-session (not= cookie-session session))
      (core/handle-request-error
        {:request request
         :type "login_required"
         :state state
         :description "Your session isn't authenticated. First log in"})
      ;; Check that there isn't some other code active
      (and silent? (contains? (core/get-session session) :code)) 
      (core/handle-request-error
        {:request request
         :type "invalid_request"
         :description "Your session has unused access code active"})
      ;; When silent and above checks passed, than return directly to
      ;; requested redirect_uri, with prepared authorization code
      silent?
      (try
        (validate-client request)
        (let [code (prepare-code session request)]
          (mark-code-issued code)
          {:status 302
           :headers {"Location" (str redirect_uri "?" (codec/form-encode {:state state :code code}))}})
        (catch clojure.lang.ExceptionInfo ex
          (core/handle-request-error (ex-data ex))))
      ;;
      :else
      (try
        (let [client (validate-client request)]
          ;; Proper implementation
          (core/set-session-client session client)
          ;; Prepare session code
          (prepare-code session request)
          (let [{{url "login-page"} :settings} (core/get-session-client session)]
            {:status 302
             :headers {"Location" (str url "?" (codec/form-encode
                                                 {:state (core/encrypt
                                                           {:session session
                                                            :flow "authorization_code"})}))
                       "Cache-Control" "no-cache"}}))
        (catch clojure.lang.ExceptionInfo ex
          (core/handle-request-error (ex-data ex)))))))


(defn authorization-request
  [request]
  (log/debugf "Authorizing request:\n%s" request)
  (letfn [(split-spaces [request k]
            (if-some [val (get request k)]
              (assoc request k (set (str/split val #"\s+")))
              request))]
    (let [{:keys [response_type redirect_uri]
           :as request}
          (-> request
              (split-spaces :scope)
              (split-spaces :response_type))]
      (cond
        (empty? redirect_uri)
        (core/handle-request-error
          {:type "missing_redirect"
           :request request})
        ;;
        (empty? response_type)
        (core/handle-request-error
          {:type "missing_response_type"
           :request request})
        ;;
        (contains? response_type "code")
        (authorization-code-flow request)
        ;;
        :else
        (core/handle-request-error
          {:type "server_error"
           :request request})))))


(def authorize-request-interceptor
  {:name ::authorize-request
   :enter
   (fn [{{:keys [params]} :request :as context}]
     (chain/terminate
       (assoc context :response
              (authorization-request params))))})


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
