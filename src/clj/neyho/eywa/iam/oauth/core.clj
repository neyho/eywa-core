(ns neyho.eywa.iam.oauth.core
  (:require
    [clojure.string :as str]
    clojure.java.io
    clojure.pprint
    [clojure.core.async :as async]
    [clojure.tools.logging :as log]
    [clojure.walk :refer [keywordize-keys]]
    [clojure.spec.alpha :as s]
    [nano-id.core :as nano-id]
    [vura.core :as vura]
    [ring.util.codec :as codec]
    [ring.util.response :as response]
    [clojure.data.json :as json]
    [buddy.core.codecs :as codecs]
    [buddy.core.crypto :as crypto]
    [neyho.eywa.iam
     :refer [get-client
             validate-password
             get-user-details]]
    [neyho.eywa.env :as env])
  (:import
    [java.util Base64]))


(defn pprint [data] (with-out-str (clojure.pprint/pprint data)))


(defonce subscription (async/chan (async/sliding-buffer 10000)))
(defonce publisher
  (async/pub
    subscription
    (fn [{:keys [topic]
          :or {topic ::broadcast}}]
      topic)))


(defn publish [topic data]
  (async/put! subscription (assoc data :topic topic)))


(defonce ^:dynamic *resource-owners* (atom nil))
(defonce ^:dynamic *clients* (atom nil))
(defonce ^:dynamic *sessions* (atom nil))


(defn domain+
  ([] (domain+ ""))
  ([path]
   (str env/iam-protocol "://" env/iam-domain path)))



(defonce ^:dynamic *encryption-key* (nano-id/nano-id 32))
(defonce ^:dynamic *initialization-vector* (nano-id/nano-id 12))

;
; (defn encrypt
;   [data]
;   (String. (codecs/bytes->b64 (nippy/freeze data {:password [:salted *encryption-key*]}))))
;
;
; (defn decrypt
;   [data]
;   (nippy/thaw (codecs/b64->bytes data) {:password [:salted *encryption-key*]}))


(defn encrypt
  [data]
  (let [json-data (.getBytes (json/write-str data))]
    (String.
      (codecs/bytes->b64
        (crypto/encrypt
          json-data *encryption-key* *initialization-vector*
          {:alg :aes256-gcm})))))


(defn decrypt [encrypted-data]
  (json/read-str
    (String.
      (crypto/decrypt
        (codecs/b64->bytes encrypted-data) *encryption-key* *initialization-vector*
        {:alg :aes256-gcm}))
    :key-fn keyword))


(comment
  (time
    (decrypt
      (encrypt
        {:device-code 100
         :user-code 200
         :ip "a"
         :user-agent "jfioq"}))))





(let [alphabet "ACDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"]
  (def gen-session-id (nano-id/custom alphabet 30)))


(let [default (vura/hours 2)]
  (defn access-token-expiry
    [{{{expiry "access"} "token-expiry"} :settings}]
    (or expiry default)))


(let [default (vura/days 1.5)]
  (defn refresh-token-expiry
    [{{{expiry "refresh"} "token-expiry"} :settings}]
    (or expiry default)))


(defmulti sign-token (fn [_ token-key _] token-key))


(defmethod sign-token :default
  [session token-key data]
  (log/errorf "[%s] Couldn't signt token `%s`" session token-key)
  data)


(defmulti process-scope (fn [_ _ scope] scope))


(defmethod process-scope :default
  [session tokens scope]
  (log/errorf "[%s] Couldn't find scope resolver for scope `%s`" session scope)
  tokens)


(defn get-session-client [session]
  (let [euuid (get-in @*sessions* [session :client])]
    (get @*clients* euuid)))


(defn get-session-resource-owner [session]
  (let [euuid (get-in @*sessions* [session :resource-owner])]
    (get @*resource-owners* euuid)))


(defn token-error [code & description]
  ; (log/debugf "Returning error: %s\n%s" code (str/join "\n" description))
  {:status 400
   :headers {"Content-Type" "application/json;charset=UTF-8"
             "Pragma" "no-cache"
             "Cache-Control" "no-store"}
   :body (json/write-str
           {:error code
            :error_description (str/join "\n" description)})})


(defn get-base-uri
  "Returns the base URI without query parameters from the given URL."
  [url]
  (when (not-empty url)
    (let [uri (java.net.URI. url)]
      (str (java.net.URI. (.getScheme uri) (.getAuthority uri) (.getPath uri) nil nil)))))


(defn validate-resource-owner [username password]
  (let [{db-password :password
         active :active
         :as resource-owner} (get-user-details username)]
    (if-not active nil
      (when (validate-password password db-password)
        (dissoc resource-owner :password)))))


(defn set-session-resource-owner
  [session {:keys [euuid] :as resource-owner}]
  (swap! *sessions* assoc-in [session :resource-owner] euuid)
  (swap! *resource-owners* update euuid
         (fn [current]
           (->
             current 
             (merge resource-owner)
             (update :sessions (fnil conj #{}) session))))
  nil)


(defn remove-session-resource-owner [session]
  (let [euuid (get-in @*sessions* [session :resource-owner])]
    (swap! *sessions* update session dissoc :resource-owner)
    (when euuid
      (swap! *resource-owners* update euuid
             (fn [current]
               (update current :sessions (fnil disj #{}) session))))
    nil))


(defn set-session-client [session {:keys [euuid] :as client}]
  (swap! *sessions* assoc-in [session :client] euuid)
  (swap! *clients* update euuid
         (fn [current]
           (->
             current 
             (merge client)
             (update :sessions (fnil conj #{}) session))))
  nil)


(defn clients-match? [session {:keys [client_id client_secret]}]
  (let [{known-id :id
         known-secret :secret} (get-session-client session)]
    (cond
      (not= client_id known-id) false
      (and client_secret (not= client_secret known-secret)) false
      (and known-secret (not= client_secret known-secret)) false
      :else true)))


(def clients-doesnt-match? (complement clients-match?))




(defn remove-session-client [session]
  (let [euuid (get-in @*sessions* [session :client])]
    (swap! *sessions* update session dissoc :client)
    (when euuid
      (swap! *clients* update euuid
             (fn [current]
               (update current :sessions (fnil disj #{}) session))))
    nil))


(defn get-session [id] (get @*sessions* id))
(defn set-session [id data] (swap! *sessions* assoc id data))
(defn remove-session [id]
  (let [{{client-euuid :euuid :as client} :client
         {resource-owner-euuid :euuid :as resource-owner} :resource-owner} (get-session id)]
    (swap! *sessions* dissoc id)
    ;; Remove session from resource owner
    (when resource-owner
      (swap! *resource-owners* update resource-owner-euuid
             (fn [current]
               (update current :sessions (fnil disj #{}) id))))
    ;; Remove session from client
    (when client
      (swap! *clients* update client-euuid
             (fn [current]
               (update current :sessions (fnil disj #{}) id))))
    nil))


(defn get-redirection-uris [session]
  (let [{{:strs [redirections]} :settings} (get-session-client session)]
    redirections))


(def request-errors
  (reduce-kv
    (fn [r k v]
      (assoc r k (str/join "\n" v)))
    nil
    {"invalid_request"
     ["The request is missing a required parameter, includes an"
      "invalid parameter value, includes a parameter more than" 
      "once, or is otherwise malformed."]
     ;;
     "unauthorized_client"
     ["The client is not authorized to request an authorization"
      "code using this method."]
     ;;
     "access_denied"
     ["The resource owner or authorization server denied the"
      "request."]
     ;;
     "unsupported_response_type"
     ["The authorization server does not support obtaining an"
      "authorization code using this method."]
     ;;
     "invalid_scope"
     ["The requested scope is invalid, unknown, or malformed."]
     ;;
     "server_error"
     ["The authorization server encountered an unexpected"
      "condition that prevented it from fulfilling the request."]
     ;;
     "temporarily_unavailable"
     ["The authorization server is currently unable to handle"
      "the request due to a temporary overloading or maintenance"
      "of the server."]}))


(defn handle-request-error
  [{t :type
    session :session
    request :request
    description :description
    :as data}]
  (let [{:keys [state redirect_uri]} (or
                                       request
                                       (:request (get-session session)))
        base-redirect-uri (get-base-uri redirect_uri)]
    (when session (remove-session session))
    (case t
      ;; When redirection uri error than redirect to 
      ("no_redirections"
        "missing_redirect" "redirect_missmatch" "missing_response_type"
        "client_not_registered" "corrupt_session" "unsupported_grant_type")
      {:status 302
       :headers {"Location" (str "/oauth2/request_error?"
                                 (codec/form-encode (select-keys data [:type])))
                 "Cache-Control" "no-cache"}}
      ;; Otherwise
      {:status 302
       ;; TODO - handle when redirect uri already has parameters
       :headers {"Location" (str base-redirect-uri "?"
                                 (codec/form-encode
                                   (cond->
                                     {:error t}
                                     description (assoc :error_description description)
                                     state (assoc :state state))))
                 "Cache-Control" "no-cache"}})))


(defn set-session-authorized-at
  [session timestamp]
  (swap! *sessions* assoc-in [session :authorized-at] timestamp))


(defn get-session-authorized-at
  [session]
  (get-in @*sessions* [session :authorized-at]))


(defn kill-session
  [session]
  (let [session-data (get-session session)]
    (remove-session-resource-owner session)
    (remove-session-client session)
    (swap! *sessions* dissoc session)
    (publish :session/kill
             {:session session
              :data session-data})))


(defn kill-sessions
  []
  (doseq [[session] @*sessions*]
    (kill-session session)))



(s/def ::authorization-code-grant #{"authorization_code"})
(s/def ::password-grant #{"password"})
(s/def ::implict-grant #{"implicit"})
(s/def ::refresh-token-grant #{"refresh_token"})
(s/def ::client-credentials-grant #{"client_credentials"})


(s/def ::grant_type
  (s/or
    :authorization-code ::authorization-code-grant
    :password ::password-grant
    :refresh-token ::refresh-token-grant
    :implicit ::implict-grant))


(defn json-error
  [status & description]
  (let [_status (if (number? status) status 400)
        [code & description] (if (number? status)
                               description
                               (concat [status] description))]
    {:status _status
     :headers {"Content-Type" "application/json;charset=UTF-8"
               "Pragma" "no-cache"
               "Cache-Control" "no-store"}
     :body (json/write-str
             {:error code
              :error_description (str/join "\n" description)})}))


(defn validate-client [request]
  (let [{:keys [client_id state redirect_uri]
          request-secret :client_secret} request 
        base-redirect-uri (get-base-uri redirect_uri)
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





(defn reset
  []
  (reset! *sessions* nil)
  (reset! *resource-owners* nil)
  (reset! *clients* nil))


;; Interceptors
(defn- decode-base64-credentials
  [data]
  (when data
    (let [decoded-bytes (.decode (Base64/getDecoder) (.getBytes data "UTF-8"))
          decoded (String. decoded-bytes)]
      (str/split decoded #":" 2))))


(def basic-authorization-interceptor
  {:name ::authentication-basic
   :enter
   (fn [{{{authorization "authorization"} :headers} :request :as context}]
     (if-not authorization context
       (let [[_ credentials] (re-find #"Basic\s+(.*)" authorization)
             [id secret] (decode-base64-credentials credentials)]
         (update-in context [:request :params] assoc
                    :client_id id
                    :client_secret secret))))})


(def keywordize-params
  {:name ::keywordize-params
   :enter
   (fn [ctx]
     (update-in ctx [:request :params] keywordize-keys))})


(def scope->set
  {:name ::keywordize-params
   :enter
   (fn [ctx]
     (update-in ctx [:request :params :scope] (fn [scope] (when scope (set (str/split scope #"\s+"))))))})


(def idsrv-session-read
  {:enter (fn [ctx]
            (let [{{{{idsrv-session :value} "idsrv.session"} :cookies} :request} ctx]
              (if (empty? idsrv-session) ctx
                (assoc-in ctx [:request :params :idsrv/session] idsrv-session))))})


(def idsrv-session-remove
  {:leave (fn [ctx]
            (assoc-in ctx [:response :cookies "idsrv.session"]
                      {:value ""
                       :path "/"
                       :http-only true
                       :secure true
                       :max-age 0}))})

(def serve-resource
  {:enter (fn [{{:keys [uri]} :request :as ctx}]
            (assoc ctx :response (response/resource-response uri)))})


;; Maintenance
(defn clean-sessions
  "Function will clean all sessions that were issued
  authorization code, but code wasn't used for more
  than timeout value. Default timeout is 1 minute"
  ([] (clean-sessions (vura/minutes 1)))
  ([timeout]
   (let [now (System/currentTimeMillis)]
     (letfn [(scan? [at]
               (pos? (- now (+ at timeout))))]
       (doseq [[session {:keys [code at tokens]}] @*sessions*
               :when (scan? at)]
         (cond
           ;; Authorization code wasn't used
           (and (some? code) (empty? tokens))
           (do
             (log/debugf "[%s] Session timed out. No code or access token was assigned to this session" session)
             (kill-session session))
           :else
           nil))))))


(defonce maintenance-agent (agent {:running true :period (vura/seconds 30)}))


(defn maintenance
  [{:keys [running period] :as data}]
  (when running
    (log/debug "OAuth2 maintenance start")
    (send-off *agent* maintenance)
    (clean-sessions (vura/minutes 1))
    (log/debug "OAuth2 maintenance finish")
    (Thread/sleep period)
    data))


(defn start-maintenance
  []
  (send-off maintenance-agent maintenance))



(comment
  (clean-codes)
  (java.util.Date. 1720693899)
  (def token (-> *tokens* deref :access_token ffirst))
  (reset)
  (require '[buddy.sign.jwt :as jwt])
  (jwt/decode-header token)
  (reset))
