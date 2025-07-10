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
   [neyho.eywa.iam :as iam]
   [neyho.eywa.dataset.core :as core]
   [neyho.eywa.iam.access :as access]
   [neyho.eywa.env :as env]
   [neyho.eywa.iam.uuids :as iu]
   [io.pedestal.http.body-params :as bp]
   [io.pedestal.http.ring-middlewares :as middleware])
  (:import
   [java.util Base64]))

(defn pprint [data] (with-out-str (clojure.pprint/pprint data)))

(defonce ^:dynamic *resource-owners* (atom nil))
(defonce ^:dynamic *clients* (atom nil))
(defonce ^:dynamic *sessions* (atom nil))
(defonce ^:dynamic *domain* nil)

(defn domain+
  ([] (domain+ ""))
  ([path]
   (str (or env/iam-root-url *domain*) path)))

(defonce ^:dynamic *encryption-key* (nano-id/nano-id 32))
(defonce ^:dynamic *initialization-vector* (nano-id/nano-id 12))

(defn encrypt
  [data]
  (let [json-data (.getBytes (json/write-str data))]
    (String.
     (codecs/bytes->b64
      (crypto/encrypt
       json-data *encryption-key* *initialization-vector*
       {:alg :aes256-gcm})))))

(defn decrypt [encrypted-data]
  (try
    (json/read-str
     (String.
      (crypto/decrypt
       (codecs/b64->bytes encrypted-data) *encryption-key* *initialization-vector*
       {:alg :aes256-gcm}))
     :key-fn keyword)
    (catch Throwable _ nil)))

; (comment
;   (time
;    (decrypt
;     (encrypt
;      {:device-code 100
;       :user-code 200
;       :ip "a"
;       :user-agent "jfioq"}))))
;
(let [alphabet "ACDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"]
  (def gen-session-id (nano-id/custom alphabet 30)))

(let [default (vura/hours 2)]
  (defn access-token-expiry
    [{{{expiry "access"} "token-expiry"} :settings}]
    (or expiry default)))

(let [default (vura/days 1.5)]
  (defn refresh-token-expiry
    [{{{expiry "refresh"} "token-expiry"} :settings}]
    (long (or expiry default))))

(defn expired?
  [token]
  (try
    (let [{:keys [exp]} (iam/unsign-data token)]
      (< (* 1000 exp) (System/currentTimeMillis)))
    (catch clojure.lang.ExceptionInfo ex
      (let [{:keys [cause]} (ex-data ex)]
        (if (= cause :exp)
          true
          (throw ex))))))

(defn expires-at
  [token]
  (try
    (let [{:keys [exp]} (iam/unsign-data token)]
      (java.util.Date. (* 1000 exp)))
    (catch clojure.lang.ExceptionInfo ex
      (let [{:keys [cause]} (ex-data ex)]
        (if (= cause :exp)
          (vura/value->date 0)
          (throw ex))))))

(defmulti sign-token (fn [_ token-key _] token-key))

(defmethod sign-token :default
  [session token-key data]
  (log/errorf "[%s] Couldn't signt token `%s`" session token-key)
  data)

(defn get-session-client [session]
  (let [euuid (get-in @*sessions* [session :client])]
    (get @*clients* euuid)))

(defprotocol LazyOAuth
  (get-resource-owner [this]))

(extend-protocol LazyOAuth
  java.util.UUID
  (get-resource-owner [this] (get @*resource-owners* this))
  java.lang.String
  (get-resource-owner [this]
    (if-some [euuid (get-in @*resource-owners* [::name-mapping this])]
      (get-resource-owner euuid)
      (let [{:keys [euuid name] :as resource-owner} (iam/get-user-details this)]
        (swap! *resource-owners*
               (fn [resource-owners]
                 (->
                  resource-owners
                  (assoc euuid resource-owner)
                  (update ::name-mapping assoc name euuid))))
        resource-owner))))

(defn get-session-resource-owner [session]
  (let [euuid (get-in @*sessions* [session :resource-owner])]
    (get @*resource-owners* euuid)))

(defmulti process-scope (fn [_ _ scope] scope))

(defmethod process-scope :default
  [session tokens scope]
  (let [{:keys [roles]} (get-session-resource-owner session)
        user-scopes (access/roles-scopes roles)]
    (if (contains? user-scopes scope)
      (update-in tokens [:access_token :scope] (fnil conj []) scope)
      tokens)))

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
         :as resource-owner} (iam/get-user-details username)]
    (if-not active
      nil
      (when (iam/validate-password password db-password)
        (dissoc resource-owner :password)))))

(defn set-session-resource-owner
  [session {:keys [euuid] username :name :as resource-owner}]
  (swap! *sessions* assoc-in [session :resource-owner] euuid)
  (swap! *resource-owners*
         (fn [resource-owners]
           (->
            resource-owners
            (update euuid (fn [current]
                            (->
                             current
                             (merge resource-owner)
                             (update :sessions (fnil conj #{}) session))))
            (update ::name-mapping assoc username euuid))))
  nil)

(defn remove-session-resource-owner [session]
  (let [euuid (get-in @*sessions* [session :resource-owner])]
    (swap! *sessions* update session dissoc :resource-owner)
    (when euuid
      (swap! *resource-owners*
             (fn [resource-owners]
               (let [{{:keys [sessions]} euuid :as resource-owners}
                     (update-in resource-owners [euuid :sessions] (fnil disj #{}) session)]
                 (if (empty? sessions)
                   (dissoc resource-owners euuid)
                   resource-owners)))))
    nil))

(defn set-session-audience-scope
  ([session scope] (set-session-audience-scope session nil scope))
  ([session audience scope]
   (swap! *sessions* assoc-in [session :scopes audience] scope)))

(defn get-session-audience-scope
  ([session] (get-session-audience-scope session nil))
  ([session audience]
   (get-in @*sessions* [session :scopes audience])))

(defn clients-match? [session {:keys [client_id client_secret]}]
  (let [{known-id :id
         known-secret :secret} (get-session-client session)]
    (cond
      (not= client_id known-id) false
      (and client_secret (not= client_secret known-secret)) false
      (and known-secret (not= client_secret known-secret)) false
      :else true)))

(def clients-doesnt-match? (complement clients-match?))

(defn get-client
  [id]
  (when-let [client (iam/get-client id)]
    (swap! *clients* update (:euuid client) merge client)
    client))

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

(defn get-session-tokens [session]
  (reduce
   (fn [r tokens]
     (reduce
      (fn [r [k token]] (update r k (fnil conj []) token))
      r
      (partition 2 (interleave (keys tokens) (vals tokens)))))
   nil
   (vals (:tokens (get-session session)))))

(comment
  (def session "YiSclFaUmCFKhSRrHNshCYDKlMjQKz"))

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

;; TODO - this is bad... Go through OAuth spec check what
;; should be returned to client and what should be redirected
;; to AS error page... This is the only reason this function exists
;; It is used in authorization_code namespace
(defn handle-request-error
  [{t :type
    session :session
    request :request
    description :description}]
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
       :headers {"Location" (str "/oauth/status?"
                                 (codec/form-encode
                                  {:value "error"
                                   :error t}))
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

(defmulti session-kill-hook (fn [priority _] priority))

(defn kill-session
  [session]
  (let [session-data (get-session session)]
    (doseq [p (sort (keys (methods session-kill-hook)))]
      (session-kill-hook p session))
    (remove-session-resource-owner session)
    (remove-session-client session)
    (swap! *sessions* dissoc session)
    (iam/publish
     :oauth.session/killed
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

(defn reset
  []
  #_(reset! *sessions* nil)
  #_(reset! *resource-owners* nil)
  #_(reset! *clients* nil))

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

(defn original-uri
  [{original-uri :uri
    {forwarded-host "x-forwarded-host"
     forwarded-proto "x-forwarded-proto"
     host "host"} :headers
    :keys [scheme]}]
  ; (def request request)
  (format
   "%s://%s"
   (or forwarded-proto (name scheme))
   (or forwarded-host host original-uri)))

(def original-uri-interceptor
  {:enter
   (fn [{request :request :as ctx}]
     (assoc ctx ::uri (original-uri request)))})

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

(def oauth-common-interceptor
  [basic-authorization-interceptor
   middleware/cookies
   (bp/body-params)
   keywordize-params])

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
   (let [now (vura/time->value (vura/date))]
     (doseq [[session {:keys [authorized-at tokens]}] @*sessions*
             :let [authorized-at (when authorized-at (vura/time->value authorized-at))]
             :when (or
                    (nil? authorized-at)
                    (and
                     (< (+ authorized-at timeout) now)
                     (every? empty? (vals tokens))))]
       (log/debugf "[%s] Session timed out. No code or access token was assigned to this session" session)
       (kill-session session)))))

(defn reload-clients
  []
  (try
    (let [ids (remove nil? (map :id (vals @*clients*)))
          new-clients (iam/get-clients ids)]
      (swap! *clients*
             (fn [old-clients]
               (merge-with
                merge old-clients
                (reduce
                 (fn [r {:keys [euuid] :as client}]
                   (assoc r euuid client))
                 nil
                 new-clients)))))
    (catch Throwable ex
      (log/errorf ex "[OAuth] Couldn't reload clients"))))

(defn monitor-client-change
  []
  (let [delta-chan (async/chan (async/sliding-buffer 1))
        close-chan (async/promise-chan)]
    (async/sub core/*delta-publisher* iu/app delta-chan)
    ;; Start idle service that will listen on delta changes
    (async/go-loop
     [_ (async/<! delta-chan)]
      (log/debugf "[IAM] Received client delta")
      ;; When first delta change is received start inner loop
      (loop [[idle-value] (async/alts!
                           [;; That will check for new delta values
                            delta-chan
                             ;;
                            close-chan
                             ;; Or timeout
                            (async/go
                              (async/<! (async/timeout 5000))
                              ::TIMEOUT)])]
        (log/debugf "[IAM] Next idle value is: %s" idle-value)
        ;; IF timeout is received than reload rules
        (if (= ::TIMEOUT idle-value)
          (do
            (log/info "[IAM] Reloading clients!")
            (reload-clients))
          ;; Otherwise some other delta has been received and
          ;; inner loop will be repeated
          (recur (async/alts!
                  [;; That will check for new delta values
                   delta-chan
                    ;; Or timeout
                   (async/go
                     (async/<! (async/timeout 5000))
                     ::TIMEOUT)]))))
      ;; when reloading is complete, wait for new delta value
      ;; and repeat process
      (recur (async/<! delta-chan)))))
