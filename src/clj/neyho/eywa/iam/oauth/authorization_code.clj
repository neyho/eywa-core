(ns neyho.eywa.iam.oauth.authorization-code
  (:require
    clojure.java.io
    clojure.pprint
    [clojure.data.json :as json]
    [clojure.tools.logging :as log]
    [nano-id.core :as nano-id]
    [vura.core :as vura]
    [neyho.eywa.iam
     :refer [validate-password]]
    [neyho.eywa.iam.oauth.core :as core
     :refer [get-client
             pprint
             publish]]
    [neyho.eywa.iam.oauth.token :as token
     :refer [grant-token
             token-error
             client-id-missmatch
             owner-not-authorized]]))


(defonce ^:dynamic *authorization-codes* (atom nil))


(def grant "authorization_code")


(defn delete [code] 
  (swap! *authorization-codes* dissoc code))


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


(defn code-was-issued? [code] (true? (get-in @*authorization-codes* [code :issued?])))


(defn validate-client [request]
  ; (def request request)
  ; (comment
  ;   (def client_id "MUMADPADAKQHSDFDGFAEJZJXUSFJGFOOYTWVAUDEFVPURUOP"))
  (let [{:keys [client_id state redirect_uri]
          request-secret :client_secret} request 
        base-redirect-uri (core/get-base-uri redirect_uri)
        {:keys [euuid secret type]
         {redirections "redirections"
          allowed-grants "allowed-grants"} :settings
         :as client} (get-client client_id)
        grants (set allowed-grants)]
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
        (do
          (swap! core/*clients* assoc (:euuid client) client)
          client)
        (throw
          (ex-info
            "Client secret missmatch"
            {:type "access_denied"
             :request request
             :state state})))
      ;;
      (not (contains? grants grant))
      (throw
        (ex-info
          "Client doesn't support authorization_code flow"
          {:type "access_denied"
           :request request}))
      ;;
      (and (= type "public") (nil? secret))
      (do
        (swap! core/*clients* assoc (:euuid client) client)
        client)
      ;;
      :else
      (do
        (log/errorf "Couldn't validate client\n%s" (pprint request))
        (throw
          (ex-info "Unknown client error"
                   {:request request
                    :type "server_error"}))))))


(defmethod grant-token "authorization_code"
  [request]
  (let [{:keys [code redirect_uri client_id client_secret grant_type]} request
        {{request-redirect-uri :redirect_uri
          :as original-request} :request
         client-euuid :client
         :keys [session expires-at]} (get @*authorization-codes* code) 
        {id :id :as client} (get @core/*clients* client-euuid)
        {:keys [active]} (core/get-session-resource-owner session)]
    (log/debugf
      "[%s] Processing token code grant for code: %s\n%s"
      session code (pprint request))
    (if-not session
      ;; If session isn't available, that is if somebody
      ;; is trying to hack in
      (token-error
        "invalid_request"
        "Trying to abuse token endpoint for code that"
        "doesn't exsist or has expired. Further actions"
        "will be logged and processed")
      ;; If there is some session than check other requirements
      (let [{_secret :secret
             {:strs [allowed-grants]} :settings
             session-client :id} (core/get-session-client session)
            grants (set allowed-grants)]
        (cond
          ;;
          (not (contains? grants "authorization_code"))
          (token-error
            "unauthorized_grant"
            "Client sent access token request"
            "for grant type that is outside"
            "of client configured privileges")
          ;;
          (not (contains? @*authorization-codes* code))
          (token-error
            "invalid_request"
            "Provided authorization code is illegal!"
            "Your request will be logged"
            "and processed")
          ;;
          (< expires-at (System/currentTimeMillis))
          (token-error
            "invalid_request"
            "This device code has expired. Restart"
            "authentication process.")
          ;; If redirect uri doesn't match
          (not= request-redirect-uri redirect_uri)
          (token-error
            "invalid_request"
            "Redirect URI that you provided doesn't"
            "match URI that was issued to provided authorization code")
          ;; If client ids don't match
          (not= session-client client_id)
          (token-error
            "invalid_client"
            "Client ID that was provided doesn't"
            "match client ID that was used in authorization request")
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
          (not= id client_id)
          client-id-missmatch
          ;;
          (not active)
          (do
            (delete code)
            (core/kill-session session)
            owner-not-authorized)
          ;; Issue that token
          :else
          (let [response (json/write-str (token/generate client session original-request))]
            (revoke-authorization-code code)
            {:status 200
             :headers {"Content-Type" "application/json;charset=UTF-8"
                       "Pragma" "no-cache"
                       "Cache-Control" "no-store"}
             :body response}))))))



(defn mark-code-issued [session code]
  (swap! *authorization-codes* update code
         (fn [data]
           (assoc data
                  :issued? true
                  :session session
                  :expires-at (-> 
                                (System/currentTimeMillis)
                                (+ (vura/minutes 5)))))))


(defn clean-codes
  ([] (clean-codes (vura/minutes 8)))
  ([timeout]
   (let [now (System/currentTimeMillis)]
     (swap! *authorization-codes*
            (fn [codes]
              (reduce-kv
                (fn [result code {:keys [created-on] :as data}]
                  (if (or (nil? created-on) (> (- now created-on) timeout)) result
                    (assoc result code data)))
                nil
                codes))))))

(comment
  (clean-codes))
