(ns neyho.eywa.iam.oauth.mem
  (:require
    [nano-id.core :refer [nano-id]]
    [clojure.string :as str]
    [ring.util.codec :as codec]
    [neyho.eywa.iam :as iam]
    [io.pedestal.interceptor.chain :as chain]))


(defonce ^:dynamic *refresh-tokens* (atom nil))
(defonce ^:dynamic *access-tokens* (atom nil))
(defonce ^:dynamic *access-codes* (atom nil))


(defonce ^:dynamic *resource-owners* (atom nil))
(defonce ^:dynamic *clients* (atom nil))
(defonce ^:dynamic *sessions* (atom nil))


(defn validate-client [session]
  (let [{{:keys [client_id state redirect_uri]
          request-password :client-password} :request} (get @*sessions* session)
        {:keys [euuid password]
         {type "type"
          redirections "redirections"} :settings
         :as client} (iam/get-client client_id)]
    (cond
      (nil? euuid)
      (throw
        (ex-info
          "Client not regiestered"
          {:type "client_not_registered"
           :session session}))
      ;;
      (empty? redirections)
      (throw
        (ex-info
          "Client missing redirections"
          {:type "no_redirections"
           :session session}))
      ;;
      (empty? redirect_uri)
      (throw
        (ex-info
          "Client hasn't provided redirect URI"
          {:type "missing_redirect"
           :session session}))
      ;;
      (not-any? #(= redirect_uri %) redirections)
      (throw
        (ex-info
          "Client provided uri doesn't match available redirect URI(s)"
          {:type "redirect_mismatch"
           :session session}))
      ;;
      (or (some? request-password) (some? password))
      (if (iam/validate-password request-password password)
        client
        (throw
          (ex-info
            "Client password mismatch"
            {:type "access_denied"
             :session session
             :state state})))
      ;;
      (and (= type "public") (nil? password))
      client
      ;;
      :else
      (throw
        (ex-info "Unknown client error"
                 {:session session
                  :type "server_error"})))))


(defn validate-resource-owner [username password]
  (let [{db-password :password
         active :active
         :as resource-owner} (iam/get-user-details username)]
    (if-not active nil
      (when (iam/validate-password password db-password)
        resource-owner))))


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


(defn get-session-resource-owner [session]
  (let [euuid (get-in @*sessions* [session :resource-owner])]
    (get @*resource-owners* euuid)))


(defn remove-session-resource-owner [session]
  (let [euuid (get-in @*sessions* [session :resource-owner])]
    (swap! @*sessions* update session dissoc :resource-owner)
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



(defn get-session-client [session]
  (let [euuid (get-in @*sessions* [session :client])]
    (get @*clients* euuid)))


(defn remove-session-client [session]
  (let [euuid (get-in @*sessions* [session :client])]
    (swap! @*sessions* update session dissoc :client)
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


(defn- handle-request-error
  [{t :type
    session :session}]
  (let [{{:keys [state redirect_uri] :as r} :request} (get-session session)]
    (remove-session session)
    (case t
      ;; When redirection uri error than redirect to 
      ("no_redirections" "missing_redirect" "redirect_mismatch")
      {:status 302
       :headers {"Location" (str "/oauth/request_error.html?"
                                 (codec/form-encode
                                   {:type t}))
                 "Cache-Control" "no-cache"}}
      ;;
      ("client_not_registered")
      {:status 302
       :headers {"Location" (str "/oauth/request_error.html?"
                                 (codec/form-encode
                                   {:type t}))
                 "Cache-Control" "no-cache"}}
      ;;
      ("missing_response_type")
      {:status 302
       :headers {"Location" (str "/oauth/request_error.html?"
                                 (codec/form-encode
                                   {:type t}))
                 "Cache-Control" "no-cache"}}
      ;; Otherwise
      {:status 302
       :headers {"Location" (str redirect_uri "?"
                                 (codec/form-encode
                                   (cond->
                                     {:error t}
                                     state (assoc :state state))))
                 "Cache-Control" "no-cache"}})))


(defn return-token [session])


(defn authorization-request
  [{:keys [response_type username password redirect_uri]
    :as request}]
  (println "REQ: " request)
  (let [now (System/currentTimeMillis)
        session (nano-id 20)]
    (set-session session
                 {:request request
                  :at now})
    (case response_type
      ("code" "token")
      (try
        (let [client (validate-client session)]
          ;; Proper implementation
          (set-session-client session client)
          (let [{{url "login-page"} :settings} (get-session-client session)]
            {:status 302
             :headers {"Location" (str url "?" (codec/form-encode {:session session}))
                       "Cache-Control" "no-cache"}}))
        (catch clojure.lang.ExceptionInfo ex
          (handle-request-error (ex-data ex))))
      "password" (try
                   (let [user (validate-resource-owner username password)]
                     (set-session-resource-owner session user)
                     (remove-session session)
                     (return-token session))
                   (catch clojure.lang.ExceptionInfo ex
                     (handle-request-error (ex-data ex))))
      "client_credentials"
      (try
        (validate-client session)
        (return-token session)
        (catch clojure.lang.ExceptionInfo ex (handle-request-error (ex-data ex))))
      ;;
      (cond
        ;;
        (empty? redirect_uri)
        (handle-request-error
          {:type "missing_redirect"
           :session session})
        ;;
        (empty? response_type)
        (handle-request-error
          {:type "missing_response_type"
           :session session})
        ;;
        :else
        (handle-request-error
          {:type "server_error"
           :session session})))))


(defn reset
  []
  (reset! *sessions* nil)
  (reset! *resource-owners* nil)
  (reset! *clients* nil)
  (reset! *access-codes* nil)
  (reset! *refresh-tokens* nil)
  (reset! *access-tokens* nil))



(def authorize-request-interceptor
  {:name :eywa/default-redirect 
   :enter
   (fn [{{:keys [query-params]} :request :as context}]
     ; (def context context)
     ; (:query-params (:request context))
     (chain/terminate
       (assoc context :response
              (authorization-request query-params))))})


(def routes
  #{["/oauth2/authorize" :get [authorize-request-interceptor] :route-name ::authorize-request]})


(comment
  (reset)
  "http://localhost:8080/oauth2/authorize?client_id=oauth_test_confidential&redirect_uri=http://localhost:8080/eywa/&return_type=code")
