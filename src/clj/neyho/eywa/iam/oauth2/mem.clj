(ns neyho.eywa.iam.oauth.mem
  (:require
    [ring.util.codec :as codec]
    [neyho.eywa.iam :as iam]
    [neyho.eywa.iam.oauth2
     :refer [authorization-request]]
    [neyho.eywa.iam.oauth2.protocol :as oauth2]))


(defonce ^:dynamic *refresh-tokens* (atom nil))
(defonce ^:dynamic *access-tokens* (atom nil))
(defonce ^:dynamic *access-codes* (atom nil))


(defonce ^:dynamic *resource-owners* (atom nil))
(defonce ^:dynamic *clients* (atom nil))
(defonce ^:dynamic *sessions* (atom nil))


(defn- handle-request-error
  [_ _ {t :type}]
  (case t
    ("no_redirections" "missing_redirect" "redirect_mismatch")
    {:status 302
     :headers {"Location" (str "http://localhost:8080/login/error.html?" )
               "Cache-Control" "no-cache"
               "Set-Cookie" "EYWA=;path=/;max-age=-1;httpOnly;Secure;"}}
    ))


(defrecord MemStore []
  neyho.eywa.iam.oauth2.protocol/OAuthProtocol
  (validate-client [this session]
    (let [{{:keys [client-id state redirect-uri]
            request-password :client-password} :request} (oauth2/get-session this session)
          {:keys [euuid password]
           {type "type"
            redirections "redirections"} :settings
           :as client} (iam/get-client client-id)]
      (cond
        (nil? euuid)
        (throw
          (ex-info
            "Client not regiestered"
            {:type "identifier_invalid"
             :state state}))
        ;;
        (empty? redirections)
        (throw
          (ex-info
            "Client missing redirections"
            {:type "no_redirections"
             :state state}))
        ;;
        (empty? redirect-uri)
        (throw
          (ex-info
            "Client hasn't provided redirect URI"
            {:type "missing_redirect"
             :state state}))
        ;;
        (not-any? #(= redirect-uri %) redirections)
        (throw
          (ex-info
            "Client hasn't provided redirect URI"
            {:type "redirect_mismatch"
             :state state}))
        ;;
        (or (some? request-password) (some? password))
        (if (iam/validate-password request-password password)
          client
          (throw
            (ex-info
              "Client password mismatch"
              {:type "access_denied"
               :state state}))) 
        ;;
        (and (= type "public") (nil? password))
        client
        ;;
        :else nil)))
  ;;
  (validate-resource-owner [_ username password]
    (let [{db-password :password
           active :active
           :as resource-owner} (iam/get-user-details username)]
      (if-not active nil
        (when (iam/validate-password password db-password)
          resource-owner))))
  ;; OAuth flow protocol
  (redirect-login [this session]
    (let [{{:keys []} :request} (oauth2/get-session this session)
          {{url "login-page"} :settings} (oauth2/get-session-client this session)]
      {:status 302
       :headers {"Location" (str url "?" (codec/form-encode {:session session}))
                 "Cache-Control" "no-cache"
                 "Set-Cookie" "EYWA=;path=/;max-age=-1;httpOnly;Secure;"}}))
  ;;
  (request-error [this session data]
    (handle-request-error this session data))
  ;;
  (return-token [this session]
    (let [{} (oauth2/get-session this session)]
      (println "Vratio sam token")))
  ;;
  (return-code [this session]
    (let [{} (oauth2/get-session this session)]
      (println "Vratio sam ti kod")))



  ;; This is for session management
  (get-redirection-uris [this session]
    (let [{{:strs [redirections]} :settings} (oauth2/get-session-client this session)]
      redirections))
  (set-session-resource-owner [_ session {:keys [euuid] :as resource-owner}]
    (swap! *sessions* assoc-in [session :resource-owner] euuid)
    (swap! *resource-owners* update euuid
           (fn [current]
             (->
               current 
               (merge resource-owner)
               (update :sessions (fnil conj #{}) session))))
    nil)
  ;;
  (get-session-resource-owner [_ session]
    (let [euuid (get-in @*sessions* [session :resource-owner])]
      (get @*resource-owners* euuid)))
  ;;
  (remove-session-resource-owner [_ session]
    (let [euuid (get-in @*sessions* [session :resource-owner])]
      (swap! @*sessions* update session dissoc :resource-owner)
      (when euuid
        (swap! *resource-owners* update euuid
               (fn [current]
                 (update current :sessions (fnil disj #{}) session))))
      nil))
  ;; Client sessions
  (set-session-client [_ session {:keys [euuid] :as client}]
    (swap! *sessions* assoc-in [session :client] euuid)
    (swap! *clients* update euuid
           (fn [current]
             (->
               current 
               (merge client)
               (update :sessions (fnil conj #{}) session))))
    nil)
  ;;
  (get-session-client [_ session]
    (let [euuid (get-in @*sessions* [session :client])]
      (get @*clients* euuid)))
  ;;
  (remove-session-client [_ session]
    (let [euuid (get-in @*sessions* [session :client])]
      (swap! @*sessions* update session dissoc :client)
      (when euuid
        (swap! *clients* update euuid
               (fn [current]
                 (update current :sessions (fnil disj #{}) session))))
      nil))
  ;;
  (get-session [_ id] (get @*sessions* id))
  ;;
  (set-session [_ id data] (swap! *sessions* assoc id data))
  ;;
  (remove-session [this id]
    (let [{{client-euuid :euuid :as client} :client
           {resource-owner-euuid :euuid :as resource-owner} :resource-owner} (oauth2/get-session this id)]
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
      nil)))


(defn reset
  []
  (reset! *sessions* nil)
  (reset! *resource-owners* nil)
  (reset! *clients* nil)
  (reset! *access-codes* nil)
  (reset! *refresh-tokens* nil)
  (reset! *access-tokens* nil))


(comment
  (reset)
  (def impl (MemStore.))
  (get-session impl "TJMakGMFyLvejpBy8-yq")
  (iam/get-client "oauth_test_public")
  (iam/get-client "oauth_test_confidential")
  (oauth2/validate-client impl "ZyQZ-HBNftNJic_guo1W")
  (oauth2/validate-client impl "Md34K1pNoSjQhVSMikfu")
  (oauth2/get-redirection-uris impl "XCWDjoKLUXSqipHKhStb")
  (oauth2/get-session-client impl "XCWDjoKLUXSqipHKhStb")
  (time
    (authorization-request
      impl
      {:client-id "oauth_test_confidential"
       :response-type "code"
       :username "test_oauth"
       :password "password"}))
  (time
    (authorization-request
      impl
      {:client-id "oauth_test_confidential"
       :response-type "code"
       :username "test_oauth"}))
  (time
    (authorization-request
      impl
      {:client-id "oauth_test_confidential"
       :client-password "testisi123$"
       :response-type "code"
       :username "test_oauth"
       :password "password"})))
