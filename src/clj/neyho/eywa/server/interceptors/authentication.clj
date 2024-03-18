(ns neyho.eywa.server.interceptors.authentication
  (:require
    clojure.string
    clojure.java.io
    clojure.pprint
    [vura.core :as ntime]
    [buddy.sign.jwt :as jwt]
    [buddy.hashers :as hashers]
    [buddy.core.keys :as keys]
    [buddy.sign.util :refer [to-timestamp]]
    [neyho.eywa.data :refer [*ROOT*]]
    [neyho.eywa.administration.uuids :as au]
    [neyho.eywa.dataset
     :refer [get-entity]]
    [neyho.eywa.authorization
     :refer [get-role-schema]]
    [clojure.tools.logging :as log]
    [io.pedestal.interceptor.chain :as chain]
    [io.pedestal.http.body-params :as body-params])
  (:import
    [java.security KeyPairGenerator]))



(defonce ^:dynamic *private-key* nil)
(defonce ^:dynamic *public-key* nil)


(defn init-encryption
  [{:keys [public private]}]
  (if-not (keys/public-key? public) 
    (throw (ex-info "Unacceptable public key" {:key public}))
    (alter-var-root #'*private-key* (constantly private)))
  (if-not (keys/private-key? private)
    (throw (ex-info "Unacceptable private key" {:key private}))
    (alter-var-root #'*public-key* (constantly public))))


(defn init-default-encryption
  []
  (let [generator (KeyPairGenerator/getInstance "RSA")
        key-pair (.generateKeyPair generator)
        public (.getPublic key-pair)
        private (.getPrivate key-pair)]
    (init-encryption
      {:private private
       :public public})))


(defn sign-data
  "Function encrypts data that should be in map form and returns encrypted
  string."
  ([data] (sign-data
            data
            (->
              (ntime/date)
              ntime/date->value
              (+ ntime/day)
              ntime/value->date
              to-timestamp)))
  ([data valid]
   (jwt/sign
     data
     *private-key*
     {:alg :rs256
      :exp valid})))


(defn get-password [username]
  (:password
    (get-entity
      au/user
      {:name username}
      {:password nil})))


(comment
  (def username "rgersak"))

(defn get-user-details [username]
  (->
    (get-entity
      au/user
      {:name username}
      {:_eid nil
       :euuid nil
       :name nil
       :password nil
       :active nil
       :avatar nil
       :settings nil
       :groups [{:selections {:euuid nil}}]
       :roles [{:selections {:euuid nil}}]})
    (update :roles #(set (map :euuid %)))
    (update :groups #(set (map :euuid %)))))


(defn get-token-user-details [token]
  (let [[username token] (clojure.string/split token #"\:") ]
    (if-some [{[user] :services}
              (get-entity
                #uuid "1029c9bd-dc48-436a-b7a8-2245508a4a72"
                {:id token}
                {:services
                 [{:selections {:_eid nil
                                :euuid nil
                                :name nil
                                :password nil
                                :active nil
                                :avatar nil
                                :settings nil
                                :groups [{:selections {:euuid nil}}]
                                :roles [{:selections {:euuid nil}}]}
                   :args {:_where {:name {:_eq username}}}}]})]
      (let [{:keys [_eid euuid groups roles] username :name}
            (->
              user
              (update :roles #(set (map :euuid %)))
              (update :groups #(set (map :euuid %))))]
        {:eywa/id _eid
         :eywa/user euuid
         :eywa/username username
         :eywa/groups groups
         :eywa/roles roles})
      (throw
        (ex-info 
          "Couldn't find valid token"
          {:token token})))))


(defn jwt-token? [token]
  (= 2 (count (re-seq #"\." token))))


(defn unsign-data
  "Function takes encrypted string and returns decrypted data."
  [data]
  (jwt/unsign data *public-key* {:alg :rs256}))


(defn check-credentials
  "Checks credentials for username, password"
  [{:keys [username password]}]
  (when (every? not-empty [username password])
    (let [{:keys [_eid euuid active groups roles settings]
           password' :password} (get-user-details username)]
      (log/tracef "Checking credentials password for username: %s" username)
      (if (and active (hashers/check password password'))
        (do
          (log/tracef "User %s authenticated" username)
          (let [data {:token-data
                      {:eywa/id _eid
                       :eywa/user euuid
                       :eywa/username username
                       :eywa/groups groups
                       :eywa/roles roles}
                      :settings settings}] 
            (log/tracef "Password matches. Returning token %s" data)
            data))
        (do
          (log/errorf "Invalid password for user: %s\n%s   %s" username password password')
          nil)))))

; (defn active?
;   "Returns true if user id in account id is marked as active."
;   [{:keys [eywa/username]}]
;   (:active (get-user-details username)))


(comment
  (def username "rgersak"))


(defn get-cookies [{{:keys [headers]} :request}]
  (let [{cookies "cookie"} headers]
    (when cookies
      (reduce
       (fn [r c]
         (let [[k v] (clojure.string/split c #"=")]
           (assoc r k v)))
       nil
       (clojure.string/split cookies #"[;\s]+")))))


(defn get-token [{{:keys [headers]} :request :as context}]
  (let [{auth "authorization"} headers]
    ; (log/errorf "Authenticating: %s" headers)
    (if (not-empty auth)
      (clojure.string/replace auth #"^Bearer\s+" "")
      (let [{eywa "EYWA"} (get-cookies context)]
        (if eywa eywa
          (log/errorf "Couldn't find authorization token in %s %s" auth (get-cookies context)))))))



(defn get-user-context [context]
  (when-some [token (get-token context)]
    (try
      (if (jwt-token? token)
        (when-let [{:keys [eywa/username] :as data} (unsign-data token)]
          (when username data))
        (get-token-user-details token))
      (catch Throwable _ nil))))


(def eywa-context
  {:name :eywa/context
   :enter
   (fn [context]
     (let [user (get-user-context context)]
       (if (:eywa/username user)
         (let [user-roles (set (map #(java.util.UUID/fromString %) (:eywa/roles user)))
               user-groups (set (map #(java.util.UUID/fromString %) (:eywa/groups user)))
               user-context (->
                              user
                              (update
                                :user/roles
                                (fn [roles]
                                  (set
                                    (map
                                      #(java.util.UUID/fromString %)
                                      roles))))
                              (assoc
                                :user (:eywa/id user)
                                :username (:eywa/username user)
                                :root? (contains? user-roles (:euuid *ROOT*))
                                :groups user-groups
                                :roles user-roles))]
           (assoc-in
             context
             [:request :lacinia-app-context]
             user-context))
         (chain/terminate
           (assoc context
                  :response {:status 403
                             :headers {"WWW-Authenticate" "Bearer"}
                             :body "Not authorized"})))))})


(def authenticate
  {:name :authenticate
   :enter
   (fn [context]
     (if-let [uc (get-user-context context)]
       (assoc context :eywa.user/context uc)
       (chain/terminate
         (assoc context
                :response {:status 403
                           :headers {"WWW-Authenticate" "Bearer"}
                           :body "Not authorized"}))))})


(def default-redirect-response
  {:status 301
   :headers {"Location" "/index.html"
             "Cache-Control" "no-cache"
             "Set-Cookie" "EYWA=;path=/;max-age=-1;"}
   :body "Not authorized!"})


(def authenticate-or-redirect
  {:name :authenticate-or-redirect
   :enter
   (fn [context]
     (if-let [uc (get-user-context context)]
       (assoc context :eywa.user/context uc)
       context))})


(comment
  (log/trace :msg "hi")
  (get-role-schema #{#uuid "601ee98d-796b-43f3-ac1f-881851407f34"}))

(def access-tree
  {:name :eywa/props
   :enter
   (fn [{user :eywa.user/context
         :as context}]
     (if (:eywa/username user)
       (let [roles (set
                     (map
                       #(java.util.UUID/fromString %)
                       (:eywa/roles user)))]
         (assoc context
                :response
                {:status 200
                 :body (get-role-schema roles)}))
       (chain/terminate
         (assoc context
                :response
                {:status 403
                 :headers {"WWW-Authenticate" "Bearer"}
                 :body "Not authorized"}))))})

(def user-data
  {:name :eywa/props
   :enter
   (fn [{user :eywa.user/context :as context}]
     (log/tracef "Returning user data %s" user)
     (assoc context
       :response {:status 200
                  :body (dissoc
                          (get-user-details (:eywa/username user))
                          :password
                          :_eid)}))})


(def user-token
  {:name :eywa/props
   :enter
   (fn [context]
     (assoc context
       :response {:status 200
                  :body (get-token context)}))})


(def app-login
  {:name :login
   :enter
   (fn [context]
     (let [{{{:keys [username password]} :json-params} :request} context]
       (log/tracef "Application login attempt for user %s" username)
       (assoc context :response
              (cond
                (not (string? username)) {:status 400 :body "username should be string"}
                (not (string? password)) {:status 400 :body "password should be string"}
                ; (not (string? account)) {:status 400 :body "account should be string"}
                (empty? username) {:status 400 :body "username not specified"}
                (empty? password) {:status 400 :body "password not specified"}
                ; (empty? account) {:status 400 :body "account not specified"}
                :else
                (do
                  (log/trace "User parameters valid. Ready for credentials checking")
                  (if-let [{user-context :token-data}
                           (check-credentials
                             {:username username
                              :password password})]
                    (do
                      (log/tracef "Credentials checked. Signing data and returning response to user %s" username)
                      {:status 200
                       :headers {"Cache-Control" "no-cache"
                                 "Content-Type" "text/html"
                                 "Set-Cookie"
                                 (clojure.string/join
                                   ";"
                                   (reduce
                                     (fn [r [k v]]
                                       (if (some? v)
                                         (conj r (str k "=" v))
                                         (conj r k)))
                                     []
                                     {"EYWA" (str (sign-data user-context) ";path=/;")
                                      "Max-Age" (ntime/hours 16)
                                      ;; TODO - ENABLE THIS ON PRODUCTION
                                      ; "Secure" true
                                      "HttpOnly" true}))}
                       :body (sign-data user-context)})
                    (do
                      (log/trace "Credentials not valid. Returning 'Not authorized!'")
                      {:status 401
                       :body "Not authorized!"})))))))})


(def login
  {:name :login
   :enter
   (fn [context]
     (let [{{:keys [username password]} :form-params}
           (body-params/form-parser (:request context))]
       (log/tracef "Web login attempt for %s" username)
       (assoc context
              :response
              (do
                (log/tracef
                  "User %s not logged in jet. Trying to resolve credentials"
                  username)
                (if-let [{user-context :token-data
                          {redirect "landing_page"}
                          :settings} 
                         (check-credentials
                           {:username username
                            :password password})]
                  (do
                    (log/tracef "User %s credentials OK. Redirecting %s" username (or redirect "/app/eywa/"))
                    {:status 301
                     :headers {"Location" (if (and (string? redirect) (not-empty redirect))
                                            (str \/ redirect)
                                            "/app/eywa")
                               "Cache-Control" "no-cache"
                               "Content-Type" "text/html"
                               "Set-Cookie"
                               (clojure.string/join
                                 ";"
                                 (reduce
                                   (fn [r [k v]]
                                     (if (some? v)
                                       (conj r (str k "=" v))
                                       (conj r k)))
                                   []
                                   {"EYWA" (str (sign-data user-context) ";path=/;")
                                    "Max-Age" (ntime/hours 16)
                                    ;; TODO - ENABLE THIS ON PRODUCTION
                                    ; "Secure" true
                                    "HttpOnly" true}))}
                     :body "Redirecting to landing page..."})
                  {:status 301
                   :headers {"Location" (str "/index.html")
                             "Cache-Control" "no-cache"
                             "Set-Cookie" "EYWA=;path=/;max-age=-1;httpOnly;Secure;"}
                   :body "Not authorized!"})))))})



;; This was used when 
(def logout
  {:name :logout
   :enter
   (fn [{{{:keys [app]} :query-params} :request :as context}]
     (let [{{:keys [eywa/username]} :eywa.user/context} context]
       (log/debugf "%s logging out" username)
       (chain/terminate 
         (assoc context :response
                {:status 301
                 :headers {"Location" (str (when app (str "/app/" app)) "/index.html")
                           "Cache-Control" "no-cache"
                           "Set-Cookie" "EYWA=;path=/;max-age=-1;httpOnly;Secure;"}
                 :body "Not authorized!"}))))})
