(ns neyho.eywa.iam.oauth2.client
  "This namespace is used to integrate with other OIDC providers."
  (:require
    nano-id.core
    [clojure.string :as str]
    [clojure.data.json :as json]
    [ring.util.codec :as codec])
  (:import
    [java.lang String]
    [java.util Base64]
    [org.eclipse.jetty.client HttpClient]
    [org.eclipse.jetty.client.api ContentResponse]
    [org.eclipse.jetty.client.util StringContentProvider]
    [org.eclipse.jetty.util.ssl SslContextFactory$Client]
    [org.eclipse.jetty.http HttpMethod]))

(def alphabet "ACDEFGHJKLMNOPQRSTUVWXYZ")

(defonce gen-id (nano-id.core/custom alphabet 30))
(defn gen-secret [] (nano-id.core/nano-id 50))


(defn generate-client
  ([] (generate-client nil))
  ([data]
   (merge
     {:id (gen-id)
      :password (gen-secret)}
     data )))


(defn request
  [{:keys [method url headers form-params]
    :or {method :get
         headers {"Content-Type" "application/x-form-urlencoded"}}}]
  (let [client (if (str/starts-with? url "https")
                 (HttpClient. (SslContextFactory$Client.))
                 (HttpClient.))]
    (.start client)
    (try
      (let [;; url (if-not form-params url
            ;;       (str url "?" (codec/form-encode form-params)))
            ;; _ (println "SENDING TO URL: " url)
            response (as-> (.newRequest client url) request
                       (reduce-kv
                         (fn [r k v]
                           (.header r k v)
                           r)
                         request
                         headers)
                       (if-not form-params request
                         (do
                           (.content request 
                                     (StringContentProvider.
                                       (codec/form-encode form-params)))
                           request))
                       (.method request (case method
                                           :get HttpMethod/GET
                                           :post HttpMethod/POST))
                       (.send request))
            headers (.getHeaders response)
            status (.getStatus response)
            body (.getContentAsString response)]
        {:status status 
         :headers headers 
         :body body})
      (finally
        (.stop client)))))


(defn- encode-basic-authorization
  [username password]
  (let [ba (str username (when password (str ":" password)))
        encoded (.encode (Base64/getEncoder) (.getBytes ba "UTF-8"))]
    (String. encoded "UTF-8")))


(comment
  (basic-authorization "robi" "bototo"))


(defn resource-owner-password
  [{:keys [url
           client_id client_password
           client-id client-password
           username password state scope]}]
  (let [client-id (or client_id client-id)
        client-password (or client_password client-password)
        {:keys [status body]}
        (request
          (cond->
            {:url url
             :headers {"Content-Type" "application/x-www-form-urlencoded"
                       "Authorization" (str "Basic " (encode-basic-authorization client-id client-password))}
             :method :post
             :form-params (cond->
                            {:grant_type "password"
                             :username username
                             :password password}
                            ; (some? client-password) (assoc :client_password client-password)
                            (some? state) (assoc :state state)
                            (some? scope) (assoc :scope (str/join " " scope)))}))
        body (try
               (json/read-str body)
               (catch Throwable _
                 (println body)
                 {:error "unknown" :error_description "Returned body wasn't in json format"}))]
    (case status
      200 body
      (let [{:strs [error error_description]} body]
        (throw
          (ex-info error_description
                   {:error error}))))))


(defn client-credentials 
  [{:keys [url
           client_id client_password
           client-id client-password
           state scope]
    :as params}]
  (let [client-id (or client_id client-id)
        client-password (or client_password client-password)
        custom-params (dissoc params
                              :client-id :client_id
                              :client-password :client_password
                              :scope :state url)
        {:keys [status body]}
        (request
          (cond->
            {:url url
             :headers {"Content-Type" "application/x-www-form-urlencoded"
                       "Authorization" (str "Basic " (encode-basic-authorization client-id client-password))}
             :method :post
             :form-params (cond->
                            (assoc custom-params :grant_type "client_credentials")
                            (some? state) (assoc :state state)
                            (some? scope) (assoc :scope (str/join " " scope)))}))
        body (try
               (json/read-str body)
               (catch Throwable _
                 (println body)
                 {:error "unknown" :error_description "Returned body wasn't in json format"}))]
    (case status
      200 body
      (let [{:strs [error error_description]} body]
        (throw
          (ex-info error_description
                   {:error error}))))))


(defn authorization-code-request
  [{:keys [url
           client_id client_password
           client-id client-password state]}]
  (let [client-id (or client_id client-id)
        client-password (or client_password client-password)
        {:keys [status body]}
        (request
          (cond->
            {:url url
             :headers {"Content-Type" "application/x-www-form-urlencoded"
                       "Authorization" (str "Basic " (encode-basic-authorization client-id client-password))}
             :method :get
             :form-params (cond->
                            {:response_type "code"}
                            (some? state) (assoc :state state))}))
        _ (println "BODY: " body)
        body (json/read-str body)]
    (case status
      200 body
      (let [{:strs [error error_description]} body]
        (throw
          (ex-info error_description
                   {:error error}))))))
