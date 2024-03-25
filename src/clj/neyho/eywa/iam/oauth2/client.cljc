(ns neyho.eywa.iam.oauth2.client
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
  (let [client (HttpClient.)]
    (.start client)
    (try
      (let [url (if-not form-params url
                  (str url "?" (codec/form-encode form-params)))
            response (as-> (.newRequest client url) request
                       (reduce-kv
                         (fn [r k v]
                           (.header r k v)
                           r)
                         request
                         headers)
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
             :headers {"Content-Type" "application/x-form-urlencoded"
                       "Authorization" (str "Basic " (encode-basic-authorization client-id client-password))}
             :method :post
             :form-params (cond->
                            {:grant_type "password"
                             :username username
                             :password password}
                            ; (some? client-password) (assoc :client_password client-password)
                            (some? state) (assoc :state state)
                            (some? scope) (assoc :scope (str/join " " scope)))}))
        body (json/read-str body)]
    (case status
      200 body
      (let [{:strs [error error_description]} body]
        (throw
          (ex-info error_description
                   {:error error}))))))


(comment
  (request {:url nil
            :client_id "jifoq"
            :client_password "ifoq"})

  (neyho.eywa.iam/reset)

  (try
    (resource-owner-password
      {:url "http://localhost:8080/oauth2/token"
       :client_id "XFYWDCONOFSZMTVAEOQHTZFHSUCTXQ"
       :client_password "fjiqooi"
       :username "oauth_test"
       :password "change-me"})
    (catch clojure.lang.ExceptionInfo ex
      (ex-data ex)))


  (resource-owner-password
    {:url "http://localhost:8080/oauth2/token"
     :client_id "XFYWDCONOFSZMTVAEOQHTZFHSUCTXQ"
     :client_password "e9w7BwGDTLBgaHYxMpctUrOy_aVA4tiZHlgfb2GrotWiBhr_u0"
     :username "oauth_test"
     :password "change-me"})

  (resource-owner-password
    {:url "http://localhost:8080/oauth2/token"
     :client_id "ZHXGGUGLQVOSJZHZCETLFTUZWSSRWG"
     :client_password "e9w7BwGDTLBgaHYxMpctUrOy_aVA4tiZHlgfb2GrotWiBhr_u0"
     :username "oauth_test"
     :password "change-me"})

  (request
    {:url "http://localhost:8080/oauth2/authorize"
     :form-params {:client_id "ZHXGGUGLQVOSJZHZCETLFTUZWSSRWG"
                   :redirect_uri "http://localhost:8080/eywa"
                   :response_type "code"
                   :state "100292"}})
  (println
    (:body
      )))
