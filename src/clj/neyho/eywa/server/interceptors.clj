(ns neyho.eywa.server.interceptors
  (:require 
    [clojure.string :as str]
    [clojure.java.io :as io]
    [clojure.tools.logging :as log]
    [ring.util.response :as response]
    [ring.middleware.head :as head]
    [neyho.eywa.transit
     :refer [eywa-read-handlers]]
    [cheshire.core :as cheshire]
    [io.pedestal.interceptor :refer [interceptor]]
    [io.pedestal.interceptor.chain :as chain]
    [io.pedestal.http.content-negotiation :as conneg]
    [io.pedestal.http.body-params :as body-params]))


(def supported-types ["text/html" "application/transit+json" "application/edn" "application/json" "text/plain"])
(def content-neg-intc (conneg/negotiate-content supported-types))


(defn on-leave-json-response
  [context]
  (let [body (get-in context [:response :body])]
    (if (map? body)
      (-> context
          (assoc-in [:response :headers "Content-Type"] "application/json")
          (update-in [:response :body] #(cheshire/generate-string % {:date-format "yyyy-MM-dd'T'HH:mm:ss.SSS'Z'"})))
      context)))


(def json-response-interceptor
  "An interceptor that sees if the response body is a map and, if so,
  converts the map to JSON and sets the response Content-Type header."
  (interceptor
    {:name ::json-response
     :leave on-leave-json-response}))


(def transit-body-params
  (body-params/body-params
    (body-params/default-parser-map
      :transit-options [{:handlers eywa-read-handlers}])))


(def spa-interceptor
  {:name ::spa
   :enter
   (fn [{{:keys [path-info uri] :as request} :request
         :as context}]
     (let [extension (re-find #"(?<=\.).*?$" uri)] 
       ;; If extension exists
       (if (some? extension)
         ;; Try to return that file
         (as-> (or path-info uri) path
           (if (.startsWith path "/")
             (subs path 1)
             path)
           (do
             (log/tracef "Returning resource file %s, for path %s" uri path-info)
             (if (io/resource path)
               (assoc context :response
                      (-> (response/resource-response path)
                          (head/head-response request)))
               (chain/terminate
                 (assoc context
                        :response {:status 404
                                   :body "Not found!"})))))
         ;; Otherwise proceed
         context)))
   ;; Leave will only happen when requested URI ends without extension
   ;; That is when static file isn't directly required
   :leave
   (fn [{{:keys [path-info uri] :as request} :request
         response :response
         :as context}]
     (if response
       ;; If there is some kind of response
       context
       ;; Otherwise return root html
       (loop [sections (remove empty? (str/split (or path-info uri) #"/"))]
         (if (empty? sections)
           ;; If there are no more sections
           (if-some [target (io/resource "index.html")]
             (assoc context :response
                    (-> (response/resource-response target)
                        (assoc-in [:headers "Content-Type"] "text/html")
                        (head/head-response request)))
             (chain/terminate
               (assoc context
                      :response {:status 404
                                 :body "Not found!"})))
           ;;
           (let [target (str
                          (str/join "/" sections)
                          "/index.html")]
             (if (io/resource target)
               (assoc context :response
                      (-> (response/resource-response target)
                          (assoc-in [:headers "Content-Type"] "text/html")
                          (head/head-response request)))
               (recur (butlast sections))))))))})


(comment
  (def uri nil)
  (def path-info nil)
  (def context nil)
  (def request nil)
  (io/resource "login/index.html")
  (io/resource "neyho/eywa/iam/oauth/test_mem.clj")
  (cheshire.core/generate-string {:a (java.util.Date.)} {:date-format "yyyy-MM-dd'T'HH:mm:ss.SSS'Z'"})
  (json/write-str {:a (java.util.Date.)}))
