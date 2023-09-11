(ns neyho.eywa.server.interceptors
  (:require 
    [neyho.eywa.transit
     :refer [eywa-read-handlers]]
    [cheshire.core :as cheshire]
    [io.pedestal.interceptor :refer [interceptor]]
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


(comment
  (cheshire.core/generate-string {:a (java.util.Date.)} {:date-format "yyyy-MM-dd'T'HH:mm:ss.SSS'Z'"})
  (json/write-str {:a (java.util.Date.)}))
