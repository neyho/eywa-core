(ns neyho.eywa.server.interceptors.util
  (:require
    [clojure.data.json :as json]
    [cognitect.transit :as transit])
  (:import
    (org.apache.commons.codec.binary Base64InputStream)))

(defn accepted-type
  [context]
  (get-in context [:request :accept :field]))


(defn transform-content
  [body content-type]
  (case content-type
    "application/edn"  (pr-str body)
    "application/json" (json/write-str body)
    "application/transit+json" (with-open [baos (java.io.ByteArrayOutputStream.)]
                                 (let [w (transit/writer baos :json)
                                       _ (cognitect.transit/write w body)
                                       ret (.toString baos)]
                                   (.reset baos)
                                   ret))
    "application/octet-stream" (Base64InputStream. body true)
    body))


(defn coerce-to
  [response content-type]
  (-> 
    response
    (update :body transform-content content-type)
    (assoc-in [:headers "Content-Type"] content-type)))

(def coerce-body
  {:name ::coerce-body
   :leave
   (fn [context]
     (cond-> context
       ;;
       (nil? (get-in context [:response :body :headers "Content-Type"]))
       (update-in [:response] coerce-to (accepted-type context))))})


(comment
  (json/write-str
    {:a (java.util.Date.)})
  (println "HI"))
