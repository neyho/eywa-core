(ns neyho.eywa.iam.oauth.device-code
  (:require
    [clojure.string :as str]
    clojure.java.io
    clojure.pprint
    [clojure.tools.logging :as log]
    [nano-id.core :as nano-id]
    [io.pedestal.interceptor.chain :as chain]
    [neyho.eywa.iam.oauth.core :as core]))


(defonce ^:dynamic *device-codes* (atom nil))


(def gen-device-code (nano-id/custom "ACDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz" 20))
(def gen-user-code (nano-id/custom "0123456789" 6))


(defn device-code-response
  [request]
  (log/debugf "Device code request:\n%s" request)
  (letfn [(split-spaces [request k]
            (if-some [val (get request k)]
              (assoc request k (set (str/split val #"\s+")))
              request))]
    (let [{:keys [response_type redirect_uri] :as request}
          (-> request
              (split-spaces :scope)
              (split-spaces :response_type))
          device-code (gen-device-code)
          user-code (gen-user-code)]
      ;; Treba checkirati klijenta
      ;; dohvatiti konfiguraciju za klijenta
      {:device_code device-code
       :user_code user-code
       :verification_uri (core/domain+ "/oauth/activate-device")
       :verification_uri_complete (core/domain+ (str "/oauth/activate-device?user_code=" user-code))
       :interval 5
       :expires_in 900})))


(def device-code-flow-interceptor
  {:name ::device-code-flow
   :enter
   (fn [{{:keys [params]} :request :as context}]
     (chain/terminate (assoc context :response (device-code-response params))))})


(def device-confirm-interceptor
  {:name ::device-confirm
   :enter (fn [{{:keys [params]} :request :as context}]
            (chain/terminate (assoc context :response (device-code-response params))))})
