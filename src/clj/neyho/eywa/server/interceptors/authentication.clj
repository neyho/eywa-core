(ns neyho.eywa.server.interceptors.authentication
  (:require
    clojure.string
    clojure.java.io
    clojure.pprint
    [neyho.eywa.iam :as iam]
    [neyho.eywa.iam.oauth.core :refer [get-resource-owner]]
    [neyho.eywa.iam.oauth.token :refer [*tokens*]]
    [io.pedestal.interceptor.chain :as chain]))


(def user-data
  {:enter (fn [ctx]
            (assoc ctx :response
                   {:status 200
                    :headers {"Content-Type" "application/json"}
                    :body (dissoc (get-resource-owner (:euuid (:eywa/user ctx))) :_eid)}))})


(defn get-token [{{:keys [headers]} :request}]
  (let [{auth "authorization"} headers]
    (clojure.string/replace auth #"^Bearer\s+" "")))


(defn get-token-context
  [token]
  (let [{:keys [sub]
         sub-uuid "sub:uuid"} (when token
                                (try
                                  (iam/unsign-data token)
                                  (catch Throwable _ nil)))]
    (when (some? sub)
      (let [{:keys [groups roles] :as user} (get-resource-owner (or
                                                                  (when sub-uuid (java.util.UUID/fromString sub-uuid))
                                                                  sub))]
        #:eywa {:user (select-keys user [:_eid :euuid :name :active])
                :roles roles
                :groups groups}))))


(def authenticate
  {:name :authenticate
   :enter
   (fn [ctx]
     (let [not-authorized (assoc ctx :response {:status 403
                                                :headers {"WWW-Authenticate" "Bearer"}
                                                :body "Not authorized"})
           token (get-token ctx)]
       (if (contains? (get @*tokens* :access_token) token)
         (let [token-context (get-token-context token)]
           (if (nil? token-context)
             (chain/terminate not-authorized)
             (merge ctx token-context)))
         (chain/terminate not-authorized))))})
