(ns neyho.eywa.server.interceptors.authentication
  (:require
   clojure.string
   clojure.java.io
   clojure.pprint
   [environ.core :refer [env]]
   [neyho.eywa.data :refer [*PUBLIC_ROLE* *PUBLIC_USER*]]
   [neyho.eywa.iam :as iam]
   [neyho.eywa.iam.access :as access]
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
    (when (not-empty auth) (clojure.string/replace auth #"^Bearer\s+" ""))))

(defn get-token-context
  [token]
  (let [{:keys [sub]
         sub-uuid "sub:uuid"} (when token
                                (try
                                  (iam/unsign-data token)
                                  (catch Throwable _ nil)))]
    (when (some? sub)
      (let [{:keys [groups roles] :as user}
            (get-resource-owner (or
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
           token (get-token ctx)
           has-token (> (count token) 6)]
       (cond
         ;;
         (and has-token (contains? (get @*tokens* :access_token) token))
         (let [token-context (get-token-context token)]
           (if (nil? token-context)
             (chain/terminate not-authorized)
             (merge ctx token-context)))
         ;;
         (and has-token (not (contains? (get @*tokens* :access_token) token)))
         (chain/terminate not-authorized)
         ;;
         (and
          (access/enforced?)
          (contains?
           #{"true" "TRUE" "y" "yes" "1"}
           (env :eywa-iam-allow-public)))
         ;;
         (let [public (:euuid *PUBLIC_ROLE*)
               public-user #:eywa {:user (select-keys *PUBLIC_USER* [:_eid :euuid :name :active])
                                   :roles #{public}
                                   :groups #{}}]
           (merge ctx public-user))
         ;;
         :else
         (chain/terminate not-authorized))))})
