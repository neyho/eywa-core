(ns neyho.eywa.server
  (:require
   [environ.core :refer [env]]
   [clojure.java.io :as io]
   clojure.pprint
   clojure.string
   clojure.set
   [clojure.tools.logging :as log]
   [io.pedestal.http :as http]
   [io.pedestal.http.cors :as cors]
   [io.pedestal.http.route :as route]
   [io.pedestal.http.content-negotiation :as conneg]
   [io.pedestal.http.ring-middlewares :as middlewares]
   [com.walmartlabs.lacinia.pedestal2 :as lp]
   neyho.eywa.lacinia
   [neyho.eywa.iam.oidc :as oidc]
   [neyho.eywa.iam.access.context :refer [*user* *roles* *groups*]]
   [neyho.eywa.server.ws.graphql :as ws.graphql]
   [neyho.eywa.server.interceptors :refer [make-info-interceptor json-response-interceptor make-spa-interceptor]]
   [neyho.eywa.server.interceptors.util :refer [coerce-body]]
   [neyho.eywa.server.interceptors.authentication :as authentication
    :refer [user-data
            authenticate]]))

(def echo-integration
  {:name :eywa/transit-integration-echo
   :enter
   (fn [{:keys [request] :as context}]
     (let [params (reduce
                   merge
                   {}
                   (vals
                    (select-keys request [:json-params :transit-params])))]
       (assoc context :response {:status 200 :body params})))})

(def supported-types ["text/html" "application/transit+json" "application/edn" "application/json" "text/plain"])
(def content-neg-intc (conneg/negotiate-content supported-types))

(defn default-routes
  [info]
  #{["/eywa/whoami" :get [authenticate coerce-body content-neg-intc user-data] :route-name :eywa.identity/get]
    ["/eywa/info" :get [authenticate coerce-body content-neg-intc (make-info-interceptor info)] :route-name :eywa.version/get]})

(def graphql-routes
  (let [_schema (fn [] (deref neyho.eywa.lacinia/compiled))
        options {:api-path "/graphql"}
        wrapped-query-executor {:enter (fn [{:eywa/keys [user roles groups]
                                             :as ctx}]
                                         (binding [*user* user
                                                   *roles* roles
                                                   *groups* groups]
                                           ((:enter lp/query-executor-handler) ctx)))}
        interceptors [authenticate
                      lp/initialize-tracing-interceptor
                      json-response-interceptor
                      lp/error-response-interceptor
                      lp/body-data-interceptor
                      lp/graphql-data-interceptor
                      lp/status-conversion-interceptor
                      lp/missing-query-interceptor
                      (lp/query-parser-interceptor _schema (:parsed-query-cache options))
                      lp/disallow-subscriptions-interceptor
                      lp/prepare-query-interceptor
                      (lp/inject-app-context-interceptor nil)
                      lp/enable-tracing-interceptor
                      wrapped-query-executor]]
    (into
     #{["/graphql" :post interceptors :route-name ::graphql-api]})))

(defonce server (atom nil))

(defn stop []
  (when (some? @server)
    (log/info "Restarting server")
    (http/stop @server)
    (reset! server nil)))

(defn development-environment
  [service-map]
  (if-not (env :eywa-development) service-map
          (http/dev-interceptors service-map)))

(defn start
  ([] (start nil))
  ([{:keys [host port
            routes
            service-initializer
            info]
     :or {host "localhost"
          port 8080
          service-initializer identity}}]
   (log/infof "Starting %s server %s:%s" (env :eywa-bucket "EYWA") host port)
   (stop)
   (comment (def info nil))
   (let [routes (or routes
                    (route/expand-routes
                     (clojure.set/union
                       (default-routes info)
                       graphql-routes
                       oidc/routes)))
         router (route/router routes :map-tree)
         _server (->
                  {::http/type :jetty
                   ::http/join? false
                   ::http/host host
                   ::http/port port
                   ::http/interceptors
                   [;; Constantly true... If there is need for CORS protection than apply
                    ;; CORS protection at that routes, otherwise this is necessary because
                    ;; Access-Control-Allow-Headers have to be returned, otherwise browser
                    ;; will report error
                    (cors/allow-origin {:allowed-origins (constantly true)})
                    (middlewares/content-type {:mime-types {}})
                    route/query-params
                    (route/method-param)
                    router
                    (make-spa-interceptor (env :eywa-serve))]}
                  ws.graphql/enable
                  service-initializer
                   ; development-environment
                  http/create-server)]
     (reset! server (http/start _server))
     (log/infof "%s server started @ %s:%s" (env :eywa-bucket "EYWA") host port))))

(comment
  (stop)
  (start))
