(ns neyho.eywa.server.jetty
  (:require
    clojure.string
    [clojure.tools.logging :as log]
    [neyho.eywa.server.interceptors.authentication
     :refer [get-token-context]]
    [io.pedestal.interceptor :refer [interceptor]]
    [io.pedestal.http.jetty.websockets :as ws]
    [neyho.eywa.lacinia :as lacinia]
    [neyho.eywa.iam.access.context :refer [*user* *groups* *roles*]]
    [com.walmartlabs.lacinia.pedestal.subscriptions :as subscriptions]))


(defn make-listener
  [{:keys [eywa/user eywa/groups eywa/roles]}]
  (let [wrapped-execute (interceptor
                          {:name (:name subscriptions/execute-operation-interceptor)
                           :leave (:leave subscriptions/execute-operation-interceptor)
                           :error (:error subscriptions/execute-operation-interceptor)
                           :enter (fn [ctx]
                                    (binding [*user* user
                                              *groups* groups
                                              *roles* roles]
                                      ((:enter subscriptions/execute-operation-interceptor) ctx)))})
        interceptors [subscriptions/exception-handler-interceptor
                      subscriptions/send-operation-response-interceptor
                      (subscriptions/query-parser-interceptor @lacinia/compiled)
                      (subscriptions/inject-app-context-interceptor nil)
                      wrapped-execute]]
    ; (log/tracef "Connecting GraphQL subscription\n%s" req)
    (subscriptions/listener-fn-factory
      @lacinia/compiled
      {:subscription-interceptors interceptors
       :keep-alive-ms 30000})))


(defn context-configuration
  ([ctx]
   (ws/add-ws-endpoints
     ctx {"/graphql-ws" :eywa.dataset/subscriptions}
     {:listener-fn
      (fn [req resp ws-map]
        (case ws-map
          ;;
          :eywa.dataset/subscriptions
          (let [token (first (get (.getParameterMap req) "access_token"))
                token-ctx (get-token-context token)]
            (if (nil? token-ctx)
              (doto
                resp
                (.sendForbidden "Not authorized!")
                (.setStatusCode 401))
              ((make-listener token-ctx) req resp ws-map)))))})))
