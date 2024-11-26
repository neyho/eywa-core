(ns neyho.eywa.server.ws.graphql
  (:require
    clojure.string
    [neyho.eywa.server.interceptors.authentication
     :refer [get-token-context]]
    [io.pedestal.interceptor :refer [interceptor]]
    [neyho.eywa.lacinia :as lacinia]
    [neyho.eywa.iam.access.context :refer [*user* *groups* *roles*]]
    [io.pedestal.interceptor.chain :as chain]
    [com.walmartlabs.lacinia.validator :as validator]
    [com.walmartlabs.lacinia.internal-utils :refer [to-message]]
    [com.walmartlabs.lacinia.parser :as parser]
    [io.pedestal.http :as http]
    [com.walmartlabs.lacinia.pedestal.subscriptions :as subscriptions])
  (:import
    [jakarta.websocket EndpointConfig Session]))


(defn ^:private on-leave-query-parser
  [context]
  (update context :request dissoc :parsed-lacinia-query))

(defn ^:private add-error
  [context exception]
  (assoc context ::chain/error exception))

(defn ^:private on-error-query-parser
  [context exception]
  (-> (on-leave-query-parser context)
      (add-error exception)))


(def query-parser-interceptor
  (interceptor
    {:name ::query-parser
     :enter (fn [context]
              (let [{operation-name :operationName
                     :keys [query variables]} (:request context)
                    actual-schema (deref lacinia/compiled)
                    parsed-query (try
                                   (parser/parse-query actual-schema query operation-name)
                                   (catch Throwable t
                                     (throw (ex-info (to-message t)
                                                     {::errors (-> t ex-data :errors)}
                                                     t))))
                    prepared (parser/prepare-with-query-variables parsed-query variables)
                    errors (validator/validate actual-schema prepared {})]
                (if (seq errors)
                  (throw (ex-info "Query validation errors." {::errors errors}))
                  (assoc-in context [:request :parsed-lacinia-query] prepared))))
     :leave on-leave-query-parser
     :error on-error-query-parser}))


(defn listeners
  [{:keys [app-context]}] 
  (let [wrapped-execute (interceptor
                          {:name (:name subscriptions/execute-operation-interceptor)
                           :leave (:leave subscriptions/execute-operation-interceptor)
                           :error (:error subscriptions/execute-operation-interceptor)
                           :enter
                           (fn [{:as ctx
                                 :keys [eywa/user eywa/groups eywa/roles]}]
                             (binding [*user* user
                                       *groups* groups
                                       *roles* roles]
                               ((:enter subscriptions/execute-operation-interceptor) ctx)))})]
    [subscriptions/exception-handler-interceptor
     subscriptions/send-operation-response-interceptor
     query-parser-interceptor
     (subscriptions/inject-app-context-interceptor app-context)
     wrapped-execute]))



(defn get-session-user
  [^Session session]
  (let [params (into {} (.getRequestParameterMap session))
        [token]  (get params "access_token")]
    (get-token-context token)))


(defn endpoint
  [options]
  (assoc
    (subscriptions/subscription-websocket-endpoint
      nil {:subscription-interceptors (listeners options)
           :session-initializer (fn [^Session session _]
                                  ; (def session session)
                                  ; (.getPathParameters session)
                                  ; (.getQueryString session)
                                  ; (.getRequestParameterMap session)
                                  (when-not (get-session-user session) 
                                    (.close session)))
           :context-initializer (fn [ctx ^Session session]
                                  (let [user-ctx (get-session-user session)]
                                    (merge ctx user-ctx)))
           :keep-alive-ms 30000})
    ; :configure (fn [builder]
    ;              (def builder builder))
    :subprotocols ["graphql-ws" "graphql-transport-ws"]))



(defn enable
  ([service-map] (enable service-map nil))
  ([service-map options]
   (assoc-in service-map [::http/websockets "/graphql-ws"]
             (endpoint options))))
