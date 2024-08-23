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

; (ns-unmap 'neyho.eywa.server.jetty 'unsign-data)


(defn make-listener
  [{:keys [user groups roles]}]
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
                token-ctx  (get-token-context token)]
            (if (nil? token-ctx)
              (doto
                resp
                (.sendForbidden "Not authorized!")
                (.setStatusCode 401))
              (do
                (def req req)
                (def resp resp)
                (def ws-map ws-map)
                (log/tracef "Connecting GraphQL subscription\n%s" req)
                ((make-listener token-ctx) req resp ws-map))))))})))
;
;
; (defn eywa-context-resolver
;   ([req]
;    (let [cookies (.getCookies req)
;          ip-address (.getRemoteAddress req)
;          host (.getRemoteHostName req)
;          ; port (.getRemoteHostName req)
;          eywa-token (some 
;                       (fn [cookie]
;                         (when (= "EYWA" (.getName cookie)) 
;                           (.getValue cookie)))
;                       cookies)
;          [authorization-header] (.get (.getHeaders req) "Authorization")
;          access-token (first (get (.getParameterMap req) "access_token"))
;          eywa-token (or
;                       access-token
;                       authorization-header 
;                       eywa-token)]
;      (if (empty? eywa-token)
;        (do
;          (log/errorf
;            "Couldn't authenticate user\n%s"
;            {:ip ip-address
;             :host host
;             :authorization-header authorization-header 
;             :access-token access-token
;             :cookies cookies})
;          (throw
;            (ex-info
;              "Couldn't authenticate user"
;              {:cookies cookies})))
;        (do
;          (log/tracef
;            "Unsigning token\n%s"
;            {:ip ip-address
;             :host host
;             :req req
;             :token eywa-token})
;          (let [user (if (jwt-token? eywa-token)
;                       (unsign-data eywa-token)
;                       (get-token-user-details eywa-token))
;                roles (set (map #(java.util.UUID/fromString %) (:eywa/roles user)))]
;            #_(log/tracef
;                "Token successfully unsigned:\n%s"
;                (with-out-str (clojure.pprint/pprint user)))
;            (hash-map
;              :token eywa-token
;              :ip ip-address
;              :host host
;              :user (:eywa/id user)
;              :username (:eywa/username user)
;              :root? (contains? roles (:euuid *ROOT*))
;              :groups (set (map #(java.util.UUID/fromString %) (:eywa/groups user)))
;              :roles roles)))))))
;
;
; (defn context-configuration
;   ([ctx]
;    (ws/add-ws-endpoints
;      ctx {"/graphql-ws" :eywa.dataset/subscriptions}
;      {:listener-fn
;       (fn [req resp ws-map]
;         (letfn [(app-context []
;                   (try
;                     (eywa-context-resolver req)
;                     (catch clojure.lang.ExceptionInfo _
;                       ;; Token is expired error
;                       #_(log/errorf (ex-message e)))
;                     (catch Throwable e
;                       (log/error (ex-message e)))))
;                 (forbidden []
;                   (doto
;                     resp
;                     (.sendForbidden "Not authorized!")
;                     (.setStatusCode 401)))]
;           (case ws-map
;             ;;
;             :eywa.dataset/subscriptions
;             (if-some [app-context (app-context)]
;               (do
;                 (log/tracef "Connecting GraphQL subscription\n%s" req)
;                 ((subscriptions/listener-fn-factory
;                    @lacinia/compiled
;                    {:app-context app-context
;                     :keep-alive-ms 30000})
;                  req resp ws-map))
;               (forbidden)))))})))
