(ns neyho.eywa.server
  (:require
    [environ.core :refer [env]]
    [clojure.java.io :as io]
    clojure.pprint
    clojure.string
    clojure.set
    [clojure.tools.logging :as log]
    [ring.util.response :as response]
    [ring.middleware.head :as head]
    [io.pedestal.http :as http]
    [io.pedestal.http.cors :as cors]
    [io.pedestal.interceptor :as interceptor]
    [io.pedestal.interceptor.chain :as chain]
    [io.pedestal.http.route :as route]
    [io.pedestal.http.content-negotiation :as conneg]
    [io.pedestal.http.ring-middlewares :as middlewares]
    [com.walmartlabs.lacinia.pedestal2 :as lp]
    neyho.eywa.lacinia
    [neyho.eywa.iam.oidc :as oidc]
    [neyho.eywa.iam.access.context :refer [*user* *roles* *groups*]]
    [neyho.eywa.server.jetty :as jetty]
    [neyho.eywa.server.interceptors :refer [json-response-interceptor make-spa-interceptor]]
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


(def assets
  {:name :extension.asset/service
   :enter
   (fn [{{:keys [eywa/account]} :eywa.user/context 
         {:keys [path-info uri]} :request
         :as context}]
     (let [path' (str \/ account "/assets" (subs (or path-info uri) 7))
           context' (update context :request merge {:path-info path' :uri path'})]
       context'))})


(def apps
  (let [root "app"]
    {:name :extension.app/service
     :enter
     (fn [{{:keys [path-info uri] :as request} :request
           :as context}]
       (log/trace "Looking for web resource")
       (let [extension (re-find #"(?<=\.).*?$" uri)] 
         ;; If extension exists
         (if (some? extension)
           ;; Try to return that file
           (let [path (str root (subs (or path-info uri) 4))] 
             (log/tracef
               "Returning resource file %s, for path %s" uri path-info)
             (if (io/resource path)
               (assoc context :response
                      (-> (response/resource-response path)
                          (head/head-response request)))
               (chain/terminate
                 (assoc context
                        :response {:status 404
                                   :body "Not found!"}))))
           ;; Otherwise proceed
           context)))
     :leave
     (fn [{{{app :app} :path-params
            :as request} :request
           response :response
           :as context}]
       (if response
         ;; If there is some kind of response
         context
         ;; Otherwise return root html
         (let [app-root (or
                          (re-find #"^\w.*?(?=[\./\?])" app)
                          app)
               html (str root \/ app-root "/index.html")] 
           (if (io/resource html)
             (assoc context :response
                    (-> (response/resource-response html)
                        (assoc-in [:headers "Content-Type"] "text/html")
                        (head/head-response request)))
             (chain/terminate
               (assoc context
                      :response {:status 404
                                 :body "Not found!"}))))))}))


(def docs
  {:name :extension.app/service
   :enter
   (fn [{{:keys [path-info uri] :as request} :request
         :as context}]
     (log/info "Looking for web resource: ")
     (let [path (subs (or path-info uri) 1)] 
       (log/tracef
         "Returning resource file %s, for path %s" uri path-info)
       (if (io/resource path)
         (assoc context :response
                (-> (response/resource-response path)
                    (head/head-response request)))
         (let [html "eywa/docs/index.html"] 
           (log/trace "Couldn't find resource returning index.html")
           (if (io/resource html)
             (assoc context :response
                    (-> (response/resource-response html)
                        (assoc-in [:headers "Content-Type"] "text/html")
                        (head/head-response request)))
             (chain/terminate
               (assoc context
                      :response {:status 404
                                 :body "Not found!"})))))))})


(def pages
  {:name :extension.page/service
   :enter
   (fn [{{:keys [eywa/account]} :eywa.user/context 
         {:keys [path-info uri]} :request
         :as context}]
     (let [path' (str "/accounts" \/ account "/page" (subs (or path-info uri) 5))
           context' (update context :request merge {:path-info path' :uri path'})]
       context'))})


(def custom-logins
  {:name :extension.login/service
   :enter
   (fn [{{{:keys [account]} :path-params
          :as request} :request
         :as context}]
     (let [[account' ext] (clojure.string/split account #"\.+")
           path' (str "accounts/" account' "/login." (or ext "html"))
           options {:root "./" 
                    :index-files? true
                    :allow-symlinks? false}]
       (assoc context :response
              (-> (response/file-response path' options)
                  (head/head-response request)))))})


(def append-account
  {:name :extension.login/append-account
   :enter
   (fn [{{{:keys [account]} :path-params} :request
         :as context}]
     (let [[account' _] (clojure.string/split account #"\.+")]
       (assoc-in context [:eywa.user/context :eywa/account] account')))})


(def redirect-to-login
  {:name :eywa/default-redirect 
   :enter
   (fn [context]
     (log/trace "Redirecting to login page")
     (chain/terminate
       (assoc context :response
              {:status 301
               :headers {"Location" "/index.html"}})))})


(def default-routes
  #{["/eywa/whoami" :get [authenticate coerce-body content-neg-intc user-data] :route-name :eywa.identity/get]})


(def graphiql-spa
  (try
    {:status 200
     :headers {"Content-Type" "text/html"}
     :body (slurp (io/resource "graphiql/index.html"))}
    (catch Throwable _ "GraphiQL N/A")))


(def graphiql
  {:enter
   (fn [{:keys [request] :as context}]
     (let [path (:uri request)]
       (letfn [(resource-response []
                 (let [path (try
                              (str "graphiql/" (subs path 12))
                              (catch Throwable _ nil))]
                   (if (and
                         path
                         (clojure.string/starts-with? path "graphiql/static")) 
                     (assoc context :response
                            (-> (response/resource-response path)
                                (head/head-response request)))
                     (assoc context :response graphiql-spa))))]
         (chain/terminate (resource-response)))))})


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
      #{["/graphql" :post interceptors :route-name ::graphql-api]
        #_["/graphql/*path" :get graphiql :route-name ::graphql-ide]})))



(defonce server (atom nil))


(defn stop []
  (when (some? @server)
    (log/info "Restarting server")
    (http/stop @server)))


(def index-html
  (try
    (slurp
      (or
        ;; This is production
        (io/resource "eywa/index.html")
        ;; This is development
        (io/resource "index.html")))
    (catch Throwable _ "EYWA N/A")))


(defonce _response (atom nil))


;; { "site-name" "resource-directory" } e.g.
;; { "ring" "tmp/ring}
(def sites-map
  {"s1" "service1"
   "s2" "service2"
   "ring" "ring"})


;; redirect when missing trailing slash:
;; * relative links don't work when slash is missing
;; * assists with content-type determination - assumes text/html
(def sites-redirect
  (interceptor/interceptor
    {:name :sites-redirect
     :enter
     (fn [{{:keys [uri]} :request :as context}]
       (if ((into #{} (keys sites-map)) (subs uri 1))
         (chain/terminate (assoc context :response (response/redirect (str uri "/"))))
         context))}))


(letfn [(get-first-uri [uri sites]
          (some
            (fn [[site dest]]
              (when (clojure.string/starts-with? uri site)
                (clojure.string/replace-first uri site dest)))
            sites))
        (add-content-type [uri resp]
          (if (clojure.string/ends-with? uri "/")
            (response/content-type resp "text/html")
            resp))]
  (def sites
    (interceptor/interceptor
      {:name :sites
       :enter
       (fn [{{:keys [uri]} :request :as context}]

         (if-let [resource (io/resource (get-first-uri (subs uri 1) sites-map))]
           (chain/terminate (assoc context :response
                                   (add-content-type
                                     uri
                                     (response/file-response (.getPath resource)))))
           context))})))


(def eywa-web-interceptor
  (let [eywa-spa {:status 200
                  :headers {"Content-Type" "text/html"}
                  :body index-html}]
    (interceptor/interceptor
      {:name :eywa/web
       :enter (fn [{:keys [request] :as context}]
                (let [path (:uri request)]
                  (letfn [(resource-response []
                            (let [path (subs path 1)]
                              (if (io/resource path)
                                (assoc context :response
                                       (let [response (-> (response/resource-response path)
                                                          (head/head-response request)
                                                          (assoc-in [:headers "Cache-Control"] "max-age=500000"))]
                                         (reset! _response response)
                                         response))
                                (chain/terminate
                                  (assoc context
                                         :response {:status 404
                                                    :body "Not found!"})))))]
                    (case path
                      "/" (chain/terminate  (assoc context :response eywa-spa))
                      "/index.html" (chain/terminate  (assoc context :response eywa-spa))
                      ; "/index.html" (chain/terminate  (assoc context :response old-redirect))
                      ; "/" (chain/terminate  (assoc context :response old-redirect)) 
                      ;;
                      "/eywa/index.html"
                      (do
                        (log/infof "Serving eywa/index.html")
                        (chain/terminate (resource-response)))
                      ;;
                      (condp #(clojure.string/starts-with? %2 %1) path
                        ;;
                        "/eywa/css"
                        (chain/terminate (resource-response))
                        ;;
                        "/eywa/images"
                        (chain/terminate (resource-response))
                        ;;
                        "/eywa/js"
                        (chain/terminate (resource-response))
                        ;;
                        "/eywa"
                        (condp #(clojure.string/starts-with? %2 %1) path
                          "/eywa/token" context
                          "/eywa/whoami" context
                          "/eywa/access" context
                          "/eywa/login" context
                          "/eywa/logout" context
                          "/eywa/avatars" context
                          "/eywa/app" context
                          "/eywa/docs" context
                          (chain/terminate (assoc context :response (-> eywa-spa (head/head-response request)))))
                        ;; Try to resolve mapping
                        context)))))})))


(defn development-environment
  [service-map]
  (if-not (env :eywa-development) service-map
    (http/dev-interceptors service-map)))


(defn start
  ([] (start nil))
  ([{:keys [host port context-configurator routes]
     :or {host "localhost"
          port 8080
          routes (route/expand-routes
                   (clojure.set/union
                     default-routes
                     graphql-routes
                     oidc/routes))
          context-configurator jetty/context-configuration}}]
   (log/infof "Starting EYWA server %s:%s" host port)
   (stop)
   (let [router (route/router routes :map-tree)
         _server (->
                   {::http/type :jetty
                    ::http/join? false
                    ::http/host host 
                    ::http/port port 
                    ::http/container-options {:context-configurator context-configurator}
                    ::http/interceptors
                    [;; Constantly true... If there is need for CORS protection than apply
                     ;; CORS protection at that routes, otherwise this is necessary because
                     ;; Access-Control-Allow-Headers have to be returned, otherwise browser
                     ;; will report error
                     (cors/allow-origin {:allowed-origins (constantly true)})
                     (middlewares/content-type {:mime-types {}})
                     route/query-params
                     (route/method-param)
                     ; (sec-headers/secure-headers {:content-security-policy-settings {:object-src "none"}})
                     ; eywa-web-interceptor
                     router
                     (make-spa-interceptor (env :eywa-serve))
                     ; (middlewares/resource "public")
                     ; (interceptor/interceptor http/not-found)
                     ]
                    ; ::http/secure-headers {:content-security-policy-settings {:object-src "none"}}
                    ; ::http/file-path "web/public"
                    }
                   development-environment
                   http/create-server)]
     (reset! server (http/start _server))
     (log/infof "EYWA server started @ %s:%s" host port))))


(comment
  (start))
