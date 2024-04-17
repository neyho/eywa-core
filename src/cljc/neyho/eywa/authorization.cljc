(ns neyho.eywa.authorization
  #?(:clj
     (:require 
       clojure.set
       [clojure.zip :as zip]
       [neyho.eywa.authorization.components  :as components
        :refer [eywa
                path->position]])
     :cljs
     (:require
       clojure.set
       [neyho.eywa.authorization.components  :as components
        :refer [path->position
                eywa]]
       [neyho.eywa.context :refer [signal-channel http-url]]
       [cljs.core.async :as async]
       [clojure.zip :as zip]
       [cognitect.transit :as transit])))

(defonce role-permissions (atom nil))

(defn component-accessible? [tree path]
  (boolean (path->position tree path)))

(comment
  ()
  (def datasets (:datasets user/system))
  (require '[neyho.eywa.db.rocksdb :as rocks])
  (require '[babashka.fs :as fs])
  (def db (rocks/create-db directory))
  (pr-str @role-permissions)
  (rocks/put db "role-schema" @role-permissions)
  (rocks/put db "role-schema"  "HOMO")
  (rocks/close db)
  (rocks/size db)
  (rocks/close db)
  (.getProperty (:db db) "rocksdb.estimate-live-data-size")
  (time (rocks/get db "role-schema"))
  )

#?(:cljs 
   (defn load-role-schema []
     (let [result (async/chan)
           xhr (new js/XMLHttpRequest)
           url (str http-url "/eywa/access")]
       (.open xhr "GET" url)
       (.setRequestHeader xhr "Content-Type" "application/transit+json")
       (.setRequestHeader xhr "Accept" "application/transit+json")
       ;; Maybe enable
       ; (when *token*
       ;   (.setRequestHeader xhr "Authorization" (str "Bearer " *token*)))
       (.addEventListener
         xhr "load"
         (fn [evt]
           (case (.. evt -currentTarget -status)
             403 (async/put! 
                   signal-channel
                   {:topic :eywa/authorization
                    :authorized? false})
             (let [response (transit/read 
                              (transit/reader :json)
                              (.. evt -currentTarget -responseText))]
               (async/put! result response)
               (reset! role-permissions response)))))
       (.send xhr)
       result)))

(defn get-role-schema 
  ([]
   (if-let [schema @role-permissions]
     schema
     @role-permissions))
  ([roles]
   (let [schema (get-role-schema)
         components (components/tree->components schema)
         components' (keep
                       (fn [component]
                         (let [component' (update component :roles clojure.set/intersection (set roles))]
                           (when (not-empty (:roles component'))
                             component')))
                       components)]
     (components/components->tree components' {:euuid eywa}))))

(comment
  (def component components/robotics-pools)
  (def component components/robotics-robots)
  (let [position (components/component->position (get-role-schema) component)]
    (count (rest (zip/path position))))
  (def roles #{#uuid "1abdb2a2-781d-11eb-b410-0299cb9ddfa4"}))

(defn authorize-roles [roles target]
  (try
    (let [position (components/component->position (get-role-schema) target)
          component (zip/node position)
          parents (rest (zip/path position))
          authorization-path (map :roles (conj parents component))
          roles (set roles)]
      ; (println "ROLES: " roles)
      ; (println "AP: " authorization-path)
      ; (println 
      ;   "AUTHORIZED: "
      ;   (every? 
      ;     #(not-empty (clojure.set/intersection roles %))
      ;     authorization-path))
      (every? 
        #(not-empty (clojure.set/intersection roles %))
        authorization-path))
    (catch #?(:clj Throwable :cljs js/Error) _ false)))
