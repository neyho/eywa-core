(ns neyho.eywa.iam.access
  (:require
    [clojure.set :as set]
    [clojure.core.async :as async]
    [clojure.tools.logging :as log]
    [neyho.eywa.data :refer [*ROOT* *EYWA*]]
    [neyho.eywa.dataset :as dataset]
    [neyho.eywa.dataset.core :as core]
    [neyho.eywa.iam.uuids :as iu]
    [neyho.eywa.iam.access.context :refer [*rules* *roles* *user*]]))


(defn get-roles-access-data
  []
  (dataset/search-entity
    iu/user-role
    nil
    {:euuid nil
     :name nil
     ;; Entities
     :write_entities [{:selections {:euuid nil}}]
     :read_entities [{:selections {:euuid nil}}]
     :delete_entities [{:selections {:euuid nil}}]
     :owned_entities [{:selections {:euuid nil}}]
     ;; Relations
     :to_read_relations [{:selections {:euuid nil}}]
     :to_write_relations [{:selections {:euuid nil}}]
     :to_delete_relations [{:selections {:euuid nil}}]
     :from_read_relations [{:selections {:euuid nil}}]
     :from_write_relations [{:selections {:euuid nil}}]
     :from_delete_relations [{:selections {:euuid nil}}]}))


(defn transform-roles-data
  [data]
  (letfn [(x-entity [result role rule entities]
            (reduce
              (fn [r entity]
                (update-in r [:entity entity rule] (fnil conj #{}) role))
              result
              entities))
          (x-relation [result role direction rule relations]
            (reduce
              (fn [r relation]
                (update-in r [:relation relation direction rule] (fnil conj #{}) role))
              result
              relations))]
    (reduce
      (fn [r {:keys [euuid
                     write_entities read_entities delete_entities owned_entities
                     to_read_relations to_write_relations to_delete_relations
                     from_read_relations from_write_relations from_delete_relations]}]
        (-> r
            (x-entity euuid :read (map :euuid read_entities))
            (x-entity euuid :write (map :euuid write_entities))
            (x-entity euuid :delete (map :euuid delete_entities))
            (x-entity euuid :owners (map :euuid owned_entities))
            (x-relation euuid :from :read (map :euuid from_read_relations))
            (x-relation euuid :from :write (map :euuid from_write_relations))
            (x-relation euuid :from :delete (map :euuid from_delete_relations))
            (x-relation euuid :to :read (map :euuid to_read_relations))
            (x-relation euuid :to :write (map :euuid to_write_relations))
            (x-relation euuid :to :delete (map :euuid to_delete_relations))))
      nil
      data)))



(defn load-rules
  []
  (alter-var-root #'*rules* (fn [_] (transform-roles-data (get-roles-access-data)))))


(defn superuser?
  ([] (superuser? *roles*))
  ([roles]
   (or
     (nil? *user*)
     (= *user* (:_eid *EYWA*))
     (contains? roles (:euuid *ROOT*)))))


(defn entity-allows?
  ([entity rules] (entity-allows? entity rules *roles*))
  ([entity rules roles]
   (try
     (if (or (nil? *rules*) (superuser? roles)) true
       (letfn [(ok? [rule]
                 ; (println "Checking roles: " rule entity roles (get-in *rules* [:entity entity rule]))
                 ; (println "Result: " (set/intersection roles (get-in *rules* [:entity entity rule])))
                 (boolean (not-empty (set/intersection roles (get-in *rules* [:entity entity rule])))))]
         (some ok? rules)))
     (catch Throwable ex
       (log/error "[IAM] Couldn't evaluate entity-allows for roles %s" roles)
       (throw ex)))))



(defn relation-allows?
  ([relation direction rules] (relation-allows? relation direction rules *roles*))
  ([relation direction rules roles]
   (try
     (if (or (nil? *rules*) (superuser? roles)) true
       (letfn [(ok? [rule]
                 (boolean (not-empty (set/intersection roles (get-in *rules* [:relation relation direction rule])))))]
         ; (def relation relation)
         ; (def rules rules)
         ; (def roles roles)
         ; (def direction direction)
         ; (def rule (first rules))
         #_(def roles #{#uuid "97b95ab8-4ca3-498d-b578-b12e6d1a2df8"})
         (some ok? rules)))
     (catch Throwable ex
       (log/error "[IAM] Couldn't evaluate relation-allows for roles %s" roles)
       (throw ex)))))



(defn start-enforcing
  []
  (let [model (dataset/deployed-model)
        role-entity (core/get-entity model iu/user-role)
        relations (core/focus-entity-relations model role-entity)
        relation-euuids (set (map :euuid relations))
        all-euuids (->
                     relation-euuids
                     ;; Disj permissions and users
                     (disj #uuid "16ca53f4-0fe3-4122-93dd-1e86fd1b58db"
                           #uuid "1a2cc45d-1301-4fdd-bb02-650362165b37")
                     (conj iu/user-role))
        delta-chan (async/chan (async/sliding-buffer 1))]
    (doseq [element all-euuids]
      (log/infof "[IAM] Subscribing to dataset delta channel for: %s" element)
      (async/sub core/delta-publisher element delta-chan))
    ;; Start idle service that will listen on delta changes
    (async/go-loop
      [_ (async/<! delta-chan)]
      (log/debugf "[IAM] Received something at delta channel")
      ;; When first delta change is received start inner loop
      (loop [[idle-value] (async/alts!
                            [;; That will check for new delta values
                             delta-chan
                             ;; Or timeout
                             (async/go
                               (async/<! (async/timeout 5000))
                               ::TIMEOUT)])]
        (log/debugf "[IAM] Next idle value is: %s" idle-value)
        ;; IF timeout is received than reload rules
        (if (= ::TIMEOUT idle-value)
          (do
            (log/info "[IAM] Reloading role access!")
            (load-rules))
          ;; Otherwise some other delta has been received and
          ;; inner loop will be repeated
          (recur (async/alts!
                   [;; That will check for new delta values
                    delta-chan
                    ;; Or timeout
                    (async/go
                      (async/<! (async/timeout 5000))
                      ::TIMEOUT)]))))
      ;; when reloading is complete, wait for new delta value
      ;; and repeat process
      (recur (async/<! delta-chan)))
    (load-rules)))



(comment
  (start-enforcing))
