(ns neyho.eywa.lacinia
  (:require
    [clojure.walk :as walk]
    [clojure.string :as str]
    [clojure.tools.logging :as log]
    [clojure.instant :refer [read-instant-date]]
    [neyho.eywa.transit
     :refer [<-transit ->transit]]
    [com.walmartlabs.lacinia.selection :as selection]
    [com.walmartlabs.lacinia.resolve :as r]
    [com.walmartlabs.lacinia.schema :as schema]
    [com.walmartlabs.lacinia.parser.schema :refer [parse-schema]]))


(defn deep-merge
  "Recursively merges maps."
  [& maps]
  (letfn [(m [& xs]
            (if (some #(and (map? %) (not (record? %))) xs)
              (apply merge-with m xs)
              (last xs)))]
    (reduce m maps)))


(defonce compiled (ref nil))
(defonce state (ref nil))


(defprotocol EYWAGraphQL
  (generate-lacinia-schema
    [this]
    "Returns lacinia uncompiled schema"))


(def default-subscription-resolver
  ^r/ResolverResult (fn [_ _ v] (r/resolve-as v)))


(letfn [(parse-uuid [uuid]
          (cond
            (uuid? uuid) uuid
            (string? uuid) (java.util.UUID/fromString uuid)))
        (parse-date [date]
          (if (instance? java.util.Date date) date
            (try
              (read-instant-date date)
              (catch Exception _ nil))))]
  (def scalars
    {:null
     {:parse (constantly nil)
      :serialize (constantly nil)}
     :Timestamp
     {:description "Casts Date for internal usage"
      :parse parse-date
      :serialize identity}
     :UUID
     {:description "UUID"
      :parse parse-uuid
      :serialize identity}
     :JSON
     {:parse walk/stringify-keys
      :serialize identity}
     :Hash
     {:parse identity
      :serialize (constantly "")}
     :Transit
     {:parse <-transit
      :serialize ->transit}
     :Encrypted
     {:parse identity
      :serialize identity}}))


(defn bind-subscription-resolvers
  [schema]
  (letfn [(resolver [{:keys [directives] :as v}]
            (if-let [{resolver-fn :fn}
                     (some
                       (fn [{:keys [directive-type directive-args]}]
                         (when (= directive-type :resolve)
                           directive-args))
                       directives)]
              (-> v
                  (assoc :stream (resolve (symbol resolver-fn))
                         :resolve default-subscription-resolver)
                  (update :directives
                          (fn [directives]
                            (vec
                              (remove
                                #(= :resolve (:directive-type %))
                                directives)))))
              v))
          (resolve-fields
            [mapping]
            (reduce-kv
              (fn [r k {:keys [directives] :as v}]
                (if (not-empty directives)
                  (assoc r k (resolver v))
                  r))
              mapping
              mapping))]
    (cond->
      schema 
      (some? (get-in schema [:objects :Subscription :fields]))
      (update-in [:objects :Subscription :fields] resolve-fields))))


(defn bind-resolvers
  [schema]
  (letfn [(resolver [{:keys [directives] :as v}]
            (if-let [{resolver-fn :fn}
                     (some
                       (fn [{:keys [directive-type directive-args]}]
                         (when (= directive-type :resolve)
                           directive-args))
                       directives)]
              (-> v
                  (assoc :resolve (resolve (symbol resolver-fn)))
                  (update :directives
                          (fn [directives]
                            (vec
                              (remove
                                #(= :resolve (:directive-type %))
                                directives)))))
              v))
          ;;
          (resolve-fields
            [mapping]
            (reduce-kv
              (fn [r k {:keys [directives] :as v}]
                (if (not-empty directives)
                  (assoc r k (resolver v))
                  r))
              mapping
              mapping))
          ;;
          (bind-resolvers [schema k]
            (update schema k
                    (fn [objects]
                      (reduce-kv
                        (fn [os k v]
                          (assoc os k (update v :fields resolve-fields)))
                        objects
                        objects))))]
    (->
      schema
      bind-subscription-resolvers
      (bind-resolvers :objects)
      (bind-resolvers :input-objects))))


(defn ^:private recompile []
  (let [{:keys [shards directives]} @state]
    (when (not-empty shards)
      (log/infof
        "Recompiling shards: [%s]"
        (str/join ", " (keys shards)))
      (when-some [schema (reduce
                           deep-merge
                           (map
                             (fn [s] (if (fn? s) (s) s))
                             (vals shards)))]
        #_(clojure.pprint/pprint
          (->
            schema
            bind-resolvers))
        (schema/compile
          ; schema
          (->
            schema
            bind-resolvers)
          {:apply-field-directives
           (fn [field resolver-fn]
             (log/infof
               "Applying field directive to field %s"
               (pr-str (selection/field-name field)))
             (let [field-directives (selection/directives field)
                   matching-directives (filter
                                         #(contains? directives %)
                                         (keys field-directives))]
               ;; When there are matching directives
               (if (not-empty matching-directives)
                 ;; Than for each of directives
                 (reduce
                   (fn [v k]
                     ;; Try to get transform
                     (if-let [xf ((get directives k) (get field-directives k) v)]
                       ;; And if successfull than wrap resolver with that transformation
                       ;; Transformation should be function of 4 keys
                       ;; context args original-value resolved-value
                       xf 
                       v))
                   resolver-fn
                   (sort-by :metric matching-directives))
                 resolver-fn)))})))))


(comment
  (dosync (ref-set compiled (recompile))))


(defn remove-directive [key]
  (dosync
    (alter state update :directives dissoc key)
    (ref-set compiled (recompile))))

(defn add-directive [key shard]
  (dosync
    (alter state assoc-in [:directives key] shard)
    (ref-set compiled (recompile))))

(defn directives [] (keys (:directives @state)))
(defn directive [key] (get-in @state [:directives key]))

(defn remove-shard [key]
  (dosync
    (alter state update :shards clojure.core/dissoc key)
    (ref-set compiled (recompile))))

(comment
  (def _shard (slurp (clojure.java.io/resource "tasks.graphql")))
  (-> state deref :directives keys)
  (parse-schema _shard)
  (add-shard :neyho.eywa.tasks/tasks _shard))

(defn add-shard [key shard]
  (log/infof "Adding Lacinia shard %s" key)
  (let [shard (cond
                (string? shard) (parse-schema shard)
                (fn? shard) (shard)
                :else shard)]
    (dosync
      (alter state assoc-in [:shards key] shard)
      (ref-set compiled (recompile)))))

(defn shards [] (keys (:shards @state)))

(defn shard [key] (get-in @state [:directives key]))
