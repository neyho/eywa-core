(ns neyho.eywa.dataset.enhance
  "Schema enhancement for access control.
  
  Provides a mechanism to inject access control conditions into query schemas
  before they are executed. This allows for transparent enforcement of complex
  access patterns like folder-based file permissions."
  (:require
   [clojure.tools.logging :as log]
   [neyho.eywa.db :refer [*db*]]
   [neyho.eywa.iam.access :as access]
   [neyho.eywa.iam.access.context :refer [*user* *roles* *groups*]]
   [neyho.eywa.dataset :as dataset]))

;; ============================================================================
;; Core Enhancement Protocol
;; ============================================================================

(defmulti schema
  "Enhances a query schema with access control conditions.
   
   Dispatches on entity UUID to allow entity-specific access patterns.
   Uses dynamic bindings *user*, *roles*, and *groups* from context.
   
   Args:
     db          - database class
     schema      - The query schema to enhance
     entity-id   - UUID of the entity being queried
   
   Returns:
     Enhanced schema with additional access control conditions"
  (fn dispatch
    ([db _schema selection]
     [(class db) (:entity _schema)]))
  :default ::default)

(defmethod schema ::default
  [_ _schema _]
  ;; By default, no enhancement - rely on standard entity-level access control
  _schema)

(defmulti args
  "Enhances query arguments with access control conditions at any schema depth.
   Called during search-stack-args processing.
   
   Args:
     db        - Database instance
     schema    - Current schema node being processed
     entity-id - Entity UUID of the current schema node
     path      - Vector of relation keys from root to current node
   
   Returns:
     Enhanced schema with injected args"
  (fn [db {entity-id :entity :as schema} [stack data]]
    [(class db) entity-id])
  :default ::default)

(comment
  (do
    (ns-unmap 'neyho.eywa.dataset.enhance 'enhance-schema-for-access)
    (ns-unalias 'neyho.eywa.dataset.enhance 'enhance-schema-for-access)
    (ns-unmap 'neyho.eywa.dataset.enhance 'enhance-args-for-access)
    (ns-unalias 'neyho.eywa.dataset.enhance 'enhance-schema-for-access)))

(defmethod args ::default
  [_ schema current-stack]
  current-stack)

(defn explain-enhancement
  "Returns a human-readable explanation of what enhancements were applied.
   
   Useful for debugging and audit trails."
  [original-schema enhanced-schema entity-id]
  (let [original-relations (set (keys (:relations original-schema)))
        enhanced-relations (set (keys (:relations enhanced-schema)))
        added-relations (clojure.set/difference enhanced-relations original-relations)
        modified-relations (filter #(not= (get-in original-schema [:relations % :args])
                                          (get-in enhanced-schema [:relations % :args]))
                                   original-relations)]
    {:entity entity-id
     :user *user*
     :roles *roles*
     :groups *groups*
     :added-relations added-relations
     :modified-relations modified-relations
     :access-conditions (into {}
                              (for [rel (concat added-relations modified-relations)]
                                [rel (get-in enhanced-schema [:relations rel :args])]))}))

(defn log-enhancement
  "Logs enhancement details for monitoring and debugging."
  [schema entity-id enhanced-schema]
  (when (not= schema enhanced-schema)
    (log/debugf "Access enhancement applied for entity %s, user %s:\n%s"
                entity-id
                *user*
                (pr-str (explain-enhancement schema enhanced-schema entity-id)))))

;; ============================================================================
;; Integration Function
;; ============================================================================

(defn apply-schema
  "Main entry point for schema enhancement.
   
   Called by the query system to potentially add access control conditions
   to a schema before query execution.
   
   Args:
     schema    - The query schema to enhance
     entity-id - UUID of the entity being queried
   
   Returns:
     Enhanced schema (or original if no enhancement needed)"
  [_schema selection]
  (if (and *user*)
    (let [enhanced (schema neyho.eywa.db/*db* _schema selection)]
      (log-enhancement _schema (:entity _schema) enhanced)
      enhanced)
    _schema))

(defn ensure-path
  "Creates a nested structure and optionally sets a value at the end.
   
   Examples:
   (ensure-path {} [:a :b :c] :leaf-value)
   => {:a {:b {:c :leaf-value}}}
   
   (ensure-path {} [:folder 0 :selections :read_users 0] {:selections {:euuid nil}})
   => {:folder [{:selections {:read_users [{:selections {:euuid nil}}]}}]}"
  ([m path]
   (ensure-path m path {}))
  ([m path leaf-value]
   (if (empty? path)
     leaf-value
     (let [[k & ks] path
           next-k (first ks)]
       (cond
         ;; Last key in path - set the value
         (empty? ks)
         (assoc m k leaf-value)

         ;; Current key is numeric - ensure we have a vector
         (number? k)
         (let [v (if (vector? m) m [])
               v' (if (< k (count v))
                    v
                    (into v (repeat (inc (- k (count v))) nil)))
               existing (get v' k)
               new-val (ensure-path existing ks leaf-value)]
           (assoc v' k new-val))

         ;; Next key is numeric - current value should be a vector
         (number? next-k)
         (assoc m k (ensure-path (get m k []) ks leaf-value))

         ;; Regular map navigation
         :else
         (assoc m k (ensure-path (get m k {}) ks leaf-value)))))))

(defmulti write
  "Checks if the current user can perform the mutation.
   
   Args:
     db         - Database instance
     entity-id  - Entity UUID being mutated
     data       - The mutation data
     tx         - Current transaction
   
   Returns:
     returns updated data ready for storage"
  (fn [db entity-id data tx]
    [(class db) entity-id])
  :default ::default)

(comment
  (do
    (methods write)
    (ns-unmap 'neyho.eywa.dataset.enhance 'write)
    (ns-unalias 'neyho.eywa.dataset.enhance 'write)))

(defmethod write ::default
  [_ _ data _]
  ;; Default allows all mutations (relies on entity-level access)
  data)

(defn apply-write
  ([entity-id data tx] (apply-write *db* entity-id data tx))
  ([db entity-id data tx]
   (write db entity-id data tx)))

(defmulti delete
  "Checks if the current user can perform the mutation.
   
   Args:
     db         - Database instance
     entity-id  - Entity UUID being mutated
     data       - The mutation data
     tx         - Current transaction
   
   Returns:
     returns updated data ready for storage"
  (fn [db entity-id data tx]
    [(class db) entity-id])
  :default ::default)

(defmethod delete ::default
  [_ _ data _]
  ;; Default allows all mutations (relies on entity-level access)
  data)

(defn apply-delete
  ([entity-id data tx] (apply-delete *db* entity-id data tx))
  ([db entity-id data tx]
   (delete db entity-id data tx)))

(defmulti purge
  "Checks if the current user can perform the mutation.
   
   Args:
     db         - Database instance
     entity-id  - Entity UUID being mutated
     data       - The mutation data
     tx         - Current transaction
   
   Returns:
     returns updated data ready for storage"
  (fn [db entity-id data tx]
    [(class db) entity-id])
  :default ::default)

(defmethod purge ::default
  [_ _ data _]
  ;; Default allows all mutations (relies on entity-level access)
  data)

(defn apply-purge
  ([entity-id data tx] (apply-purge *db* entity-id data tx))
  ([db entity-id data tx]
   (purge db entity-id data tx)))
