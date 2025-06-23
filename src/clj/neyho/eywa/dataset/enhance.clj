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

(defmulti enhance-schema-for-access
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
    ([db schema entity-id]
     [(class db) entity-id]))
  :default ::default)

(defmethod enhance-schema-for-access ::default
  [_ schema entity-id]
  ;; By default, no enhancement - rely on standard entity-level access control
  schema)

;; ============================================================================
;; Helper Functions
;; ============================================================================

(defn add-relation-condition
  "Adds or merges a condition to a relation's args.
   
   If the relation doesn't exist, creates it with the condition.
   If it exists, merges the condition with existing args."
  [schema relation-key condition]
  (update-in schema [:relations relation-key :args]
             (fn [existing-args]
               (if existing-args
                 ;; Merge with existing conditions
                 {:_and [existing-args condition]}
                 ;; Set as new condition
                 condition))))

(defn force-relation-join
  "Marks a relation as 'pinned' to force it to be included in the query.
   
   This ensures the relation will be JOINed even if not explicitly requested
   in the original query."
  [schema relation-key]
  (assoc-in schema [:relations relation-key :pinned] true))

(defn user-access-condition
  "Creates a condition that checks if a user has access via direct assignment
   or group membership.
   
   Args:
     user-relation  - The relation name for direct user access (e.g., :read_users)
     group-relation - The relation name for group access (e.g., :read_groups)
   
   Returns:
     A condition map suitable for use in GraphQL-style queries"
  ([user-relation group-relation]
   (user-access-condition user-relation group-relation *user*))
  ([user-relation group-relation user-id]
   {:_or [{user-relation {:euuid {:_eq user-id}}}
          {group-relation {:users {:euuid {:_eq user-id}}}}]}))

(defn add-access-via-relation
  "Adds access control to a schema via a related entity.
   
   This is the primary mechanism for implementing inherited permissions,
   such as file access through folder permissions.
   
   Args:
     schema         - The query schema
     relation-key   - The relation to use for access control
     user-relation  - The relation on the target entity for user access
     group-relation - The relation on the target entity for group access
   
   Returns:
     Enhanced schema with access control via the specified relation"
  ([schema relation-key user-relation group-relation]
   (add-access-via-relation schema relation-key user-relation group-relation *user*))
  ([schema relation-key user-relation group-relation user-id]
   (-> schema
       (force-relation-join relation-key)
       (add-relation-condition relation-key
                               (user-access-condition user-relation
                                                      group-relation
                                                      user-id)))))

;; ============================================================================
;; Entity-Specific Enhancements
;; ============================================================================

;; Example: File entity with folder-based access control
;; (defmethod enhance-schema-for-access #uuid "your-file-entity-uuid"
;;   [schema entity-id]
;;   (cond
;;     ;; Superusers bypass folder-based access control
;;     (access/superuser? *user*)
;;     schema
;;     
;;     ;; Admin role might have different access pattern
;;     (contains? *roles* #uuid "admin-role-uuid")
;;     (add-access-via-relation schema :folder :admin_folders nil)
;;     
;;     ;; Regular users get folder-based access control
;;     :else
;;     (add-access-via-relation schema 
;;                              :folder 
;;                              :read_users 
;;                              :read_groups)))

;; ============================================================================
;; Advanced Enhancement Patterns
;; ============================================================================

(defn add-hierarchical-access
  "Adds access control that checks multiple levels of a hierarchy.
   
   Useful for nested folder structures where access can be inherited
   from parent folders.
   
   Args:
     schema        - The query schema
     relation-path - Vector of relations to traverse (e.g., [:folder :parent])
     max-depth     - Maximum depth to check"
  [schema relation-path max-depth]
  (let [conditions (for [depth (range 1 (inc max-depth))]
                     (let [path (take depth (cycle relation-path))]
                       (assoc-in {} path {:read_users {:euuid {:_eq *user*}}})))]
    (-> schema
        (force-relation-join (first relation-path))
        (add-relation-condition (first relation-path)
                                {:_or conditions}))))

(defn add-temporal-access
  "Adds time-based access control conditions.
   
   Args:
     schema      - The query schema
     relation    - The relation to check
     time-field  - The field containing the time constraint
     comparison  - The comparison operator (:_gt, :_lt, etc.)
     time-value  - The time value to compare against"
  [schema relation time-field comparison time-value]
  (-> schema
      (force-relation-join relation)
      (add-relation-condition relation
                              {time-field {comparison time-value}})))

(defn add-attribute-based-access
  "Adds access control based on entity attributes.
   
   Useful for classification-based access, department-based access, etc.
   
   Args:
     schema     - The query schema  
     relation   - The relation to check
     attribute  - The attribute to check
     values     - Set of allowed values"
  [schema relation attribute values]
  (-> schema
      (force-relation-join relation)
      (add-relation-condition relation
                              {attribute {:_in values}})))

(defn add-role-based-conditions
  "Adds conditions based on user's roles.
   
   Args:
     schema          - The query schema
     role-conditions - Map of role UUID to condition function"
  [schema role-conditions]
  (reduce-kv
   (fn [schema role-id condition-fn]
     (if (contains? *roles* role-id)
       (condition-fn schema)
       schema))
   schema
   role-conditions))

;; ============================================================================
;; Debugging and Analysis
;; ============================================================================

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

(defn enhance-schema
  "Main entry point for schema enhancement.
   
   Called by the query system to potentially add access control conditions
   to a schema before query execution.
   
   Args:
     schema    - The query schema to enhance
     entity-id - UUID of the entity being queried
   
   Returns:
     Enhanced schema (or original if no enhancement needed)"
  [schema entity-id]
  (if (and *user*)
    (let [enhanced (enhance-schema-for-access neyho.eywa.db/*db* schema entity-id)]
      (log-enhancement schema entity-id enhanced)
      enhanced)
    schema))
