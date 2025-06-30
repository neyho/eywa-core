(ns neyho.eywa.dataset.graphql
  (:require
   [clojure.string :as str]
   [clojure.core.async :as async]
   [clojure.tools.logging :as log]
   [neyho.eywa.dataset
    :refer [publisher
            deployed-model]]
   [neyho.eywa.dataset.sql.naming :refer [normalize-name]]
   [neyho.eywa.db :refer [*db*]]
   [neyho.eywa.dataset.core :as dataset]
   [neyho.eywa.iam.access :as access]))

(defn- protect-dataset
  [model]
  (as-> model m
    (reduce
     (fn [m entity]
       (if (access/entity-allows? (:euuid entity) #{:read :write})
         m
         (dataset/remove-entity m entity)))
     m
     (dataset/get-entities m))
    (reduce
     (fn [m {{from :euuid} :from
             {to :euuid} :to
             :as relation}]
       (if (or
            (access/relation-allows?
             (:euuid relation)
             [from to]
             #{:read :write})
            (access/relation-allows?
             (:euuid relation)
             [to from]
             #{:read :write}))
         m
         (dataset/remove-relation m relation)))
     m
     (dataset/get-relations m))))

(defn get-deployed-model [_ _ _]
  (protect-dataset (deployed-model)))

(defn on-deploy
  [{:keys [username]} _ upstream]
  (let [sub (async/chan)]
    (async/sub publisher :refreshedGlobalDataset sub)
    (async/go-loop [{:keys [data]
                     :as published} (async/<! sub)]
      (when published
        (log/tracef "Sending update of global model to user %s" username)
        (upstream data)
        (recur (async/<! sub))))
    (let [model (dataset/get-model *db*)
          protected-model (protect-dataset model)]
      (upstream
       {:name "Global"
        :model protected-model}))
    (fn []
      (async/unsub publisher :refreshedGlobalDataset sub)
      (async/close! sub))))

(def dataset-txt-prelude
  "# EYWA Dataset Schema

This document describes the EYWA data model and how to interact with it through GraphQL operations.

## Schema Notation Guide

### Attribute Constraints
- `#` = **Unique** - Field must be unique across all records
- `*` = **Mandatory** - Field is required
- `o` = **Optional** - Field can be null

### Attribute Types
- Basic: `string`, `int`, `float`, `boolean`, `timestamp`
- Complex: `json`, `transit`, `uuid`, `avatar`
- Security: `hashed`, `encrypted`
- Special: `user`, `group`, `role`, `timeperiod`, `currency`
- Enum: `enum{VALUE1,VALUE2,...}` - Enumerated values

### Relationship Format
```
[Source Entity]---[relation name][cardinality]--->[Target Entity]
```

### Cardinality Types
- `o2o` = One-to-One
- `o2m` = One-to-Many
- `m2o` = Many-to-One
- `m2m` = Many-to-Many
- `tree` = Hierarchical/Self-referencing

## GraphQL Operation Patterns

Each entity automatically generates these operations:

### Queries
- `get{Entity}(euuid: \" ... \")` - Get single record by ID
- `search{Entity}(_where: {...}, _limit: 10, _offset: 0, _order_by: {...})` - Search with filters
- `search{Entity}TreeBy{Field}` - Hierarchical queries for tree relationships

### Mutations
- `sync{Entity}(data: {...})` - Upsert SINGLE entity (create or update)
- `sync{Entity}List(data: [{...}, {...}])` - Upsert MULTIPLE entities
- `stack{Entity}(data: {...})` - Create SINGLE new entity
- `stack{Entity}List(data: [{...}, {...}])` - Create MULTIPLE new entities
- `slice{Entity}(euuid: \" ... \", data: {...})` - Partial update
- `delete{Entity}(euuid: \" ... \")` - Soft delete (returns boolean)
- `purge{Entity}(euuid: \" ... \")` - Hard delete (DANGEROUS - permanent)

## Quick Examples

### From Entity to Query
```
Entity: User
Attributes:
#  Name[string]
o  Active[boolean]
```

Generates:
```graphql
query {
  searchUser(_where: {active: {_eq: true}}, _limit: 10) {
    euuid
    name
    active
  }
}
```

### From Relationship to Nested Query
```
User---roles[m2m]--->User Role
```

Enables:
```graphql
query {
  getUser(euuid: \" ... \") {
    name
    roles {  # Follow the relationship
      euuid
      name
      active
    }
  }
}
```

### From Attributes to Mutation
```graphql
mutation {
  syncUser(data: {
    name: \"john_doe\",      # Unique field
    active: true           # Optional field
  }) {
    euuid
    name
  }
}
```

## Important Notes

1. **Field Names**: GraphQL uses snake_case (e.g., `name`, `active`) even if shown with capitals in schema
2. **Enum Values**: Use unquoted values (e.g., `type: ACCESS` not `type: \"ACCESS\"`)
3. **Array Operations**: Always use `List` suffix for multiple entities:
   - `syncUserList` for updating multiple users
   - `stackProjectList` for creating multiple projects
4. **Unique Fields**: Can be used as identifiers in sync operations instead of euuid
")

(defn get-dataset-txt
  ([] (get-dataset-txt (protect-dataset (deployed-model))))
  ([model]
   (let [entities (dataset/get-entities model)
         txt-entities (reduce
                       (fn [result {:keys [attributes] entity-name :name :as entity}]
                         (let [relations (dataset/focus-entity-relations model entity)]
                           (conj result
                                 (str "Entity: " entity-name \newline
                                      "Attributes:" \newline
                                      (str/join "\n" (map (fn [{:keys [constraint name type configuration]}]
                                                            (str
                                                             (case constraint
                                                               "optional" "o"
                                                               "mandatory" "*"
                                                               "unique" "#"
                                                               "o")
                                                             "\t"
                                                             name
                                                             \[
                                                             type
                                                             \]
                                                             (when (= type "enum")
                                                               (str
                                                                \{
                                                                (str/join ","
                                                                          (keep
                                                                           (fn [{:keys [active name]}]
                                                                             (when active name))
                                                                           (:values configuration)))
                                                                \}))))
                                                          attributes))
                                      (when (not-empty relations)
                                        (str
                                         "\nRelations:\n"
                                         (str/join
                                          "\n"
                                          (keep
                                           (fn [{:keys [from to-label to cardinality]}]
                                             (when to-label
                                               (str
                                                (:name from)
                                                "---" (normalize-name to-label) \[ cardinality \] "--->"
                                                (:name to))))
                                           relations))))))))
                       []
                       entities)]
     (str dataset-txt-prelude "\n\n" (str/join "\n----\n" txt-entities)))))

(defn get-deployed-model-document [_ _ _]
  (get-dataset-txt))

(comment
  (def model (deployed-model))
  (def entity (dataset/get-entity model #uuid "63b2e70a-2162-423a-be36-4909d7831605"))
  (def relations (dataset/focus-entity-relations model entity))
  (println (get-dataset-txt))
  (spit "cli/dataset_schema.md" (get-dataset-txt))
  (spit "/tmp/eywa_ai_robotics/dataset_schema.md" (get-dataset-txt)))
