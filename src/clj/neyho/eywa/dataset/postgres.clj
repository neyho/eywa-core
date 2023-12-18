(ns neyho.eywa.dataset.postgres
  (:require
    clojure.set
    clojure.data
    clojure.string
    [clojure.data.csv :as csv]
    [clojure.java.io :as io]
    ; [clojure.pprint :refer [pprint]]
    next.jdbc.date-time
    [next.jdbc :as jdbc]
    [clojure.tools.logging :as log]
    [camel-snake-kebab.core :as csk]
    [neyho.eywa.dataset.core :as core]
    [neyho.eywa.dataset :as dataset]
    [neyho.eywa.transit
     :refer [<-transit ->transit]]
    [neyho.eywa.db.postgres.next  :as n
     :refer [execute! execute-one!]]
    [neyho.eywa.db.postgres :as postgres]
    [neyho.eywa.dataset.postgres.naming
     :refer [normalize-name 
             column-name
             relation->table-name
             entity->relation-field
             entity->table-name]]
    [neyho.eywa.db
     :refer [*db*
             sync-entity
             stack-entity
             slice-entity
             get-entity
             get-entity-tree
             purge-entity
             search-entity-tree
             search-entity
             aggregate-entity
             aggregate-entity-tree
             delete-entity
             ; count-entity
             ; verify-encrypted
             delete-entity]]
    [neyho.eywa.lacinia :as lacinia]
    [neyho.eywa.data :refer [*EYWA*]]
    [neyho.eywa.administration :as administration]
    [neyho.eywa.administration.uuids :as au]
    [neyho.eywa.dataset.uuids :as du]
    [com.walmartlabs.lacinia.parser :as parser]
    ; [com.walmartlabs.lacinia.util :as util]
    [com.walmartlabs.lacinia.resolve :as lacinia.resolve]
    [com.walmartlabs.lacinia.executor :as executor]))


(def ^:dynamic *model*)
(def ^:dynamic *eywa*)


(defn user-table []
  (if-some [e (core/get-entity *model* au/user)]
    (entity->table-name e)
    (throw (Exception. "Coulnd't find user entity"))))


(defn group-table []
  (if-some [e (core/get-entity *model* au/user-group)]
    (entity->table-name e)
    (throw (Exception. "Coulnd't find group entity"))))


(defn role-table []
  (if-some [e (core/get-entity *model* au/user-role)]
    (entity->table-name e)
    (throw (Exception. "Coulnd't find role entity"))))


(defn type->ddl 
  "Converts type to DDL syntax"
  [t]
  (case t
    "currency" "jsonb"
    ("avatar" "string" "encrypted" "hashed") "text"
    "timestamp" "timestamp"
    "timeperiod" "jsonb"
    "json" "jsonb"
    "transit" "text"
    "user" (str "int references \"" (user-table) "\"(_eid) on delete set null")
    "group" (str "int references \"" (group-table) "\"(_eid) on delete set null")
    "role" (str "int references \"" (role-table) "\"(_eid) on delete set null")
    t))

(defn attribute->ddl 
  "Function converts attribute to DDL syntax"
  [entity {n :name t :type}]
  (case t 
    "enum"
    (let [table (entity->table-name entity)
          enum-name (normalize-name (str table \space n))]
      (str (column-name n) \space enum-name))
    ;;
    (clojure.string/join 
      " "
      (remove 
        empty?
        [(column-name n)
         (type->ddl t)]))))


(defn normalized-enum-value [value]
  (clojure.string/replace value #"-|\s" "_"))


(defn generate-enum-type-ddl [enum-name values]
  (format
    "do $$\nbegin\nif not exists ( select 1 from pg_type where typname='%s') then create type \"%s\" as enum%s;\nend if;\nend\n$$;"
    enum-name
    enum-name 
    (when (not-empty values)
      (str " (" 
           (clojure.string/join 
             ", "
             (map (comp #(str \' % \') normalized-enum-value :name) values))
           \)))))

(defn generate-entity-attribute-enum-ddl
  [table {n :name t :type {values :values} :configuration}]
  (let [enum-name (normalize-name (str table \space n))] 
    (when (= t "enum")
      (generate-enum-type-ddl enum-name values))))


(defn generate-entity-enums-ddl
  [{as :attributes
    :as entity}]
  (let [table (entity->table-name entity)] 
    (clojure.string/join 
      ";"
      (keep (partial generate-entity-attribute-enum-ddl table) as))))


(defn drop-entity-enums-ddl
  [{as :attributes
    :as entity}]
  (let [table (entity->table-name entity)] 
    (clojure.string/join 
      ";"
      (keep 
       (fn [{n :name t :type}]
         (when (= t "enum") 
           (str "drop type if exists " (normalize-name (str table \space n)))))
       as))))


(defn generate-entity-ddl
  "For given model and entity returns entity table DDL"
  [{n :name 
    as :attributes 
    {cs :constraints} :configuration
    :as entity}]
  (let [table (entity->table-name entity)
        as' (keep #(attribute->ddl entity %) as)
        pk ["_eid bigserial not null primary key"
            "euuid uuid not null unique default uuid_generate_v1()"]
        cs' (keep-indexed
              (fn [idx ids]
                (when (not-empty ids) 
                  (format 
                    "constraint \"%s\" unique(%s)" 
                    (str table "_eucg_" idx)
                    (clojure.string/join
                      ","
                      (map 
                        (fn [id]
                          (log/infof "Generating constraint %s %s" n id)
                          (->
                            (core/get-attribute entity id)
                            :name
                            column-name))
                        ids)))))
              (:unique cs))
        rows (concat pk as' cs')] 
    (format
      "create table \"%s\" (\n  %s\n)" 
      table 
      (clojure.string/join ",\n  " rows))))


(defn generate-relation-ddl
  "Returns relation table DDL for given model and target relation"
  [_ {f :from t :to :as relation}]
  (let [table (relation->table-name relation)
        from-table (entity->table-name f) 
        to-table (entity->table-name t)
        from-field (entity->relation-field f) 
        to-field (entity->relation-field t)]
    (format
      "create table %s(\n %s\n)"
      table
      ;; Create table
      (clojure.string/join
        ",\n " 
        [(str from-field " bigint not null references \"" from-table "\"(_eid) on delete cascade")
         (str to-field " bigint not null references \"" to-table "\"(_eid) on delete cascade")
         (str "unique(" from-field "," to-field ")")]))))


(defn generate-relation-indexes-ddl
  "Returns relation table DDL for given model and target relation"
  [_ {f :from t :to :as relation}]
  (let [table (relation->table-name relation)
        from-field (entity->relation-field f) 
        to-field (entity->relation-field t)]
    [(format "create index %s_fidx on \"%s\" (%s);" table table from-field)
     (format "create index %s_tidx on \"%s\" (%s);" table table to-field)]))


;; Deployment syncronization
(defn stale-attribute? [attribute]
  (let [{:keys [diff removed?]} (core/projection-data attribute)]
    (or (:active diff) removed?)))


(defn deactivate-stale-attributes!
  [con {:keys [id attributes] :as entity}]
  (let [as (mapv :id (filter stale-attribute? attributes))]
    (jdbc/execute-one!
      con
      ["update modeling_eywa_datasets_entities_attributes set active = false where entity=? and id=ANY(?)" id (int-array as)])
    entity))


(defn analyze-projection 
  [projection]
  (let [new-entities
        (filter 
          core/added?
          (core/get-entities projection))
        ;;
        changed-enties
        (filter
          core/diff?
          (core/get-entities projection))
        ;;
        new-relations
        (filter
          (fn [relation]
            (core/added? relation)) 
          (core/get-relations projection))
        ;;
        changed-relations
        (filter
          (fn [relation]
            (core/diff? relation))
          (core/get-relations projection))
        ;;
        {nrr true
         nr false} (group-by core/recursive-relation? new-relations)
        ;;
        {crr true
         cr false} (group-by core/recursive-relation? changed-relations)
        ]
    {:new/entities new-entities
     :changed/entities changed-enties
     :new/relations nr
     :new/recursive-relations nrr
     :changed/relations cr
     :changed/recursive-relations crr}))


;;
(defn attribute-delta->ddl
  [{:keys [euuid] :as entity}
   {:keys [constraint name type]
    {:keys [values]} :configuration :as attribute}]
  ;; Don't look at primary key since that is EYWA problem
  (let [new-table (entity->table-name entity)
        oentity (core/suppress entity)
        old-table (entity->table-name oentity)
        diff (core/diff attribute)] 
    (if (core/new-attribute? attribute)
      (do
        (log/debugf "Adding attribute %s to table %s" name old-table)
        (case type
          "enum"
          ;; Create new enum type and add it to table
          [(generate-entity-attribute-enum-ddl new-table attribute)
           (format "alter table \"%s\" add column if not exists %s %s"
                   old-table (column-name name)
                   (normalize-name (str new-table \space name)))]
          ;; Add new scalar column to table
          [(cond-> (str "alter table \"" old-table "\" add column if not exists " (column-name name) " " (type->ddl type))
             (= "mandatory" constraint) (str " not null"))]))
      (when (or 
              (:name (core/diff entity)) ;; If entity name has changed check if there are some enums
              (not-empty (dissoc diff :pk))) ;; If any other change happend follow steps
        (let [{dn :name
               dc :constraint
               dt :type
               dconfig :configuration} diff
              column (column-name (or dn name))
              old-enum-name (normalize-name (str old-table \space (or dn name)))
              new-enum-name (normalize-name (str new-table \space name))]
          (cond-> []
            ;; If current constraint is mandatory or unique
            ;; and it wasn't before, than add not null constraint
            (and
              (not (#{"mandatory"} dc))
              (#{"mandatory"} constraint))
            (conj
              (do
                (log/debugf "Setting NOT NULL constraint in table %s column %s" old-table column)
                (format
                  "alter table \"%s\" alter column %s set not null"
                  old-table column)))
            (and
              (#{"mandatory"} dc)
              (or 
                ;; If current is not mandatory or unique but it previously was,
                ;; than remove not null constraint
                (not (#{"mandatory"} constraint))
                ;; Or if attribute is removed, than remove not null constraint
                (core/removed-attribute? attribute)))
            (conj
              (do
                (log/debugf "[%s]Removing table %s column %s NOT NULL constraint" euuid old-table column)
                (format
                  "alter table \"%s\" alter column %s drop not null"
                  old-table column)))
            ;; Change attribute name
            (and dn (not= (column-name dn) (column-name name))) 
            (conj 
              (do
                (log/debugf "Renaming table %s column %s -> %s" old-table column (column-name name))
                (format 
                  "alter table \"%s\" rename column %s to %s" 
                  old-table column (column-name name))))
            ;; If attribute type has changed to enum
            ;; than create new enum type with defined values
            (and (= type "enum") (some? dt))
            (conj (generate-enum-type-ddl new-enum-name values))
            ;; Type has changed so you better cast to target type
            dt 
            (conj
              (do
                (log/debugf "Changing table %s column %s type %s -> %s" old-table column dt type)
                ;; TODO - this can be implemented by renaming current column
                ;; and adding new column with target reference to replace current column
                ;; and afterwards deleting renamed column. BE AWARE THAT ALL VALUES FOR THAT COLUMN
                ;; WILL BE LOST
                (when (#{"user" "group" "role"} type)
                  (throw
                    (ex-info
                      "Can't alter special fields"
                      {:type type
                       :attribute name
                       :entity (:name entity)})))
                (cond->
                  (format
                    "alter table \"%s\" alter column %s type %s"
                    old-table column 
                    (case type
                      "enum" new-enum-name
                      (type->ddl type)))
                  (= "int" type) (str " using(trim(" column ")::integer)")
                  (= "float" type) (str " using(trim(" column ")::float)")
                  (= "string" type) (str " using(" column "::text)")
                  (= "avatar" type) (str " using(" column "::text)")
                  (= "encrypted" type) (str " using(" column "::text)")
                  (= "hashed" type) (str " using(" column "::text)")
                  (= "boolean" type) (str " using(trim(" column ")::boolean)")
                  (= "enum" type) (str " using(" column ")::" old-enum-name))))
            ;; If attribute was previously enum and now it is not enum
            ;; than delete enum type
            (= dt "enum")
            (conj (format "drop type \"%s\"" old-enum-name))
            ;; If enum name has changed than apply changes
            (and (= type "enum") (not= old-enum-name new-enum-name))
            (conj (format "alter type %s rename to %s" old-enum-name new-enum-name))
            ;; If type is enum and it hasn't changed
            ;; but enum configuration has changed, than create statements to
            ;; compensate changes  
            (and (= type "enum") (nil? dt))
            (as-> statements
              (let [[ov nv] (clojure.data/diff 
                              (reduce
                                (fn [r [idx {n :name
                                             e :euuid}]]
                                  (if (not-empty n)
                                    (assoc r (or e (get-in values [idx :euuid])) n)
                                    r))
                                nil
                                (map-indexed (fn [idx v] (vector idx v)) (:values dconfig)))
                              (zipmap 
                                (map :euuid values)
                                (map :name values)))
                    column (column-name name)]
                ; alter type my_enum rename to my_enum__;
                ; -- create the new enum
                ; create type my_enum as enum ('value1', 'value2', 'value3');

                ; -- alter all you enum columns
                ; alter table my_table
                ; alter column my_column type my_enum using my_column::text::my_enum;

                ; -- drop the old enum
                ; drop type my_enum__;
                (log/tracef "Diff config\n%s" dconfig)
                (log/tracef "Old enums: %s" ov)
                (log/tracef "New enums: %s" nv)
                (conj
                  statements
                  (format "alter type %s rename to %s__" new-enum-name new-enum-name)
                  (format "create type %s as enum (%s)" new-enum-name (clojure.string/join "," (map #(str \' % \') (remove empty? (concat (map :name values) (vals ov))))))
                  ;; UPDATE your_table SET new_column =
                  ;;    CASE your_column
                  ;;        WHEN 'active' THEN 'active'
                  ;;        WHEN 'inactive' THEN 'suspended'
                  ;;        WHEN 'pending' THEN 'pending'
                  ;;        ELSE your_column
                  ;;    END;
                  (if (empty? ov) ""
                    (str
                      "update " old-table " set " column " ="
                      "\n  case " column  "\n    "
                      (clojure.string/join
                        "\n    "
                        (reduce-kv
                          (fn [r euuid old-name]
                            (if-let [new-name (get nv euuid)]
                              (conj r (str "     when '" old-name "' then '" new-name "'"))
                              r))
                          (list (str " else " column))
                          ov))
                      "\n   end;"))

                  (format "alter table \"%s\" alter column %s type %s using %s::text::%s"
                          old-table column new-enum-name column new-enum-name)
                  (format "drop type %s__" new-enum-name))))))))))


;; 1. Change attributes by calling attribute-delta->ddl
;; 2. Rename table if needed
;; 3. Change constraints
(defn entity-delta->ddl
  [{:keys [attributes] :as entity} ]
  (assert (core/diff? entity) "This entity is already synced with DB")
  (let [diff (core/diff entity)
        old-entity (core/suppress entity)
        old-table (entity->table-name old-entity)
        old-constraints (get-in old-entity [:configuration :constraints :unique])
        table (entity->table-name entity)
        attributes' (keep #(attribute-delta->ddl entity %) attributes)]
    (cond-> (reduce into [] attributes')
      ;; Renaming occured
      (:name diff)
      (into
        (cond->
          [(format "alter table \"%s\" rename to \"%s\"" old-table table)
           (format "alter table \"%s\" rename constraint \"%s_pkey\" to \"%s_pkey\"" table old-table table)
           (format "alter table \"%s\" rename constraint \"%s_euuid_key\" to \"%s_euuid_key\"" table old-table table)
           (format "alter sequence \"%s__eid_seq\" rename to \"%s__eid_seq\"" old-table table)
           ;; AUDIT
           (format "alter table \"%s\" rename constraint \"%s_%s_fkey\" to \"%s_%s_fkey\"" table old-table "modified_by" table "modified_by")]
          ;;
          (some? old-constraints)
          (into
            (map-indexed
              (fn [idx _]
                (let [constraint (str "_eucg_" idx)] 
                  (format 
                    "alter table \"%s\" rename constraint \"%s%s\" to \"%s%s\"" 
                    table old-table constraint 
                    table constraint)))
              old-constraints))))
      ;; If there are some differences in constraints
      (-> diff :configuration :constraints)
      (into
        ;; concatenate constraints
        (let [ncs (-> entity :configuration :constraints :unique)
              ocs old-constraints 
              groups (max (count ocs) (count ncs))]
          ;; by reducing
          (when (pos? groups)
            (reduce
              (fn [statements idx]
                (let [o (try (nth ocs idx) (catch Throwable _ nil))
                      n (try (nth ncs idx) (catch Throwable _ nil))
                      constraint (str "_eucg_" idx)
                      new-constraint (format 
                                       "alter table \"%s\" add constraint %s unique(%s)"
                                       table (str table constraint)
                                       (clojure.string/join
                                         ","
                                         (map 
                                           (fn [id]
                                             (->
                                               (core/get-attribute entity id)
                                               :name
                                               column-name))
                                           n)))
                      drop-constraint (format 
                                        "alter table \"%s\" drop constraint %s"
                                        table 
                                        (str table constraint))]
                  (cond->
                    statements
                    ;; Add new constraint
                    (empty? o)
                    (conj new-constraint)
                    ;; Delete old constraint group
                    (empty? n)
                    (conj drop-constraint)
                    ;; When constraint has changed
                    (and 
                      (every? not-empty [o n])
                      (not= (set o) (set n)) )
                    (conj
                      drop-constraint
                      new-constraint))))
              []
              (range groups))))))))


(defn transform-relation
  [tx {:keys [from to] :as relation}]
  (let [diff (core/diff relation)
        _ (log/tracef "Transforming relation\ndiff=%s\nfrom=%s\nto=%s" diff from to)
        from-diff (:from diff)
        to-diff (:to diff)
        old-from (core/suppress from) 
        old-to (core/suppress to)
        old-relation (core/suppress relation) 
        old-name (relation->table-name old-relation) 
        ;; Assoc old from and to entities
        ;; This will be handled latter
        new-name (relation->table-name relation)]
    ;; When name has changed
    (when (not= old-name new-name)
      (let [sql (format
                  "alter table %s rename to %s"
                  old-name new-name)]
        (log/debugf "Renaming relation table %s->%s\n%s" old-name new-name sql)
        (execute-one! tx [sql])))
    ;; when to name has changed than change table column
    (when (:name to-diff)
      (let [o (entity->relation-field old-to)
            n (entity->relation-field to)
            sql (format
                  "alter table %s rename column %s to %s"
                  new-name o n)]
        (log/debugf "Renaming relation table %s -> %s\n%s"  old-name new-name sql)
        (execute-one! tx [sql])))
    ;; when from name has changed than change table column
    (when (:name from-diff)
      (let [o (entity->relation-field old-from)
            n (entity->relation-field from)
            sql (format
                  "alter table %s rename column %s to %s"
                  new-name o n)]
        (log/debugf "Renaming relation %s -> %s\n%s"  old-name new-name sql)
        (execute-one! tx [sql])))))


;; 1. Generate new entities by creating tables
;;  - Create new types if needed by enum attributes
;; 2. Add audit attributes if present (modified_by,modified_on)
;; 3. Check if model has changed attributes
;;  - If so try to resolve changes by calling entity-delta->ddl
;; 4. Check if model has changed relations
;;  - If so try to resolve changes by calling transform-relation
;; 5. Generate new relations by connecting entities
(defn transform-database [ds projection configuration]
  (log/debugf "Transforming database\n%s" configuration)
  (let [{ne :new/entities
         nr :new/relations
         nrr :new/recursive-relations
         ce :changed/entities
         cr :changed/relations
         crr :changed/recursive-relations} (analyze-projection projection)
        {amt :who/table} configuration]
    (log/tracef
      "Transform projection analysis\nNew\n%s\nChanged\n%s"
      {:new/entities (map :name ne)
       :new/relations (map (juxt :from-label :to-label) nr)
       :new/recursive (map (juxt :from-label :to-label) nrr)}
      {:changed/entities (map :name ce)
       :changed/relations (map (juxt :from-label :to-label) cr)
       :changed/recursive (map (juxt :from-label :to-label) crr)})
    (jdbc/with-transaction [tx ds]
      ;; Generate new entities
      (when (not-empty ne) (log/info "Generating new entities..."))
      (doseq [{n :name :as entity} ne
              :let [table-sql (generate-entity-ddl entity)
                    enum-sql (generate-entity-enums-ddl entity)
                    table (entity->table-name entity)]]
        (when (not-empty enum-sql)
          (log/debugf "Adding entity %s enums\n%s" n enum-sql)
          (execute! tx [enum-sql]))
        (log/debugf "Adding entity %s to DB\n%s" n table-sql)
        (execute-one! tx [table-sql])
        (let [sql (format 
                    "alter table \"%s\" add column \"%s\" int references \"%s\"(_eid) on delete set null"
                    table
                    "modified_by"
                    amt)]
          (log/tracef "Adding table audit reference[who] column\n%s" sql)
          (execute-one! tx [sql]))
        (let [sql (format 
                    "alter table \"%s\" add column \"%s\" timestamp not null default localtimestamp"
                    table
                    "modified_on")] 
          (log/tracef "Adding table audit reference[when] column\n%s" sql)
          (execute-one! tx [sql])))
      ;; Change entities
      (when (not-empty ce) (log/info "Checking changed entities..."))
      (doseq [{n :name :as entity} ce
              :let [sql (entity-delta->ddl entity)]]
        (log/debugf "Changing entity %s" n)
        (doseq [statement sql]
          (log/debugf "Executing statement %s\n%s" n statement)
          (execute-one! tx [statement])))
      ;; Change relations
      (when (not-empty cr) 
        (log/info "Checking changed trans entity relations..."))
      (doseq [r cr] (transform-relation tx r))
      ;; Change recursive relation
      (when (not-empty crr) 
        (log/info "Checking changed recursive relations..."))
      (doseq [{{tname :name 
                :as e} :to 
               tl :to-label
               diff :diff} crr
              :let [table (entity->table-name e)
                    _ (log/debugf "RECURSIVE RELATION\n%s" diff)
                    sql (when diff
                          (format
                            "alter table %s rename column %s to %s"
                            table (column-name tl) (column-name (:to-label diff))))]]
        (when sql
          (log/debugf "Updating recursive relation for entity %s\n%s" tname sql)
          (execute-one! tx [sql])))
      ;; Generate new relations
      (when (not-empty nr) (log/info "Generating new relations..."))
      (doseq [{{tname :name} :to {fname :name} :from :as relation} nr
              :let [sql (generate-relation-ddl projection relation)
                    [from-idx to-idx] (generate-relation-indexes-ddl projection relation)]]
        (log/debugf "Connecting entities %s <-> %s\n%s" fname tname sql)
        (execute-one! tx [sql])
        (when from-idx
          (log/debugf "Creating from indexes for relation: %s <-> %s\n%s" fname tname from-idx)
          (execute-one! tx [from-idx]))
        (when from-idx
          (log/debugf "Creating to indexes for relation: %s <-> %s\n%s" fname tname from-idx)
          (execute-one! tx [to-idx])))
      ;; Add new recursive relations
      (when (not-empty nrr) 
        (log/info "Adding new recursive relations..."))
      (doseq [{{tname :name
                :as e} :to
               tl :to-label} nrr
              :when (not-empty tl)
              :let [table (entity->table-name e)
                    sql (format
                          "alter table %s add %s bigint references \"%s\"(_eid) on delete cascade"
                          table (column-name tl) table)]]
        (log/debug "Creating recursive relation for entity %s\n%s" tname sql)
        (execute-one! tx [sql])))))




(defn model->schema [model]
  (binding [*model* model]
    (reduce
      (fn [schema {:keys [euuid] ename :name :as entity}]
        (log/tracef "Building schema for entity %s[%s]" ename euuid)
        (let [table (entity->table-name entity)
              fields (reduce
                       (fn [fields {euuid :euuid aname :name t :type}]
                         (log/tracef "Adding field %s in entity %s to schema" aname ename)
                         (let [f {:key (keyword (normalize-name aname))
                                  :euuid euuid
                                  :type t}]
                           (assoc fields euuid 
                                  (case t 
                                    "enum"
                                    (assoc f :postgres/type (normalize-name (str table \space aname)))
                                    ;;
                                    "user"
                                    (assoc f :postgres/reference au/user)
                                    ;;
                                    "group"
                                    (assoc f :postgres/reference au/user-group)
                                    ;;
                                    "role"
                                    (assoc f :postgres/reference au/user-role)
                                    ;;
                                    f))))
                       {:audit/who {:key :modified_by
                                    :type "user"
                                    :postgres/reference au/user}}
                       (:attributes entity))
              {relations :relations
               recursions :recursions}
              (group-by 
                (fn [{t :cardinality}]
                  (case t
                    "tree" :recursions
                    :relations))
                (core/focus-entity-relations model entity))
              ;;
              relations (reduce
                          (fn [relations 
                               {:keys [euuid from to to-label cardinality]
                                :as relation}]
                            (if (and
                                  (some? from) (some? to)
                                  (not-empty to-label)) 
                              (do
                                (log/tracef
                                  "Building schema for relation\n%s" 
                                  {:euuid euuid 
                                   :from (:name from) 
                                   :to (:name to) 
                                   :cardinality cardinality})
                                (assoc relations (keyword (normalize-name to-label))
                                       {:from (:euuid from)
                                        :from/field (entity->relation-field from)
                                        :from/table (entity->table-name from)
                                        :to (:euuid to)
                                        :to/field (entity->relation-field to)
                                        :to/table (entity->table-name to)
                                        :table (relation->table-name relation)
                                        :type (case cardinality
                                                ("m2o" "o2o") :one
                                                ("m2m" "o2m") :many)}))
                              relations))
                          {}
                          relations)
              recursions (set (map (comp keyword normalize-name :to-label) recursions))
              mandatory-attributes (keep
                                     (fn [{:keys [constraint name]}]
                                       (when (= constraint "mandatory")
                                         (keyword (normalize-name name))))
                                     (:attributes entity))
              entity-schema (cond->
                              {:table table
                               :name (:name entity)
                               :constraints (cond->
                                              (get-in entity [:configuration :constraints])
                                              ;;
                                              (not-empty mandatory-attributes)
                                              (assoc :mandatory mandatory-attributes))
                               :fields fields
                               :field->attribute (reduce-kv
                                                   (fn [r a {field :key}]
                                                     (assoc r field a))
                                                   nil
                                                   fields)
                               :recursions recursions
                               :relations relations
                               :audit/who :modified_by
                               :audit/when :modified_on})]
          (assoc schema euuid entity-schema)))
      {}
      (core/get-entities model))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Protocol impelmentations ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn scalar-attribute? [{t :type}]
  (contains? 
    #{"int" "float" "boolean" "string" "avatar"
      "encrypted" "timestamp" "timeperiod" "currency"
      "transit" "json" "uuid" "enum"}
    t))

(defn reference-attribute? [{t :type}]
  (contains? #{"user" "group" "role"} t))

(defn attribute->gql-field [n]
  (keyword (normalize-name n)))

(defn entity->gql-object [n]
  (csk/->PascalCaseKeyword n))

(defn entity->gql-input-object [n]
  (csk/->PascalCaseKeyword (str n " input")))

(defn entity-attribute->enum [{ename :name} {aname :name}]
  (csk/->PascalCaseKeyword (str ename \space aname)))

(defn entity-attribute->enum-operator [{ename :name} {aname :name}]
  (csk/->PascalCaseKeyword (str ename \space aname \space "operator")))

(def ignored-field-type? #{"transit" "json" "encrypted" "hashed"})

(defn entity->search-operator [{n :name}]
  (csk/->camelCaseKeyword (str "search " n " operator")))

(defn entity->order-by-operator [{n :name}]
  (csk/->camelCaseKeyword (str "order by " n " operator")))

(defn entity->attributes-enum [{n :name}]
  (csk/->camelCaseKeyword (str n "Attributes")))

(defn entity->distinct-on-operator [{n :name}]
  (csk/->camelCaseKeyword (str "distinct on " n " operator")))

(defn entity->aggregate-object [{n :name}]
  (csk/->PascalCaseKeyword (str n " aggregate")))

(defn entity->slice-object [{n :name}]
  (csk/->PascalCaseKeyword (str n " slice")))

(defn attribute-type->scalar [entity {t :type :as attribute}]
  (case t
    "int" 'Int
    "float" 'Float
    "boolean" 'Boolean
    "avatar" 'String
    "string" 'String
    "encrypted" 'Encrypted
    "hashed" 'Hash
    "timestamp" 'Timestamp
    "timeperiod" 'TimePeriod
    "currency" 'Currency
    "json" 'JSON
    "transit" 'Transit
    "uuid" 'UUID
    "enum" (entity-attribute->enum entity attribute)))

(defn attribute-type->operator [entity {t :type :as attribute}]
  (case t
    "int" {:type :IntegerQueryOperator}
    "float" {:type :FloatQueryOperator}
    "boolean" {:type :BooleanQueryOperator}
    "string" {:type :StringQueryOperator}
    "avatar" {:type :StringQueryOperator}
    "encrypted" {:type :StringQueryOperator}
    "timestamp" {:type :TimestampQueryOperator}
    "timeperiod" {:type :TimePeriodQueryOperator}
    "currency" {:type :CurrencyQueryOperator}
    "transit" {:type :StringQueryOperator}
    "uuid" {:type :UUIDQueryOperator}
    "enum" {:type (entity-attribute->enum-operator entity attribute)} 
    {:type t}))


(defn has-numerics? [{as :attributes}]
  (some
    (fn [{t :type}]
      (boolean (#{"integer" "float"} t)))
    as))


(defn numerics? [{as :attributes}]
  (filter 
    (fn [{t :type}]
      (boolean (#{"integer" "float"} t)))
    as))

(defn entity->numeric-enum [{n :name}]
  (csk/->PascalCaseKeyword (str n " Numerics")))


(defn entity->relation-enum [{n :name}]
  (csk/->PascalCaseKeyword (str n " relations enum")))

(def currency-codes
  (let [table (with-open [reader (io/reader (io/resource "lacinia/currency.csv"))]
                (doall
                  (csv/read-csv reader)))]
    (distinct
      (keep
        (fn [[_ _ code]]
          (when (not-empty code) code))
        (rest table)))))

(defn generate-lacinia-enums
  [model]
  (letfn [(model->enums [] 
            (let [entities (core/get-entities model)]
              (reduce
                (fn [enums {:keys [attributes] :as entity}]
                  (as-> enums ens
                    ;; This may be required 
                    (assoc enums (entity->attributes-enum entity)
                           {:values (mapv 
                                      (comp normalize-name :name) 
                                      (filter scalar-attribute? attributes))})
                    (let [le (filter #(= (:type %) "enum") attributes)]
                      (if (not-empty le)
                        (reduce
                          (fn [enums {config :configuration :as attribute}]
                            (assoc enums (entity-attribute->enum entity attribute) 
                              (update config :values #(mapv (comp normalized-enum-value :name) %))))
                          ens
                          le)
                        ens))))
                {}
                entities)))]
    (merge
      (model->enums)
      {:currency_enum {:values currency-codes}
       :order_by_enum {:values [:asc :desc]}
       :is_null_enum {:values [:is_null :is_not_null]}})))


(defn generate-lacinia-objects 
  [model]
  (letfn [(reference-object [euuid]
            (let [entity (core/get-entity model euuid)]
              {:type (entity->gql-object (:name entity))
               :args {:_where {:type (entity->search-operator entity)}}}))]
    (let [entities (core/get-entities model)
          _who :modified_by
          _when :modified_on]
      (reduce
        (fn [r {ename :name attributes :attributes :as entity}]
          (let [numerics (reduce
                           (fn [fields {aname :name atype :type}]
                             (case atype
                               ("integer" "float") (conj fields (attribute->gql-field aname))
                               fields))
                           [] 
                           attributes)
                scalars (filter scalar-attribute? attributes)
                references (filter reference-attribute? attributes)
                entity-relations (core/focus-entity-relations model entity)
                to-relations (filter #(not-empty (:to-label %)) entity-relations)]
            (if (nil? ename) r
              (cond->
                (assoc 
                  r (entity->gql-object ename)
                  {:fields (as-> 
                             {:euuid {:type 'UUID}
                              :modified_by (reference-object au/user)
                              :modified_on {:type :Timestamp}}
                             fields 
                             ;; References to well known objects
                             (reduce
                               (fn [fields {aname :name t :type}]
                                 (case t
                                   "user" (assoc fields (attribute->gql-field aname) (reference-object au/user))
                                   "group" (assoc fields (attribute->gql-field aname) (reference-object au/user-group))
                                   "role" (assoc fields (attribute->gql-field aname) (reference-object au/user-role))))
                               fields
                               references)
                             ;; Scalars
                             (reduce
                               (fn [fields {aname :name atype :type active :active
                                            :as attribute}]
                                 (if-not (or active (= aname "euuid")) fields 
                                   (let [t (attribute-type->scalar entity attribute)]
                                     (assoc fields (attribute->gql-field aname)
                                            (cond-> {:type t}
                                              (= "enum" atype)
                                              (assoc
                                                :args (zipmap
                                                        [:_eq :_neq :_in :_not_in]
                                                        (concat
                                                          (repeat 2 {:type t})
                                                          (repeat 2 {:type (list 'list t)}))))
                                              (= t 'UUID)
                                              (assoc
                                                :args (zipmap
                                                        [:_eq :_neq :_in :_not_in]
                                                        (concat
                                                          (repeat 2 {:type 'UUID})
                                                          (repeat 2 {:type (list 'list 'UUID)}))))
                                              (= t 'Boolean)
                                              (assoc
                                                :args (zipmap
                                                        [:_eq :_neq]
                                                        (repeat {:type 'Boolean})))
                                              (= t 'Int) 
                                              (assoc 
                                                :args (zipmap
                                                        [:_gt :_lt :_eq :_neq :_ge :_le :_in :_not_in]
                                                        (concat
                                                          (repeat 6 {:type 'Int})
                                                          (repeat 6 {:type (list 'list 'Int)}))))
                                              ;;
                                              (= t 'Float) 
                                              (assoc 
                                                :args (zipmap
                                                        [:_gt :_lt :_eq :_neq :_ge :_le :_in :_not_in]
                                                        (concat
                                                          (repeat 6 {:type 'Float})
                                                          (repeat 6 {:type (list 'list 'Float)}))))
                                              (= t 'String)
                                              (assoc
                                                :args (zipmap
                                                        [:_neq :_eq :_like :_ilike :_in :_not_in] 
                                                        (concat
                                                          (repeat 4 {:type 'String})
                                                          (repeat 4 {:type (list 'list 'String)}))))
                                              (= t 'Currency)
                                              (assoc
                                                :args (assoc
                                                        (zipmap
                                                          [:_gt :_lt :_neq :_eq :_ge :_le]
                                                          (repeat {:type 'Float}))
                                                        :in_currencies {:type (list 'list :currency_enum)}))
                                              (= t 'Timestamp)
                                              (assoc
                                                :args (zipmap
                                                        [:_gt :_lt :_neq :_eq :_ge :_le]
                                                        (repeat {:type 'Timestamp})))
                                              (= t 'Timeperiod)
                                              (assoc
                                                :args (merge
                                                        (zipmap
                                                          [:_gt :_lt :_neq :_eq :_ge :_le ]
                                                          (repeat {:type 'TimePeriod}))
                                                        (zipmap
                                                          [:contains :exclude]
                                                          (repeat {:type 'Timestamp})))))))))
                               fields
                               (conj scalars {:name "euuid" :type "uuid"}))
                             ;; Outgoing relations
                             (reduce
                               (fn [fields {:keys [to to-label cardinality]}]
                                 (if (and (not-empty to-label) (not-empty (:name to))) 
                                   (assoc fields (attribute->gql-field to-label) 
                                          (let [t (entity->gql-object (:name to))] 
                                            (case cardinality
                                              ("o2m" "m2m") {:type (list 'list t)
                                                             :args {:_offset {:type 'Int}
                                                                    :_limit {:type 'Int}
                                                                    :_where {:type (entity->search-operator to)}
                                                                    :_order_by {:type (entity->order-by-operator to)}}}
                                              ("m2o" "o2o") {:type t
                                                             :args {:_where {:type (entity->search-operator to)}}}
                                              "tree" {:type t 
                                                      :args {:_where {:type (entity->search-operator entity)}
                                                             (attribute->gql-field to-label) {:type :is_null_enum}}}
                                              {:type t})))
                                   fields))
                               fields
                               to-relations))}
                  ;;
                  (entity->aggregate-object entity)
                  {:fields
                   (cond->
                     (reduce
                       (fn [fields {:keys [to to-label]}]
                         (if (not-empty to-label) 
                           (assoc fields (attribute->gql-field to-label)
                                  {:type (entity->aggregate-object to)
                                   :args {:_where {:type (entity->search-operator to)}}})
                           fields))
                       (reduce
                         (fn [fields {t :type n :name}]
                           (assoc fields (attribute->gql-field n)
                             (case t
                               "int"
                               {:type :IntAggregate
                                :args (zipmap
                                        [:_gt :_lt :_eq :_neq :_ge :_le]
                                        (repeat {:type 'Int}))}
                               "float"
                               {:type :FloatAggregate
                                :args (zipmap
                                        [:_gt :_lt :_eq :_neq :_ge :_le]
                                        (repeat {:type 'Float}))})))
                         {:count {:type 'Int}}
                         (numerics? entity))
                       to-relations))})
                ;;
                (not-empty to-relations)
                (assoc 
                  (entity->slice-object entity)
                  {:fields
                   (cond->
                     (reduce
                       (fn [fields {:keys [to to-label]}]
                         (if (not-empty to-label) 
                           (assoc fields (attribute->gql-field to-label)
                                  {:type 'Boolean
                                   :args {:_where {:type (entity->search-operator to)}}})
                           fields))
                       nil
                       entity-relations))})
                (not-empty numerics)
                (assoc (entity->numeric-enum entity) 
                       {:fields (zipmap numerics (repeat {:type 'Float}))})))))
        {:Currency
         {:fields
          {:currency {:type :currency_enum 
                      :args (zipmap
                              [:_neq :_eq :_in :_nin] 
                              (repeat {:type 'String}))}
           :amount {:type 'Float
                    :args (zipmap
                            [:_gt :_lt :_eq :_neq :_ge :_le]
                            (repeat {:type 'Float}))}}}
         ;;
         :IntAggregate
         {:fields
          (zipmap
            [:min :max :sum :avg]
            (repeat {:type 'Int}))}
         :FloatAggregate
         {:fields
          (zipmap
            [:min :max :sum :avg]
            (repeat {:type 'Float}))}
         ;;
         :TimePeriod
         {:fields
          {:start {:type 'Timestamp}
           :end {:type 'Timestamp}}
          :args (merge
                  (zipmap
                    [:_gt :_lt :_neq :_eq :_ge :_le ]
                    (repeat {:type 'TimePeriod}))
                  (zipmap
                    [:_contains :_exclude]
                    (repeat {:type 'Timestamp})))}}
        entities))))

(defn generate-lacinia-input-objects 
  [model]
  (let [entities (core/get-entities model)
        user (core/get-entity model au/user)
        group (core/get-entity model au/user-group)
        role (core/get-entity model au/user-role)
        user-input (csk/->PascalCaseKeyword (str (:name user) " input"))
        group-input (csk/->PascalCaseKeyword (str (:name group) " input"))
        role-input (csk/->PascalCaseKeyword (str (:name role) " input"))] 
    (letfn [(referenced-entity [{atype :type}]
              (case atype
                "user" user
                "group" group
                "role" role))]
      (reduce
        (fn [r {ename :name attributes :attributes :as entity}]
          (let [i (csk/->PascalCaseKeyword (str ename " input"))
                s (entity->search-operator entity)
                o (entity->order-by-operator entity)
                d (entity->distinct-on-operator entity)
                relations (core/focus-entity-relations model entity)
                recursions (filter 
                             #(= "tree" (:cardinality %)) 
                             relations)
                attributes' (conj attributes
                                  {:name "euuid" :type "uuid" :active true}
                                  {:name "modified_on"
                                   :type "timestamp"
                                   :active true})]
            (assoc 
              (reduce
                (fn [r attribute]
                  (let [t (entity-attribute->enum entity attribute)] 
                    (assoc r (entity-attribute->enum-operator entity attribute)
                           {:fields (zipmap
                                      [:_eq :_neq :_in :_not_in]
                                      (concat
                                        (repeat 2 {:type t})
                                        (repeat 2 {:type (list 'list t)})))})))
                r
                (filter #(= (:type %) "enum") attributes))
              ;; input fields
              i {:fields (as-> {} input
                           ;;
                           (reduce
                             (fn [fields {;constraint :constraint
                                          aname :name atype :type active :active :as attribute}]
                               (if-not (or active (= aname "euuid")) 
                                 fields
                                 (assoc fields 
                                        (keyword (normalize-name aname))
                                        {:type (let [t (case atype 
                                                         "currency"
                                                         'CurrencyInput
                                                         ;;
                                                         "timeperiod"
                                                         'TimePeriodInput
                                                         ;;
                                                         "user"
                                                         user-input
                                                         ;;
                                                         "group"
                                                         group-input
                                                         ;;
                                                         "role"
                                                         role-input
                                                         ;;
                                                         (attribute-type->scalar entity attribute))]
                                                 t
                                                 ;; TODO - To be fully compliant enable this
                                                 ;; For now this is disabled to enable easier
                                                 ;; data storing for eywa.dataset namespace
                                                 ;; that splits data into stack and slice mutations
                                                 #_(if (= constraint "mandatory")
                                                   (list 'non-null t)
                                                   t))})))
                             input
                             (conj attributes {:name "euuid" :type "uuid"}))
                           ;;
                           (reduce
                             (fn [fields {:keys [to to-label cardinality]}]
                               (if (not-empty to-label) 
                                 (assoc fields (keyword (normalize-name to-label))
                                        {:type (case cardinality
                                                 ("o2m" "m2m") (list 'list (csk/->PascalCaseKeyword (str (:name to) " input")))
                                                 (csk/->PascalCaseKeyword (str (:name to) " input")))})
                                 fields))
                             input
                             relations))}
              ;; search/filter fields operator
              s {:fields (as-> 
                           {:_and {:type (list 'list s)}
                            :_or {:type (list 'list s)}
                            :_not {:type (list 'list s)}}
                           operator
                           ;;
                           (reduce
                             (fn [args {:keys [from-label to-label]}]
                               (cond-> args
                                 (not-empty to-label)
                                 (assoc (keyword (normalize-name to-label)) {:type :is_null_enum})
                                 ;;
                                 (not-empty from-label)
                                 (assoc (keyword (normalize-name from-label)) {:type :is_null_enum})))
                             operator
                             recursions)
                           ;;
                           (reduce
                             (fn [fields {aname :name atype :type active :active :as attribute}]
                               (if (or
                                     (not active)
                                     (ignored-field-type? atype)
                                     (reference-attribute? attribute)) 
                                 fields
                                 ;;
                                 (assoc 
                                   fields 
                                   (keyword (normalize-name aname))
                                   (attribute-type->operator entity attribute))))
                             operator
                             (conj
                               attributes'
                               {:name "modified_by"
                                :type (entity->search-operator (core/get-entity model au/user)) 
                                :active true}
                               {:name "modified_on"
                                :type :TimestampQueryOperator 
                                :active true})))}
              ;; _order_by operator
              o {:fields (reduce
                           (fn [fields {:keys [to to-label cardinality]}]
                             (case cardinality
                               ("m2o" "o2o") 
                               (if (not-empty to-label)
                                 (assoc 
                                   fields
                                   (keyword (normalize-name to-label))
                                   {:type (entity->order-by-operator to)})
                                 fields)
                               fields))
                           (reduce
                             (fn [fields {aname :name atype :type active :active :as attribute}]
                               (if-not (or active (= aname "euuid")) 
                                 fields
                                 (if (#{"json" "encrypted" "hashed"} atype) fields
                                   (if (reference-attribute? attribute)
                                     (assoc 
                                       fields
                                       (keyword (normalize-name aname))
                                       {:type (entity->order-by-operator
                                                (referenced-entity attribute))})
                                     (assoc 
                                       fields 
                                       (keyword (normalize-name aname))
                                       {:type :order_by_enum})))))
                             nil
                             attributes')
                           (conj
                             relations
                             {:to (core/get-entity model au/user)
                              :to-label "modified_by" 
                              :cardinality "o2o"}))}
              ;;
              d {:fields (reduce
                           (fn [fields {:keys [to to-label cardinality]}]
                             (case cardinality
                               ;;
                               ("m2o" "o2o") 
                               (if (not-empty to-label)
                                 (assoc 
                                   fields
                                   (keyword (normalize-name to-label))
                                   {:type (entity->distinct-on-operator to)})
                                 fields)
                               ;; Default
                               fields))
                           ;;
                           (reduce
                             (fn [fields {aname :name :as attribute}]
                               (assoc fields
                                 (keyword (normalize-name aname))
                                 {:type (entity->distinct-on-operator
                                          (referenced-entity attribute))}))
                             {:attributes
                              {:type (list 'list (entity->attributes-enum entity))}}
                             (filter reference-attribute? attributes))
                           ;; 
                           (conj relations
                                 {:to (core/get-entity model au/user)
                                  :to-label "modified_on" 
                                  :cardinality "o2o"}))})))
        {:UUIDQueryOperator
         {:fields {:_eq {:type 'UUID} :_neq {:type 'UUID}
                   :_in {:type (list 'list 'UUID)}
                   :_not_in {:type (list 'list 'UUID)}}}
         :BooleanQueryOperator
         {:fields {:_eq {:type 'Boolean} :_neq {:type 'Boolean}}}
         :CurrencyInput
         {:fields
          {:amount {:type 'Float}
           :currency {:type :currency_enum}}}
         :TimePeriodInput
         {:fields
          {:start {:type 'Timestamp}
           :end {:type 'Timestamp}}}
         :StringQueryOperator
         {:fields
          (zipmap
            [:_neq :_eq :_like :_ilike :_in :_not_in] 
            (concat
              (repeat 4 {:type 'String})
              (repeat 4 {:type (list 'list 'String)})))}
         :IntegerQueryOperator
         {:fields
          (zipmap
            [:_gt :_lt :_eq :_neq :_ge :_le :_in :_not_in]
            (concat
              (repeat 6 {:type 'Int})
              (repeat 6 {:type (list 'list 'Int)})))}
         :FloatQueryOperator
         {:fields
          (zipmap
            [:_gt :_lt :_eq :_neq :_ge :_le :_in :_not_in]
            (concat
              (repeat 6 {:type 'Float})
              (repeat 6 {:type (list 'list 'Float)})))}
         :CurrencyQueryOperator
         {:fields
          (assoc
            (zipmap
              [:_gt :_lt :_neq :_eq :_ge :_le]
              (repeat {:type 'Float}))
            :in_currencies {:type (list 'list 'String)})}
         :TimestampQueryOperator
         {:fields 
          (zipmap
            [:_gt :_lt :_neq :_eq :_ge :_le]
            (repeat {:type 'Timestamp}))}
         :TimePeriodQueryOperator
         {:fields
          (merge
            (zipmap
              [:_gt :_lt :_neq :_eq :_ge :_le ]
              (repeat {:type 'TimePeriodInput}))
            (zipmap
              [:_contains :_exclude]
              (repeat {:type 'Timestamp})))}}
        entities))))



(defn log-query
  [context]
  (log/debugf
    "Processing query for user %s[%d]:\n%s"
    (:username context)
    (:user context)
    ; (with-out-str (clojure.pprint/pprint context))
    (parser/summarize-query (:com.walmartlabs.lacinia.constants/parsed-query context))))


(defn generate-lacinia-queries [model]
  (let [entities (core/get-entities model)
        user (core/get-entity model au/user)
        group (core/get-entity model au/user-group)
        role (core/get-entity model au/user-role)
        user-operator (entity->search-operator user)
        group-operator (entity->search-operator group)
        role-operator (entity->search-operator role)]
    (letfn [(reference-operator [{atype :type}]
              (case atype
                "user" user-operator
                "group" group-operator
                "role" role-operator))
            (attribute->type [entity attribute]
              (if (reference-attribute? attribute)
                {:type (reference-operator attribute)}
                (attribute-type->operator entity attribute)))]
      (reduce
        (fn [queries {euuid :euuid ename :name 
                      as :attributes 
                      {{uniques :unique} :constraints} :configuration
                      :as entity}]
          (let [relations (core/focus-entity-relations model entity)
                recursions (filter #(= "tree" (:cardinality %)) relations)
                get-args (reduce
                           (fn [args ids]
                             (reduce
                               (fn [args id]
                                 (let [{aname :name :as attribute} (core/get-attribute entity id)]
                                   (assoc args (keyword (normalize-name aname)) 
                                          {:type (cond 
                                                   (scalar-attribute? attribute) 
                                                   (attribute-type->scalar entity attribute)
                                                   ;;
                                                   (reference-attribute? attribute)
                                                   (reference-operator attribute))})))
                               args
                               ids))
                           {:euuid {:type 'UUID}}
                           uniques)
                search-arguments (cond->
                                   (conj as
                                         {:name "modified_on" 
                                          :type "timestamp"
                                          :active true}
                                         {:name "modified_by" 
                                          :type "user"
                                          :active true})
                                   (not-empty recursions)
                                   (as-> args
                                     (reduce
                                       (fn [args {:keys [from-label to-label]}]
                                         (cond-> args
                                           (not-empty to-label)
                                           (conj
                                             {:name to-label 
                                              :type :is_null_enum
                                              :active true})
                                           (not-empty from-label)
                                           (conj
                                             {:name from-label 
                                              :type :is_null_enum
                                              :active true})))
                                       args
                                       recursions)))]
            (cond->
              (assoc queries 
                     ;; GETTER
                     (csk/->camelCaseKeyword (str "get " ename))
                     {:type (entity->gql-object ename)
                      :args get-args 
                      :resolve 
                      (fn getter [context data _]
                        (try
                          (get-entity *db* (:euuid entity) data (executor/selections-tree context))
                          (catch Throwable e
                            (log/errorf e  "Couldn't resolve SYNC")
                            (throw e))))}
                     ;; SEARCH
                     (csk/->camelCaseKeyword (str "search " ename))
                     (let [args (reduce
                                  (fn [r {atype :type aname :name :as attribute}]
                                    (if (ignored-field-type? atype) r 
                                      (assoc 
                                        r
                                        (keyword (normalize-name aname))
                                        (attribute->type entity attribute))))
                                  {:_where {:type (entity->search-operator entity)}
                                   :_limit {:type 'Int}
                                   :_offset {:type 'Int}
                                   :_distinct {:type (entity->distinct-on-operator entity)}
                                   :_order_by {:type (entity->order-by-operator entity)}}
                                  search-arguments)]
                       (cond->
                         {:type (list 'list (entity->gql-object ename))
                          :resolve 
                          (fn search [context data _]
                            (try
                              (log-query context)
                              (lacinia.resolve/resolve-as
                                (search-entity *db* euuid data (executor/selections-tree context)))
                              (catch Throwable e
                                (log/error e  "Couldn't search dataset")
                                (throw e))))}
                         args (assoc :args args)))
                     ;; AGGREGATE
                     (csk/->camelCaseKeyword (str "aggregate " ename))
                     (let [args (reduce
                                  (fn [r {atype :type aname :name :as attribute}]
                                    (if (ignored-field-type? atype) r 
                                      (assoc r (keyword (normalize-name aname)) 
                                             (attribute->type entity attribute))))
                                  {:_where {:type (entity->search-operator entity)}
                                   :euuid {:type :UUIDQueryOperator}}
                                  search-arguments)]
                       (cond->
                         {:type (entity->aggregate-object entity) 
                          :resolve 
                          (fn aggregate [context data _]
                            (try
                              (log-query context)
                              (let [selection (executor/selections-tree context)]
                                (log/debugf
                                  "Aggregating entity\n%s"
                                  {:entity ename 
                                   :data data
                                   :selection selection})
                                (aggregate-entity *db* euuid data selection))
                              (catch Throwable e
                                (log/errorf e  "Couldn't resolve AGGREGATE")
                                (throw e))))}
                         args (assoc :args args))))
              ;; Add recursive getters
              (not-empty recursions)
              (as-> qs
                (reduce
                  (fn [qs' {l :to-label}]
                    (assoc qs'
                      ;;
                      (csk/->camelCaseKeyword (str "get " ename " tree" " by" " " l))
                      {:type (list 'list (entity->gql-object ename))
                       :args get-args 
                       :resolve 
                       (fn tree-getter [context data _]
                         (log/debugf
                           "Resolving recursive structure\n%s"
                           {:entity ename
                            :relation l
                            :data data})
                         (try
                           (log-query context)
                           (get-entity-tree 
                             *db*
                             euuid 
                             (:euuid data) 
                             (keyword l) 
                             (executor/selections-tree context))
                           (catch Throwable e
                             (log/error e "Couldn't resolve GET TREE")
                             (throw e))))}
                      ;;
                      (csk/->camelCaseKeyword (str "search " ename " tree by " l))
                      (let [args (reduce
                                   (fn [r {atype :type aname :name :as attribute}]
                                     (if (ignored-field-type? atype) r 
                                       (assoc 
                                         r 
                                         (keyword (normalize-name aname))
                                         (attribute->type entity attribute))))
                                   {:_where {:type (entity->search-operator entity)}
                                    :_limit {:type 'Int}
                                    :_offset {:type 'Int}
                                    :_order_by {:type (entity->order-by-operator entity)}}
                                   search-arguments)]
                        (cond->
                          {:type (list 'list (entity->gql-object ename))
                           :resolve 
                           (fn tree-search [context data _]
                             (try
                               (log-query context)
                               (let [selection (executor/selections-tree context)] 
                                 (log/debugf
                                   "Searching entity tree\n%s"
                                   {:name ename 
                                    :data data 
                                    :selection selection})
                                 (search-entity-tree *db* euuid (keyword (normalize-name l)) data selection))
                               (catch Throwable e
                                 (log/error e "Couldn't resolve SEARCH TREE")
                                 (throw e))))}
                          args (assoc :args args)))
                      ;;
                      (csk/->camelCaseKeyword (str "aggregate " ename " tree by " l))
                      (let [args (reduce
                                   (fn [r {atype :type aname :name :as attribute}]
                                     (if (ignored-field-type? atype) r 
                                       (assoc r (keyword (normalize-name aname)) 
                                              (attribute->type entity attribute))))
                                   {:_where {:type (entity->search-operator entity)}
                                    :euuid {:type :UUIDQueryOperator}}
                                   search-arguments)]
                        (cond->
                          {:type (entity->aggregate-object entity) 
                           :resolve 
                           (fn tree-aggregator [context data _]
                             (try
                               (log-query context)
                               (let [selection (executor/selections-tree context)]
                                 (log/debugf
                                   "Aggregating entity\n%s"
                                   {:entity ename 
                                    :data data
                                    :selection selection})
                                 (aggregate-entity-tree *db* euuid (keyword (normalize-name l)) data selection))
                               (catch Throwable e
                                 (log/error e "Coulnd't resolve AGGREGATE")
                                 (throw e))))}
                          args (assoc :args args)))))
                  qs
                  recursions)))))
        {}
        entities))))


(defn sync-mutation
  [{{euuid :euuid} :eywa/entity
    :keys [user]
    :as context} data _]
  (binding [core/*user* user] 
    (log-query context)
    (let [{row :euuid} (sync-entity *db* euuid (val (first data)))
          selection (executor/selections-tree context)
          ; _ (log/infof
          ;     :message "Getting entity"
          ;     :entity ename :row row :selection selection)
          value (get-entity *db* euuid {:euuid row} selection)]
      value)))


(defn sync-list-mutation
  [{:keys [user]
    {euuid :euuid} :eywa/entity
    :as context}
   data
   _]
  (log-query context)
  (binding [core/*user* user] 
    (let [rows (sync-entity *db* euuid (val (first data)))
          rows' (mapv :euuid rows)
          selection (executor/selections-tree context)
          value (search-entity 
                  *db* euuid
                  {:_where {:euuid {:_in rows'}}}
                  selection)]
      value)))


(defn stack-mutation
  [{:keys [user] :as context
    {euuid :euuid} :eywa/entity}
   data
   _]
  (log-query context)
  (binding [core/*user* user] 
    (let [{row :euuid} (stack-entity *db* euuid (val (first data)))
          selection (executor/selections-tree context)
          value (get-entity *db* euuid {:euuid row} selection)]
      value)))

(defn stack-list-mutation
  [{:keys [user]
    {euuid :euuid} :eywa/entity
    :as context}
   data
   _]
  (log-query context)
  (binding [core/*user* user] 
    (let [rows (stack-entity *db* euuid (val (first data)))
          rows' (mapv :euuid rows)
          selection (executor/selections-tree context)
          value (search-entity 
                  *db*  euuid
                  {:_where {:euuid {:_in rows'}}}
                  selection)]
      value)))


(defn slice-mutation
  [{:keys [user]
    :as context
    {euuid :euuid} :eywa/entity}
   data
   _]
  (let [args data
        selection (executor/selections-tree context)] 
    ; (log/trace
    ;   :message "Slicing entity" 
    ;   :euuid euuid :args args :selection selection
    ;   (with-out-str (pprint args)) 
    ;   (with-out-str (pprint selection)))
    (binding [core/*user* user]
      (slice-entity *db* euuid args selection))))


(defn delete-mutation
  [{{euuid :euuid} :eywa/entity :as context}
   data
   _]
  (try
    (log-query context)
    (delete-entity *db* euuid data)
    true 
    (catch Throwable e
      (log/error
        :exception e 
        :message "Couldn't delete entity")
      false)))


(defn purge-mutation
  [{:keys [user] :as context
    {euuid :euuid} :eywa/entity}
   data
   _]
  (log-query context)
  (let [args data
        selection (executor/selections-tree context)] 
    (binding [core/*user* user]
      (purge-entity *db* euuid args selection))))


(defn generate-lacinia-mutations [model]
  (let [entities (core/get-entities model)]
    (reduce
      (fn [mutations {ename :name :as entity}]
        (let [t (entity->gql-input-object ename)
              relations (core/focus-entity-relations model entity)
              to-relations (filter #(not-empty (:to-label %)) relations)] 
          (cond->
            (assoc mutations 
                   ;;
                   (csk/->camelCaseKeyword (str "sync " ename))
                   {:type (entity->gql-object ename)
                    :args {(keyword (normalize-name ename)) {:type (list 'non-null t)}}
                    :resolve 
                    (fn sync [context data value]
                      (sync-mutation (assoc context :eywa/entity entity) data value))}
                   ;; SYNC LIST
                   (csk/->camelCaseKeyword (str "sync " ename " List"))
                   {:type (list 'list (entity->gql-object ename))
                    :args {(keyword (normalize-name ename)) {:type (list 'list t)}} 
                    :resolve 
                    (fn syncList [context data value]
                      (try
                        (sync-list-mutation (assoc context :eywa/entity entity) data value)
                        (catch Throwable e
                          (log/error e "Couldn't resolve SYNC list")
                          (throw e))))}
                   ;;
                   (csk/->camelCaseKeyword (str "stack " ename " List"))
                   {:type (list 'list (entity->gql-object ename))
                    :args {(keyword (normalize-name ename)) {:type (list 'list t)}} 
                    :resolve 
                    (fn stackList [context data value]
                      (try
                        (stack-list-mutation (assoc context :eywa/entity entity) data value)
                        (catch Throwable e
                          (log/error e "Couldn't resolve STACK list")
                          (throw e))))}
                   ;;
                   (csk/->camelCaseKeyword (str "stack" ename))
                   {:type (entity->gql-object ename)
                    :args {(keyword (normalize-name ename)) {:type (list 'non-null t)}}
                    :resolve 
                    (fn stack [context data value]
                      (try
                        (stack-mutation (assoc context :eywa/entity entity) data value)
                        (catch Throwable e
                          (log/error e "Couldn't resolve STACK")
                          (throw e))))}
                   ;;
                   (csk/->camelCaseKeyword (str "delete " ename))
                   (let [uniques (-> entity :configuration :constraints :unique)
                         args (reduce
                                (fn [args ids]
                                  (reduce
                                    (fn [args id]
                                      (let [{aname :name :as attribute} (core/get-attribute entity id)]
                                        (assoc args (keyword (normalize-name aname)) 
                                               {:type (attribute-type->scalar entity attribute)})))
                                    args
                                    ids))
                                {:euuid {:type 'UUID}}
                                uniques)]
                     ; (log/debugf "Adding delete method for %s\n%s" ename args)
                     {:type 'Boolean
                      :args args 
                      :resolve (fn delete [context data value]
                                 (delete-mutation (assoc context :eywa/entity entity) data value))})
                   ;;
                   (csk/->camelCaseKeyword (str "purge " ename))
                   {:type (list 'list (entity->gql-object ename))
                    :args {:_where {:type (entity->search-operator entity)}}
                    :resolve 
                    (fn purge [context  data value]
                      (try
                        (purge-mutation (assoc context :eywa/entity entity) data value)
                        (catch Throwable e
                          (log/error e "Couldn't resolve purge")
                          (throw e))))})
            ;;
            (not-empty to-relations)
            (assoc
              ;; Slicing
              (csk/->camelCaseKeyword (str "slice " ename))
              {:type (entity->slice-object entity)
               :args {:_where {:type (entity->search-operator entity)}}
               :resolve 
               (fn slice [context  data value]
                 (try
                   (slice-mutation (assoc context :eywa/entity entity) data value)
                   (catch Throwable e
                     (log/error e "Couldn't resolve SLICE")
                     (throw e))))}))))
      {}
      entities)))


(def dataset-versions #uuid "d922edda-f8de-486a-8407-e62ad67bf44c")


(defn db->model [eywa]
  (let [entities (map
                   (fn [e]
                     (core/map->ERDEntity
                       (update e :attributes #(mapv core/map->ERDEntityAttribute %))))
                   (search-entity  
                     eywa du/dataset-entity  nil
                     {:euuid nil
                      :name nil
                      :locators nil
                      :constraints nil
                      :height nil
                      :width nil
                      :configuration nil
                      :attributes [{:selections
                                    {:euuid nil
                                     :seq nil
                                     :name nil
                                     :type nil
                                     :constraint nil
                                     :configuration nil
                                     :active nil}}]}))
        relations (map
                    (fn [r] 
                      (core/map->ERDRelation
                        (->
                          r
                          (update :from #(hash-map :euuid %))
                          (update :to #(hash-map :euuid %))
                          (clojure.set/rename-keys
                            {:from_label :from-label
                             :to_label :to-label}))))
                    (search-entity
                      eywa du/dataset-relation nil
                      {:euuid nil
                       :from nil
                       :to nil
                       :path nil
                       :from_label nil
                       :to_label nil
                       :cardinality nil}))
        versions (mapcat 
                   :versions
                   (search-entity 
                     eywa du/dataset nil
                     {:name nil
                      :versions [{:args {:_order_by [{:modified_on :desc}]
                                         :_where {:deployed {:_eq true}}
                                         :_limit 1}
                                  :selections {:name nil
                                               :configuration nil}}]}))
        configuration (apply core/deep-merge (map :configuration versions))]
    (reduce
      core/set-relation
      (reduce
        core/set-entity
        (core/map->ERDModel {:configuration configuration})
        entities)
      relations)))


(extend-type neyho.eywa.Postgres
  core/ModuleLifecycleProtocol
  (core/setup-module [this {:keys [model] :as version}]
    (try
      (let [deployed (core/deploy! this version)]
        (core/reload this)
        (when model 
          (when-let [hs (core/get-setup-handler model)]
            (require (symbol (namespace hs)))
            (if-let [h (resolve hs)]
              (h this)
              (throw
                (ex-info 
                  "Couldn't find setup-handler"
                  {:type :dataset/lifecycle
                   :handler (core/get-setup-handler model)})))))
        (core/add-to-deploy-history this (core/get-model this))
        deployed)
      (catch Throwable e
        ;; Don't auto destroy module... Handle tear-down-module explicitly!
        ; (dataset/tear-down-module this version)
        (log/errorf
          e "Couldn't load module %s@%s"
          (:name (:dataset version)) (:name version))
        (throw e))))
  (dataset/init-module [this {:keys [model] :as version}]
    (log/infof
      "Initializing model %s %s"
      (:name (:dataset version)) (:name version))
    (when model
      (when-let [hs (core/get-init-handler model)] 
        (log/infof "Initializing module %s" hs)
        (require (symbol (namespace hs)))
        (if-let [h (resolve hs)]
          (h this)
          (throw
            (ex-info 
              "Couldn't find init-handler"
              {:type :dataset/lifecycle
               :handler (core/get-init-handler model)}))))))
  (dataset/tear-down-module [this
                             {versions :versions
                              module-name :name}]
    (let [uber-model (when (not-empty versions) 
                       (reduce
                         (fn [old new]
                           (core/join-models old new))
                         (map :model versions)))]
      (when uber-model
        (core/unmount this {:model uber-model})
        (doseq [{:keys [model euuid] version-name :name} (reverse versions)]
          (log/infof "Destroying dataset version %s@%s" module-name version-name)
          (delete-entity this du/dataset-version {:euuid euuid})
          ;; Finaly try to execute model tear down handler
          (try
            (when-let [hs (core/get-tear-down-handler model)] 
              (log/infof "Executing teardown handler %s" hs)
              (require (symbol (namespace hs)))
              (if-let [h (resolve hs)]
                (h this)
                (throw
                  (ex-info
                    "Couldn't find tear-down-handler"
                    {:type :dataset/lifecycle
                     :handler (core/get-tear-down-handler model)}))))
            (catch Throwable e
              (log/errorf
                e  "Couldn't execute tear-down-handler %s@%s" module-name version-name))))
        (let [db-model (core/get-last-deployed this) 
              model (core/get-model this)
              final-model (with-meta db-model (meta model))]
          (dataset/save-model final-model)
          (let [global-model (core/reload this)]
            (core/add-to-deploy-history this global-model)
            ;; Restart core
            global-model)))))
  core/DatasetProtocol
  (core/deploy! [this {:keys [model]  :as version}] 
    (try
      (let [{model' :model :as dataset'} (core/mount this version)
            version'' (assoc
                        version
                        :model model 
                        :deployed true
                        :configuration (:configuration model)
                        :entities (core/get-entities model')
                        :relations 
                        (reduce
                          (fn [r relation]
                            (conj r
                              (->
                                relation
                                (update :from :euuid)
                                (update :to :euuid))))
                          []
                          (core/get-relations model')))]
        ;; Reload current projection so that you can sync data
        ;; for new model
        (core/reload this dataset')
        (log/info "Preparing model for DB")
        (sync-entity this dataset-versions version'')
        version'')
      (catch Throwable e
        (log/errorf e "Couldn't deploy dataset %s@%s" 
                    (:name version)
                    (:name (:dataset version)))
        (throw e))))
  (core/recall! [this version]
    (core/unmount this version)
    (let [db-model (db->model this)
          model (core/get-model this)
          final-model (with-meta
                        db-model
                        (meta model))]
      (delete-entity this du/dataset-version {:euuid (:euuid version)})
      (dataset/save-model final-model) 
      (core/reload this)))
  (core/get-model
    [_] 
    (dataset/deployed-model))
  (core/reload
    ([this]
     (let [model' (db->model this) 
           schema (model->schema model')
           model'' (->
                     model'
                     (with-meta {:dataset/schema schema}))]
       (dataset/save-model model'')
       (try
         (lacinia/add-shard ::datasets (fn [] (lacinia/generate-lacinia-schema this)))
         (catch Throwable e
           (log/error e "Couldn't add lacinia schema shard")))
       model''))
    ([this {:keys [model]}]
     (let [model' (core/join-models
                    (or
                      (core/get-model this)
                      (core/map->ERDModel nil))
                    model)
           schema (model->schema model')
           model'' (->
                     model'
                     (with-meta {:dataset/schema schema}))]
       (dataset/save-model model'')
       (try
         (lacinia/add-shard ::datasets (fn [] (lacinia/generate-lacinia-schema this)))
         (catch Throwable e
           (log/error e "Couldn't add lacinia schema shard")))
       model'')))
  (core/mount
    [this {model :model :as version}]
    (log/debugf "Mounting dataset version %s@%s" (:name version) (get-in version [:dataset :name]))
    (with-open [con (jdbc/get-connection (:datasource *db*))]
      (let [current-model (or
                            (core/get-model this)
                            (core/map->ERDModel nil))
            projection (core/project current-model model)]
        (binding [*model* current-model]
          (transform-database
            con projection
            (core/deep-merge
              (assoc
                (:configuration current-model)
                :who/table
                (entity->table-name
                  (some 
                    #(core/get-entity % au/user)
                    [current-model projection])))
              (:configuration model))))
        (assoc version :model (core/join-models current-model model)))))
  (core/unmount
    [this {:keys [model]}]
    ;; USE Global model to reference true DB state
    (let [global (core/get-model this)] 
      (with-open [con (jdbc/get-connection (:datasource *db*))]
        (doseq [relation (core/get-relations model)
                :let [{:keys [from to] :as relation} (core/get-relation global (:euuid relation))
                      sql (format "drop table if exists \"%s\"" (relation->table-name relation))]]
          (try
            (execute-one! con [sql])
            (log/tracef
              "Removing relation from %s to %s\n%s"
              (:name from)
              (:name to)
              sql)
            (delete-entity this du/dataset-relation (select-keys relation [:euuid]))
            (catch Throwable e
              (log/errorf e "Couldn't remove table %s" (relation->table-name relation)))))
        (doseq [entity (core/get-entities model)
                :let [entity (core/get-entity global (:euuid entity))]
                :when (some? entity)]
          (try
            (let [sql (format "drop table if exists \"%s\"" (entity->table-name entity))
                  enums-sql (drop-entity-enums-ddl entity)] 
              (log/tracef "Removing entity %s\n%s" (:name entity) sql)
              (execute-one! con [sql])
              (delete-entity this du/dataset-entity (select-keys entity [:euuid]))
              (when (not-empty enums-sql) 
                (log/trace "Removing %s enum types: %s" (:name entity) enums-sql)
                (execute-one! con [enums-sql])))
            ; (let [sql (format "drop table if exists \"%s_eid_seq\"" (entity->table-name entity))
            ; (log/tracef "Removing entity PK sequence:\n %s" (:name entity) sqlt)
            ; (execute-one! con [sqls])])
            (catch Throwable e
              (log/errorf e "Couldn't remove table %s" (entity->table-name entity)))))))
    (core/reload this))
  (core/setup
    ([this]
     (let [admin (postgres/admin-from-env)
           db (postgres/create-db admin (:db this))]
       ;; Set this new database as default db
       (alter-var-root #'neyho.eywa.db/*db* (constantly db))
       ;;
       (log/infof "Initializing tables for host\n%s" (pr-str db))
       (core/create-deploy-history db)
       (log/info "Created __deploy_history")
       (as-> (<-transit (slurp (io/resource "dataset/aaa.edm"))) model 
         (core/mount db model)
         (core/reload db model))
       (dataset/stack-entity au/permission administration/permissions)
       (log/info "Mounted aaa.edm dataset")
       (binding [core/*return-type* :edn] 
         (dataset/sync-entity au/user *EYWA*)
         (dataset/bind-service-user #'neyho.eywa.data/*EYWA*))
       (log/info "*EYWA* user created")
       (binding [core/*user* (:_eid *EYWA*)]
         (as-> (<-transit (slurp (io/resource "dataset/dataset.edm"))) model
           (core/mount db model)
           (core/reload db model))
         (log/info "Mounted dataset.edm dataset")
         (dataset/stack-entity au/permission dataset/permissions)
         (dataset/load-role-schema)
         (log/info "Deploying AAA dataset")
         (core/deploy! db (<-transit (slurp (io/resource "dataset/aaa.edm"))))
         (log/info "Deploying Datasets dataset")
         (core/deploy! db (<-transit (slurp (io/resource "dataset/dataset.edm"))))
         (log/info "Reloading")
         (core/reload db)
         (log/info "Adding deployed model to history")
         (core/add-to-deploy-history db (core/get-model db)))))
    ([this options]
     (core/setup this)
     (administration/setup options)))
  (core/tear-down
    [this]
    (let [admin (postgres/admin-from-env)]
      (postgres/drop-db admin (:db this))))
  (core/get-last-deployed
    ([this]
     (core/get-last-deployed this nil))
    ([_ offset]
     (with-open [connection (jdbc/get-connection (:datasource *db*))]
       (when-let [m (n/execute-one!
                      connection 
                      [(cond->
                         "select model from __deploy_history order by deployed_on desc"
                         offset (str " offset " offset))])]
         (-> m :model <-transit)))))
  (core/create-deploy-history [_]
    (with-open [connection (jdbc/get-connection (:datasource *db*))]
      (n/execute-one!
        connection
        [(format
           "create table __deploy_history (
           \"deployed_on\" timestamp not null default localtimestamp,
           \"model\" text)")])))
  (core/add-to-deploy-history [_ model]
    (with-open [connection (jdbc/get-connection (:datasource *db*))]
      (n/execute-one!
        connection
        ["insert into __deploy_history (model) values (?)"
         (->transit model)])))
  ;;
  lacinia/EYWAGraphQL
  (generate-lacinia-schema [this]
    (binding  [*eywa* this]
      (let [model (core/get-model this)
            service-definition {:enums (generate-lacinia-enums model) 
                                :objects (assoc
                                           (generate-lacinia-objects model)
                                           :Mutation {:fields (generate-lacinia-mutations model)}
                                           :Query {:fields (generate-lacinia-queries model)})
                                :input-objects (generate-lacinia-input-objects model)}
            schema service-definition]
        (merge schema {:scalars neyho.eywa.lacinia/scalars})))))


;; Version compatibility fix
(defn fix-on-delete-set-null-to-references
  []
  (binding [*model* (dataset/deployed-model)]
    (let [model (dataset/deployed-model)
          entities (core/get-entities model)]
      (reduce
        (fn [r entity]
          (let [user-table-name (user-table)
                entity-table (entity->table-name entity)
                modified_by (str entity-table \_ "modified_by_fkey")
                refered-attributes (filter
                                     (comp
                                       #{"user" "group" "role"}
                                       :type)
                                     (:attributes entity))
                ;;
                current-result
                (conj r
                      (format "alter table \"%s\" drop constraint \"%s\"" entity-table modified_by)
                      (format
                        "alter table \"%s\" add constraint \"%s\" foreign key (modified_by) references \"%s\"(_eid) on delete set null"
                        entity-table modified_by user-table-name))]
            (reduce
              (fn [r {attribute-name :name
                      attribute-type :type}]
                (let [attribute-column (normalize-name attribute-name)
                      constraint-name (str entity-table \_ attribute-column "_fkey")
                      refered-table (case attribute-type
                                      "user" (user-table)
                                      "group" (group-table)
                                      "role" (role-table))]
                  (conj
                    r
                    (format "alter table \"%s\" drop constraint %s" entity-table constraint-name)
                    (format
                      "alter table \"%s\" add constraint \"%s\" foreign key (%s) references \"%s\"(_eid) on delete set null"
                      entity-table constraint-name attribute-column refered-table))))
              current-result
              refered-attributes)))
        []
        entities))))


(defn get-relation-indexes
  [relation]
  (let [table (relation->table-name relation)]
    (with-open [conn (jdbc/get-connection (:datasource *db*))]
      (let [metadata (.getMetaData conn)
            indexes (.getIndexInfo metadata nil "public" table false false)]
        (loop [col indexes
               result []]
          (let [idx (.next col)]
            (if-not idx result
              (recur col (conj result
                               {:index (.getString col "INDEX_NAME")
                                :column (.getString col "COLUMN_NAME")
                                :type (if (.getBoolean col "NON_UNIQUE") :non-unique :unique)})))))))))



(defn fix-on-reference-indexes
  []
  (let [statements (binding [*model* (dataset/deployed-model)]
                     (let [model (dataset/deployed-model)
                           relations (core/get-relations model)]
                       (reduce
                         (fn [r {:keys [from to] :as relation}]
                           (let [table (relation->table-name relation)
                                 from-field (entity->relation-field from)
                                 to-field (entity->relation-field to)
                                 indexes (set (map :index (get-relation-indexes relation)))]
                             (cond->
                               r
                               (not (indexes (str table \_ "fidx")))
                               (conj (format "create index %s_fidx on \"%s\" (%s);" table table from-field))
                               ;;
                               (not (indexes (str table \_ "tidx")))
                               (conj (format "create index %s_tidx on \"%s\" (%s);" table table to-field)))))
                         []
                         relations)))]
    (with-open [con (jdbc/get-connection (:datasource *db*))]
      (doseq [statement statements]
        (try
          (execute-one! con [statement])
          (println statement)
          (catch Throwable ex
            (println "EX: " ex)
            (println statement)))))))
