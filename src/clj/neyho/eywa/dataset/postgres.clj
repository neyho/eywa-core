(ns neyho.eywa.dataset.postgres
  (:require
    clojure.set
    clojure.data
    clojure.string
    [clojure.java.io :as io]
    ; [clojure.pprint :refer [pprint]]
    next.jdbc.date-time
    [next.jdbc :as jdbc]
    [clojure.tools.logging :as log]
    [neyho.eywa.dataset.core :as core]
    [neyho.eywa.dataset :as dataset]
    [neyho.eywa.transit
     :refer [<-transit ->transit]]
    [neyho.eywa.db.postgres.next  :as n
     :refer [execute! execute-one!]]
    [neyho.eywa.db.postgres :as postgres]
    [neyho.eywa.dataset.sql.naming
     :refer [normalize-name 
             column-name
             relation->table-name
             entity->relation-field
             entity->table-name]]
    [neyho.eywa.db
     :refer [*db*
             sync-entity
             search-entity
             delete-entity]]
    [neyho.eywa.dataset.lacinia
     :refer [normalized-enum-value]]
    [neyho.eywa.iam.uuids :as iu]
    [neyho.eywa.iam.access.context :refer [*user*]]
    [neyho.eywa.dataset.postgres.query :as query]
    [neyho.eywa.lacinia :as lacinia]
    [neyho.eywa.data :refer [*EYWA*]]
    [neyho.eywa.iam :as iam]
    [neyho.eywa.iam.util
     :refer [import-role
             import-api
             import-app]]
    [neyho.eywa.dataset.uuids :as du]))


;; TODO - remove this... probably not necessary
(defonce ^:dynamic *model* nil)


(defn user-table []
  (if-some [e (core/get-entity *model* iu/user)]
    (entity->table-name e)
    (throw (Exception. "Coulnd't find user entity"))))


(defn group-table []
  (if-some [e (core/get-entity *model* iu/user-group)]
    (entity->table-name e)
    (throw (Exception. "Coulnd't find group entity"))))


(defn role-table []
  (if-some [e (core/get-entity *model* iu/user-role)]
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


; (defn generate-enum-type-ddl [enum-name values]
;   (format
;     "do $$\nbegin\nif not exists ( select 1 from pg_type where typname='%s') then create type \"%s\" as enum%s;\nend if;\nend\n$$;"
;     enum-name
;     enum-name 
;     (when (not-empty values)
;       (str " (" 
;            (clojure.string/join 
;              ", "
;              (map (comp #(str \' % \') normalized-enum-value :name) values))
;            \)))))


(defn generate-enum-type-ddl [enum-name values]
  (format
    "create type \"%s\" as enum%s;"
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
      (if (= f t)
        (clojure.string/join
          ",\n " 
          [(str to-field " bigint not null references \"" to-table "\"(_eid) on delete cascade")
           (str "unique(" to-field ")")])
        (clojure.string/join
          ",\n " 
          [(str from-field " bigint not null references \"" from-table "\"(_eid) on delete cascade")
           (str to-field " bigint not null references \"" to-table "\"(_eid) on delete cascade")
           (str "unique(" from-field "," to-field ")")])))))


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
      ["update modeling_eywa_datasets_entities_attributes set active = false where entity=? and id=ANY(?)" id (long-array as)])
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
      ;;
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
      ;;
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
            ;; If type is one of
            ;; Set all current values to null
            (and dt
                 (#{"avatar"} dt)
                 (#{"json"} type))
            (conj
              (do
                (log/debugf "Setting all avatar values to NULL")
                (format "update \"%s\" set %s = NULL" old-table column)))
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
                  (= "json" type) (str " using(" column "::jsonb)")
                  (= "transit" type) (str " using(" column "::text)")
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
                              (conj r (str "     when '" old-name "' then '" old-name "'"))))
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
        _ (log/tracef "Transforming relation\ndiff=%s\nfrom=%s\nto=%s" diff (pr-str from) (pr-str to))
        from-diff (:from diff)
        to-diff (:to diff)
        old-from (core/suppress from) 
        old-to (core/suppress to)
        old-relation (core/suppress relation) 
        _ (do
            (def relation relation)
            (def old-relation old-relation)
            (def old-to old-to)
            (def old-from old-from)
            (def to-diff to-diff)
            (def from-diff from-diff)
            (:euuid old-relation))
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



(defn column-exists?
  [tx table column]
  (let [sql (str
              "select exists ("
              "select 1 from pg_attribute where attrelid = '" table "'::regclass"
              " and attname = '" column "'"
              " and attnum > 0"
              " and not attisdropped"
              ");")]
    (:exists (execute-one! tx [sql]))))


(comment
  (do (def tx nil) (def table "ict_category") (def column "parent"))
  (with-open [con (jdbc/get-connection (:datasource *db*))]
    (column-exists? con "ict_category" "parent")))


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
      (let [entity-priority {iu/user -100}
            ne (sort-by
                 (fn [{:keys [euuid]}]
                   (get entity-priority euuid 0))
                 ne)]
        (when (not-empty ne)
          (log/infof
            "Generating new entities... %s"
            (clojure.string/join ", " (map :name ne))))
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
                      table "modified_by" amt)]
            (log/tracef "Adding table audit reference[who] column\n%s" sql)
            (execute-one! tx [sql]))
          (let [sql (format 
                      "alter table \"%s\" add column \"%s\" timestamp not null default localtimestamp"
                      table "modified_on")] 
            (log/tracef "Adding table audit reference[when] column\n%s" sql)
            (execute-one! tx [sql]))))
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
               diff :diff
               euuid :euuid} crr
              :let [table (entity->table-name e)
                    ; _ (log/debugf "RECURSIVE RELATION\n%s" diff)
                    previous-column (when-some [label (not-empty (:to-label diff))]
                                      (column-name label))]]
        (when-not (and (some? tl) (not-empty tl)) 
          (throw
            (ex-info 
              (str "Can't change recursive relation for entity " tname " that has empty label")
              {:entity e
               :relation {:euuid euuid
                          :label (:to-label diff)}
               :type ::core/error-recursive-no-label})))
        (if (empty? previous-column)
          (do
            (log/debugf
              "Previous deploy didn't have to label for recursive relation %s at entity %s"
              euuid tname)
            (when tl
              (when-not (column-exists? tx table tl)
                (let [sql (format
                            "alter table %s add %s bigint references \"%s\"(_eid) on delete cascade"
                            table tl table)]
                  (log/debug "Creating recursive relation for entity %s\n%s" tname sql)
                  (execute-one! tx [sql])))))
          ;; Apply changes
          (when diff
            (let [sql (format
                        "alter table %s rename column %s to %s"
                        table previous-column (column-name tl))]
              (log/debugf "Updating recursive relation for entity %s\n%s" tname sql)
              (execute-one! tx [sql])))))
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
                                    (assoc f :postgres/reference iu/user)
                                    ;;
                                    "group"
                                    (assoc f :postgres/reference iu/user-group)
                                    ;;
                                    "role"
                                    (assoc f :postgres/reference iu/user-role)
                                    ;;
                                    f))))
                       {:audit/who {:key :modified_by
                                    :type "user"
                                    :postgres/reference iu/user}}
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
                                       {:relation euuid
                                        :from (:euuid from)
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


(def dataset-versions #uuid "d922edda-f8de-486a-8407-e62ad67bf44c")


(defn db->model
  [this]
  (let [entities (keep
                   (fn [{:keys [attributes] :as e}]
                     (when-let [attributes (not-empty
                                             (map
                                               #(core/map->ERDEntityAttribute %)
                                               attributes))]
                       (core/map->ERDEntity (assoc e :attributes attributes))))
                   (search-entity
                     this du/dataset-entity  nil
                     {:_eid nil
                      :euuid nil
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
                      this du/dataset-relation nil
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
                     this du/dataset nil
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
  core/DatasetProtocol
  (core/deploy! [this {:keys [model]  :as version}] 
    (try
      (let [dataset' (core/mount this version)
            version'' (assoc
                        version
                        :model model 
                        :deployed true
                        :entities (core/get-entities model)
                        :relations 
                        (reduce
                          (fn [r relation]
                            (conj r
                                  (->
                                    relation
                                    (update :from :euuid)
                                    (update :to :euuid))))
                          []
                          (core/get-relations model)))]
        ;; Reload current projection so that you can sync data
        ;; for new model
        (core/reload this dataset')
        (log/info "Preparing model for DB")
        (sync-entity this dataset-versions version'')
        (core/add-to-deploy-history this (core/get-model this))
        version'')
      (catch Throwable e
        (log/errorf e "Couldn't deploy dataset %s@%s" 
                    (:name version)
                    (:name (:dataset version)))
        (throw e))))
  ;;
  (core/recall! [this version]
    (core/unmount this version)
    (let [db-model (db->model this)
          model (core/get-model this)]
      (delete-entity this du/dataset-version {:euuid (:euuid version)})
      (dataset/save-model db-model) 
      (query/deploy-schema (model->schema model))
      (core/reload this)))
  ;;
  (core/destroy! [this
                  {versions :versions
                   module-name :name}]
    (let [uber-model (when (not-empty versions) 
                       (reduce
                         (fn [old new]
                           (core/join-models old new))
                         (map :model versions)))]
      (when uber-model
        (core/unmount this {:model uber-model})
        (doseq [{:keys [euuid] version-name :name} (reverse versions)]
          (log/infof "Destroying dataset version %s@%s" module-name version-name)
          (delete-entity this du/dataset-version {:euuid euuid}))
        (let [db-model (core/get-last-deployed this) 
              model (core/get-model this)]
          (dataset/save-model db-model)
          (query/deploy-schema (model->schema model))
          (let [global-model (core/reload this)]
            (core/add-to-deploy-history this global-model)
            ;; Restart core
            global-model)))))
  (core/get-model
    [_] 
    (dataset/deployed-model))
  (core/reload
    ([this]
     (comment
       (def this neyho.eywa.db/*db*))
     (let [model' (db->model this)
           schema (model->schema model')]
       (dataset/save-model model')
       (query/deploy-schema schema)
       (try
         (lacinia/add-shard ::datasets (fn [] (lacinia/generate-lacinia-schema this)))
         (catch Throwable e
           (log/error e "Couldn't add lacinia schema shard")))
       model'))
    ([this {:keys [model]}]
     (let [model' (core/join-models
                    (or
                      (core/get-model this)
                      (core/map->ERDModel nil))
                    model)
           schema (model->schema model')]
       (query/deploy-schema schema)
       (dataset/save-model model')
       (try
         (lacinia/add-shard ::datasets (fn [] (lacinia/generate-lacinia-schema this)))
         (catch Throwable e
           (log/error e "Couldn't add lacinia schema shard")))
       model')))
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
                    #(core/get-entity % iu/user)
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
                :let [{:keys [attributes] :as entity} (core/get-entity global (:euuid entity))]
                :when (some? entity)]
          (try
            (let [sql (format "drop table if exists \"%s\"" (entity->table-name entity))
                  enums-sql (drop-entity-enums-ddl entity)] 
              (log/tracef "Removing entity %s\n%s" (:name entity) sql)
              (execute-one! con [sql])
              ;; If SQL runs without error than this entity doesn't have
              ;; is good to delete from  DB, as well as all attributes that
              ;; are linked to this entity
              (delete-entity this du/dataset-entity (select-keys entity [:euuid]))
              (doseq [{:keys [euuid]} attributes]
                (delete-entity this du/dataset-entity-attribute euuid))
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
     (comment
       (def this (postgres/from-env))
       (def db neyho.eywa.db/*db*)
       (def admin (postgres/admin-from-env))
       (def db (postgres/create-db admin (:db this)))
       )
     (let [admin (postgres/admin-from-env)
           db (postgres/create-db admin (:db this))]
       ; (def admin (postgres/admin-from-env))
       ; (def db (postgres/connect (postgres/admin-from-env)))
       ;; Set this new database as default db
       (alter-var-root #'neyho.eywa.db/*db* (constantly db))
       ;;
       (log/infof "Initializing tables for host\n%s" (pr-str db))
       (core/create-deploy-history db)
       (log/info "Created __deploy_history")
       (as-> (<-transit (slurp (io/resource "dataset/iam.json"))) model 
         (core/mount db model)
         (core/reload db model))
       ; (dataset/stack-entity iu/permission iam/permissions)
       (log/info "Mounted iam.json dataset")
       (binding [core/*return-type* :edn]
         (dataset/sync-entity iu/user *EYWA*)
         (dataset/bind-service-user #'neyho.eywa.data/*EYWA*))
       (log/info "*EYWA* user created")
       (binding [*user* (:_eid *EYWA*)]
         (comment
           (alter-var-root #'*user* (fn [_] (:_eid *EYWA*))))
         (as-> (<-transit (slurp (io/resource "dataset/dataset.json"))) model
           (core/mount db model)
           (core/reload db model))
         (log/info "Mounted dataset.json dataset")
         ; (dataset/stack-entity iu/permission dataset/permissions)
         ; (dataset/load-role-schema)
         ;;
         (log/info "Deploying IAM dataset")
         (core/deploy! db (<-transit (slurp (io/resource "dataset/iam.json"))))
         ;;
         (log/info "Deploying Datasets dataset")
         (core/deploy! db (<-transit (slurp (io/resource "dataset/dataset.json"))))
         ;;
         ; (log/info "Mounted oauth.json dataset")
         ; (core/deploy! db (<-transit (slurp (io/resource "dataset/oauth.json"))))
         ;;
         (log/info "Reloading")
         (core/reload db)
         (import-app "exports/app_eywa_frontend.json")
         (import-api "exports/api_eywa_graphql.json")
         (doseq [role ["exports/role_dataset_developer.json"
                       "exports/role_dataset_modeler.json"
                       "exports/role_dataset_explorer.json"
                       "exports/role_iam_admin.json"
                       "exports/role_iam_user.json"]]
           (import-role role))
         (log/info "Adding deployed model to history")
         (core/add-to-deploy-history db (core/get-model db)))))
    ([this options]
     (core/setup this)
     (iam/setup options)))
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
         (let [model (-> m :model <-transit)
               clean-model (reduce
                             (fn [m entity]
                               (if (empty? (:attributes entity))
                                 (core/remove-entity m entity)
                                 m))
                             model
                             (core/get-entities model))
               clean-schema (model->schema clean-model)]
           (with-meta clean-model {:dataset/schema clean-schema}))))))
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
  (generate-lacinia-schema [_]
    (neyho.eywa.dataset.lacinia/generate-lacinia-schema)))


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
