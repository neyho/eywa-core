(ns neyho.eywa.dataset.sql.naming
  (:require
   [clojure.string :as str]
   [neyho.eywa.dataset.core :as core]
   [neyho.eywa.iam :refer [hash-uuid]]))

(def npattern #"[\s\_\-\.\$\[\]\{\}\#]+")

(def
  ^{:doc "Name normalization function"}
  normalize-name
  (memoize
   (fn
     [n]
     (clojure.string/lower-case
      (clojure.string/replace n npattern "_")))))

(defn column-name [n]
  (str \" (normalize-name n) \"))

(def table-name
  (memoize
   (fn [n]
     (let [n' (str
               (clojure.string/lower-case
                (clojure.string/replace n npattern "_")))]
       (cond-> n'
         (> (count n') 63) (subs 0 64))))))

(def
  ^{:doc "Returns DB table name for given entity"}
  entity->table-name
  (comp table-name :name))

(def relation-field
  (memoize
   (fn [table-name]
     (let [f (str table-name "_id")]
       (cond-> f
         (> (count f) 63) (subs 0 64))))))

(defn entity->relation-field
  [entity]
  (as-> (entity->table-name entity) field-name
    (if (core/cloned? entity)
      (str field-name "_cloned")
      field-name)
    (relation-field field-name)))

(def
  ^{:doc "Returns relation DB table name"}
  relation->table-name
  (letfn [(short-table [name]
            (str/join
             "_"
             (map
              (comp str/lower-case #(re-find #"\w{2,4}" %))
              (str/split name #"[\_\-\s]"))))]
    (fn [relation]
      (let [{inverted? :dataset.relation/inverted?} (meta relation)
            ;;
            {id :euuid f :from t :to}
            (if inverted?
              (core/invert-relation relation)
              relation)
            rn (str
                (short-table (:name f))
                \_
                (hash-uuid id)
                \_
                (short-table (:name t)))]
        (cond-> rn
          (> (count rn) 63) (subs 0 64))))))

(defprotocol SQLNameResolution
  (table [this euuid] "Returns table name based on input euuid")
  (relation [this table label] "Returns relation table name based on label, that can be keyword or UUID")
  (related-table [this table label] "Returns related table name based on label, that can be keyword or UUID")
  (relation-from-field [this table label] "Returns name for relation field that is directed from current entity")
  (relation-to-field [this table label] "Returns name for relation field that is directed towards referenced entity"))
