(ns neyho.eywa.dataset.sql.compose
  (:require
    [clojure.string :as str]
    [next.jdbc :as jdbc]
    [neyho.eywa.db :refer [*db*]]
    [neyho.eywa.dataset.sql.naming :as n]
    [neyho.eywa.db.postgres.next :as postgres]
    [neyho.eywa.dataset.core :refer [*return-type*]]))


(defn table [x] (n/table *db* x))
(defn relation [t x] (n/relation *db* t x))
(defn relation-from-field [t x] (n/relation-from-field *db* t x))
(defn relation-to-field [t x] (n/relation-to-field *db* t x))


(defn ml
  "Multiline... Joins lines with newline. Removes empty lines"
  [& lines]
  (str/join "\n" (remove empty? lines)))


(defn mwa
  "Expects bindings of form [query-part arg1 arg2 arg3] and returns
  final vector with first argument as final query in multiline form and
  rest of arguments that were provided in bindings"
  [& bindings]
  (reduce
    (fn [[query :as result] [part & data]]
      (if (empty? part) result
        (as-> result result
          (assoc result 0 (str query (when query \newline) part))
          (if (empty? data) result
            (into result data)))))
    []
    bindings))


(defn lwa
  "Expects bindings of form [query-part arg1 arg2 arg3] and returns
  final vector with first argument as space separated parts of query-parts
  and rest of arguments that were provided in bindings"
  [& bindings]
  (reduce
    (fn [[query :as result] [part & data]]
      (if (empty? part) result
        (as-> result result
          (assoc result 0 (str query " " part))
          (if (empty? data) result
            (into result data)))))
    []
    bindings))


(defn and-join [condition text]
  (if condition (str "and " text)
    text))

(defn or-join [condition text]
  (if condition (str "or " text)
    text))


(defn execute!
  [query-binding]
  (with-open [con (jdbc/get-connection (:datasource *db*))]
    (postgres/execute! con query-binding *return-type*)))
