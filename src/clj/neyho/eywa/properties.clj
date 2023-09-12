(ns neyho.eywa.properties
  (:require
    environ.core
    [clojure.java.io :as io]
    [clojure.string :as str]))


(defn read-properties
  ([] (when-some [p (io/resource "eywa.properties")]
        (read-properties p)))
  ([f]
   (let [content (slurp f)
         lines (clojure.string/split-lines content)
         properties-lines (keep
                            (fn [line]
                              (not-empty
                                (str/replace
                                  (str/trim line)
                                  #"#.*" "")))
                            lines)
         bindings (map #(str/split % #"\s*=\s*") properties-lines)]
     bindings)))


(defn load-properties
  ([] (load-properties (io/resource "eywa.properties")))
  ([reader]
   (when reader
     (doseq [[name value] (read-properties reader)]
       (System/setProperty name value))
     (remove-ns 'environ.core)
     (dosync (alter @#'clojure.core/*loaded-libs* disj 'environ.core))
     (require 'environ.core))))
