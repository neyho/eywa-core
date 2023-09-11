(ns build
  (:require
    [clojure.tools.build.api :as b]))


(def lib 'neyho.eywa/core)
(def version "0.1.0")
(def class-dir "target/classes")
(def basis 
  (b/create-basis
    {:project "deps.edn"}))
(def uber-file (format "target/eywa.%s.jar" version))


(defn clean [_]
  (b/delete {:path "target"}))


(def exclude-uber-basis-pattern #"src.*$")


(def uber-basis
  (update basis :libs
          (fn [libs]
            (reduce-kv
              (fn [libs lib context]
                (if (contains? context :local/root)
                  (update-in libs [lib :paths]
                             (fn [paths]
                               (vec
                                 (remove
                                   (fn [path]
                                     (re-find exclude-uber-basis-pattern path))
                                   paths))))
                  libs))
              libs
              libs))))


(defn uber [& _]
  (println "Creating uberjar file")
  (b/uber
    {:class-dir class-dir
     :uber-file uber-file
     :main 'example.main
     :manifest {"Application-Name" "EYWA"}
     :basis uber-basis}))


(defn release
  [& _]
  (println "Compiling backend")
  (b/compile-clj
    {:basis basis
     :src-dirs ["src/clj" "resources"]
     :ns-compile ['example.main]
     :class-dir class-dir})
  (uber))
