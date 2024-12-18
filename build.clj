(ns build
  (:require
   [clojure.string :as str]
   [clojure.pprint :refer [pprint]]
   [clojure.java.io :as io]
   [clojure.tools.build.api :as b]
   [clojure.java.shell :refer [sh]]
   [cognitect.aws.client.api :as aws]))

(def lib 'neyho/eywa)
(def class-dir "target/classes")
(def basis
  (b/create-basis
   {:project "deps.edn"
    :aliases [:prod]}))

(def version
  (let [{:keys [out err]} (sh "clj" "-M:dev" "-m" "neyho.eywa.core" "version")
        version (some not-empty [out err])]
    (when (empty? version)
      (throw
       (ex-info "Couldn't resolve version"
                {:command "clj -M -m neyho.eywa.core version"})))
    (re-find #"\d+\.\d+\.\d+" version)))

(def uber-file (format "target/example.%s.jar" version))

(println "UBER FILE: " uber-file)

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

(defn compile-backend
  [& _]
  (println "Compiling backend")
  (b/compile-clj
   {:basis basis
    :src-dirs ["src/clj" "resources" "src/prod"]
    :ns-compile ['neyho.eywa.core]
    :class-dir class-dir})
  (b/copy-dir
   {:src-dirs ["resources"]
    :target-dir class-dir})
  (b/copy-file
   {:src "src/prod/logback.xml"
    :target (str class-dir "/logback.xml")}))

(defn copy-frontend
  [& _]
  (b/process
   {:command-args ["clj" "-X:css"]})
  #_(b/copy-dir
     {:src-dirs ["frontend/dist/graphiql"]
      :target-dir (str class-dir "/graphiql")})
  #_(b/copy-dir
     {:src-dirs ["frontend/dist/eywa"]
      :target-dir (str class-dir "/eywa")})
  (b/copy-dir
   {:src-dirs ["frontend/dist/oauth"]
    :target-dir (str class-dir "/oauth")}))

(defn uber [& _]
  (println "Creating uberjar file")
  (b/uber
   {:class-dir class-dir
    :uber-file uber-file
    :main 'neyho.eywa.core
    :manifest {"Application-Name" "EYWA"}
     ;; Exclude source code
    :basis uber-basis}))

(defn docs
  [& _]
  (b/delete {:path "../docs/eywa"})
  (b/process
   {:command-args ["npm" "run" "build"]
    :dir "../docs"})
  (b/copy-dir
   {:src-dirs ["../docs/eywa/docs"]
    :target-dir "target/classes/eywa/docs"}))

(defn release
  [& _]
  (println "Cleaning: " class-dir)
  (clean nil)
  (println "Writting pom.xml")
  (b/write-pom {:class-dir class-dir
                :lib lib
                :version version
                :basis basis
                :src-dirs ["src/clj" "src/prod"]})
  (compile-backend)
  (copy-frontend)
  ; (docs)
  (uber))

(defn upload [& _]
  (let [s3 (aws/client {:api :s3})
        s3-path (str "eywa_core/jar/" version ".jar")
        bucket "eywa.public"]
    (println (format "Uploading file: %s to S3: %s:%s" uber-file bucket s3-path))
    (with-open [file (io/input-stream (io/file uber-file))]
      (pprint
       (aws/invoke
        s3 {:op :PutObject
            :request {:ACL "public-read"
                      :Bucket bucket
                      :Key s3-path
                      :Body file}})))))

(defn all [& _]
  (release nil)
  (upload nil))
