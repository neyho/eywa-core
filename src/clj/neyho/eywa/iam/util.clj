(ns neyho.eywa.iam.util
  (:require
   [clojure.java.io :as io]
   [neyho.eywa.json :refer [<-json]]
   [neyho.eywa.dataset :as dataset]
   [neyho.eywa.iam.uuids :as iu]))

(defn import-data
  [path entity]
  (when-some [data (<-json (slurp (io/resource path)) :keyfn keyword)]
    (dataset/stack-entity entity data)))

(defn import-role [path] (import-data path iu/user-role))
(defn import-api [path] (import-data path iu/api))
(defn import-app [path] (import-data path iu/app))

(comment
  ()
  (<-json (slurp (io/resource "exports/app_eywa_frontend.json")) :keyfn keyword)
  (import-role "roles/role_dataset_developer.json"))
