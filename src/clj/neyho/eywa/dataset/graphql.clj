(ns neyho.eywa.dataset.graphql
  (:require
   [clojure.core.async :as async]
   [clojure.tools.logging :as log]
   [neyho.eywa.dataset
    :refer [publisher
            deployed-model]]
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
