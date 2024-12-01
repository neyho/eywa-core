(ns examples.movies
  (:require
    [camel-snake-kebab.core :as csk]
    [camel-snake-kebab.extras :as cske]
    [clojure.data.json :as json]
    [neyho.eywa]
    [neyho.eywa.core :as core]
    [neyho.eywa.dataset :as dataset]
    [neyho.eywa.json :refer [<-json]]
    [neyho.eywa.transit :refer [<-transit]]))


(defn slurp-dataset
  [relative]
  (slurp (str "../../datasets/movies/" relative ".json")))


(defn load-dataset
  [name]
  (<-json (slurp-dataset name)))




(comment
  (def md (slurp-dataset "movies"))
  (def md (load-dataset "movies"))
  (count md)
  (first md)
  )


(def -actor- #uuid "f274cee0-681f-48d5-a67d-b0232f456f86")
(def -movie- #uuid "607556f6-8eaa-4089-a438-70aa91cd96e0")
(def -genre- #uuid "73b5f25f-727e-4180-8e4e-60d6dfa5ad92")
(def -user- #uuid "4b0a2143-d049-4022-9cbe-f0fba1ca3149")
(def -rating- #uuid "43056d28-e652-49a9-9276-70abb7019785")


(defn deploy-dataset
  []
  (dataset/deploy! (<-transit (slurp "../../datasets/Movies_Example_0_2.json"))))



(defn all
  []
  (deploy-dataset)
  (doseq [[entity dataset] [[-movie- "movies"]
                            [-actor- "movie_actors"]
                            [-genre- "movie_genres"]
                            [-user- "movie_users"]
                            [-actor- "movie_actors_mapping"]
                            [-genre- "movie_genres_mapping"]
                            [-rating- "user_ratings"]]]
    (doseq [part (partition-all 10000 (load-dataset dataset))]
      (println "Loading " dataset \[ (count part) \])
      (dataset/stack-entity entity part))))



(comment

  (def db
    (neyho.eywa/map->Postgres
      {:host "localhost"
       :port 5432
       :db "eywa_movies_test"
       :password "password"
       :user "postgres"
       :max-connections 20}))

  (core/initialize db)


  (core/set-superuser
    db
    {:username "admin"
     :password "admin"})


  (core/start db)


  ;; navigate ot https://my.eywaonline.com/

  (time (all))


  (time
    (dataset/search-entity
      -actor-
      {:_limit 100}
      {:euuid nil
       :name nil
       :nationality nil
       :movies [{:selections
                 {:title nil
                  :release_year nil
                  :_agg [{:selections
                          {:movie_ratings
                           [{:selections
                             {:_avg [{:selections
                                      {:value nil}}]}}]}}]}}]}))

  (time
    (dataset/search-entity
      -movie-
      {:_limit 100}
      {:title nil
       :release_year nil
       :_count [{:selections
                 {:movie_ratings nil}}]
       :movie_ratings [{:args {:_order_by {:value :desc}}
                        :selections
                        {:value nil
                         :user
                         [{:selections
                           {:name nil
                            :join_date nil
                            :country nil}}]}}]}))

  ;; To drop database run
  (core/tear-down db))
