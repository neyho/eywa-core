(ns examples.movies
  (:require
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



(defn insert-movies
  []
  (println "Inserting Movies")
  (dataset/sync-entity -movie- (load-dataset "movies")))


(defn insert-actors
  []
  (println "Inserting actors")
  (let [actors (load-dataset "movie_actors")]
    (dataset/sync-entity -actor- actors)))


(defn insert-genres
  []
  (println "Inserting genres")
  (let [genres (load-dataset "movie_genres")]
    (dataset/sync-entity -genre- genres)))


(defn insert-users
  []
  (let [users (map
                #(update % :join-date (fn [d] (java.util.Date. d)))
                (load-dataset "movie_users"))]
    (println "Inserting " (count users) " users")
    (dataset/sync-entity -user- users)))


(defn insert-ratings
  []
  (let [ratings (map
                  (fn [{:keys [movie-id user-id] :as data}]
                    (assoc data
                           :movie {:euuid movie-id}
                           :user {:euuid user-id}))
                  (load-dataset "user_ratings"))]
    (println "Inserting " (count ratings) " ratings")
    (doseq [part (partition 5000 ratings)]
      (dataset/sync-entity -rating- part))))


(defn insert-actor-mapping
  []
  (let [actor-mapping (load-dataset "movie_actors_mapping")
        per-actor (reduce-kv
                    (fn [r euuid movies]
                      (conj
                        r
                        (hash-map :euuid euuid :movies
                                  (map #(hash-map :euuid (:movie-id %)) movies))))
                    []
                    (group-by :actor-id actor-mapping))]
    (println "Linking " (count per-actor) " actors with movies")
    (dataset/sync-entity -actor- per-actor)))

(defn insert-genre-mapping
  []
  (let [genre-mapping (load-dataset "movie_genres_mapping")
        per-genre (reduce-kv
                    (fn [r euuid movies]
                      (conj
                        r
                        (hash-map :euuid euuid :movies
                                  (map #(hash-map :euuid (:movie-id %)) movies))))
                    []
                    (group-by :genre-id genre-mapping))]
    (println "Linking " (count per-genre) " movies to genres")
    (dataset/sync-entity -genre- per-genre)))



(defn all
  []
  (deploy-dataset)
  (insert-movies)
  (insert-actors)
  (insert-genres)
  (insert-users)
  (insert-actor-mapping)
  (insert-genre-mapping)
  (insert-ratings))



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
  (core/tear-down db)
  )
