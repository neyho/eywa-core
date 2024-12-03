(ns example.movies
  (:require
    [eywa.client :as eywa]
    [cheshire.core :as json]
    [eywa.client.json :refer [<-json ->json]]
    [clojure.pprint :refer [pprint]]
    [clojure.core.async :as async]
    [toddler.graphql :as graphql]))


(defn deploy-movie-model
  []
  (let [model  (slurp "../datasets/Movies_Example_0_2.json")]
    (async/go
      (async/<!
        (eywa/graphql
          (graphql/mutations
            [{:mutation :importDataset
              :types {:dataset :Transit}
              :selection {:euuid nil}
              :variables {:dataset model}}])))
      (println "Movies dataset deployed"))))


(defn slurp-dataset [x] (slurp (str "../datasets/movies/" x ".json")))


(defn load-datasets
  []
  (async/go
    (println "Importing Movies, Actors, Genres, Users...")
    (time
      (let [mutations [{:mutation :syncMovieList
                        :selection {:euuid nil}
                        :types {:movie :MovieInput}
                        :variables {:movie (<-json (slurp-dataset "movies"))}}
                       {:mutation :syncMovieActorList
                        :alias "actors"
                        :selection {:euuid nil}
                        :types {:movie_actor :MovieActorInput}
                        :variables {:movie_actor (<-json (slurp-dataset "movie_actors"))}}
                       {:mutation :syncMovieGenreList
                        :selection {:euuid nil}
                        :alias "genres"
                        :types {:movie_genre :MovieGenreInput}
                        :variables {:movie_genre (<-json (slurp-dataset "movie_genres"))}}
                       {:mutation :syncMovieUserList
                        :selection {:euuid nil}
                        :types {:movie_user :MovieUserInput}
                        :variables {:movie_user (<-json (slurp-dataset "movie_users"))}}
                       {:mutation :syncMovieActorList
                        :alias "actors_mapping"
                        :selection {:euuid nil}
                        :types {:movie_actor :MovieActorInput}
                        :variables {:movie_actor (<-json (slurp-dataset "movie_actors_mapping"))}}
                       {:mutation :syncMovieGenreList
                        :alias "genres_mapping"
                        :selection {:euuid nil}
                        :types {:movie_genre :MovieGenreInput}
                        :variables {:movie_genre (<-json (slurp-dataset "movie_genres_mapping"))}}]]
        (eywa/graphql (graphql/mutations mutations))))
    (doseq [part (partition-all 10000 (<-json (slurp-dataset "user_ratings")))]
      (println "Importing ratings...")
      (time
        (async/<!
          (eywa/graphql
            (graphql/mutations
              [{:mutation :syncUserRatingList
                :types {:user_rating :UserRatingInput}
                :variables {:user_rating part}
                :selection {:euuid nil}}])))))
    (println "Data importing finished!")))


(defn delete-movies
  []
  (async/go
    (async/<!
      (eywa/graphql
        (graphql/mutations
          [{:mutation :deleteDataset
            :args {:euuid #uuid "6b48570e-e629-45f7-b118-b27239690a05"}}])))
    (println "Movies dataset removed!")))


(defn statistics-query
  ([] (statistics-query nil)) 
  ([{:keys [limit title]
     :or {limit 10}}]
   (graphql/queries
     [{:query :searchMovie
       :args (cond-> {:_limit limit}
               title (assoc :title {:_ilike (str \% title \%)}))
       :selection
       {:title nil
        :release_year nil
        :_count [{:selections
                  {:movie_ratings [{:alias :all}
                                   {:alias :super
                                    :args {:_where {:value {:_ge 8}}}}
                                   {:alias :bad
                                    :args {:_where {:value {:_le 4}}}}]}}]
        :_agg [{:selections
                {:movie_ratings
                 [{:selections
                   {:_avg [{:selections {:value nil}}]}}
                  {:selections
                   {:_max [{:selections {:value nil}}]}}
                  {:selections
                   {:_min [{:selections {:value nil}}]}}]}}]
        :movie_ratings
        [{:args {:_join :LEFT :_order_by {:value :desc} :_limit 5}
          :selections {:value nil
                       :user [{:selections {:name nil}}]}}]}}])))


(comment
  (eywa/start)


  (deploy-movie-model)
  (load-datasets)

  (println (statistics-query {:title "enhanced"}))
  (async/go
    (pprint
      (time
        (async/<!
          (eywa/graphql {:query (statistics-query {:title "enhanced"})})))))

  (delete-movies)
  )
