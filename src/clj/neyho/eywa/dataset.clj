(ns neyho.eywa.dataset
  (:require 
    clojure.string
    clojure.pprint
    clojure.set
    [clojure.java.io :as io]
    [clojure.tools.logging :as log]
    [clojure.core.async :as async]
    neyho.eywa
    [neyho.eywa.data]
    [neyho.eywa.iam.uuids :as au]
    [neyho.eywa.dataset.core :as dataset]
    [neyho.eywa.db :as db :refer [*db*]]
    [neyho.eywa.dataset.uuids :as du]
    [neyho.eywa.lacinia :as lacinia]
    [com.walmartlabs.lacinia.resolve :as resolve]
    [com.walmartlabs.lacinia.selection :as selection]
    [com.walmartlabs.lacinia.executor :as executor]))


(defonce ^:dynamic *model* (ref nil))

(defn deployed-model [] @*model*)

(defn deployed-entity [id]
  (dataset/get-entity (deployed-model) id))


(defn deployed-relation [id]
  (dataset/get-relation (deployed-model) id))

(defn save-model [new-model]
  (dosync (ref-set *model* new-model)))

(defn list-entity-ids []
  (sort 
    (map 
      (juxt :euuid :name) 
      (dataset/get-entities (deployed-model)))))

(comment
  (deployed-relation #uuid "7efa7244-ae20-4248-9792-7623d12cea9e")
  (deployed-entity #uuid "5338693b-9dbc-4434-b598-b15175da04c3"))

(defn get-deployed-model [_ _ _]
  {:model (deployed-model)})

(defonce wc (async/chan))

(defonce publisher (async/pub wc :topic))

(def datasets #uuid "a800516e-9cfa-4414-9874-60f2285ec330")


; (defn wrap-resolver-request
;   "Function should take handle apply "
;   [handler resolver]
;   (fn [ctx args value]
;     (let [[ctx args value] (handler ctx args value)]
;       (resolver ctx args value))))


; (defn wrap-context
;   [handler resolver]
;   (fn [ctx args value]
;     (let [[ctx args value] (handler ctx args value)]
;       [ctx args (resolver ctx args value)])))


(defn wrap-hooks
  [hooks resolver]
  (if (not-empty hooks)
    (let [hooks (keep
                  (fn [definition]
                    (let [{resolver :fn :as hook} (selection/arguments definition)]
                      (when-some [resolved (try
                                             (resolve (symbol resolver))
                                             (catch Throwable e
                                               (log/errorf e "Couldn't resolve symbol %s" resolver)
                                               nil))]
                        (assoc hook :fn resolved))))
                  hooks)
          {:keys [pre post]} (group-by
                               #(cond 
                                  (neg? (:metric % 1)) :pre
                                  (pos? (:metric % 1)) :post
                                  :else :resolver)
                               hooks)
          steps (cond-> (or (some-> (not-empty (map :fn pre)) vec)
                            [])
                  ;;
                  (some? resolver)
                  (conj (fn wrapped-resolver [ctx args value]
                          (let [new-value (resolver ctx args value)]
                            [ctx args new-value])))
                  ;;
                  (not-empty post)
                  (into (map :fn post)))]
      ;; FINAL RESOLVER
      (fn wrapped-hooks-resolver [ctx args value]
        ; (log/infof "RESOLVING: %s" resolver)
        (let [[_ _ v]
              (reduce
                (fn [[ctx args value] f]
                  (f ctx args value))
                [ctx args value]
                steps)]
          ; (log/infof "RETURNING FINAL RESULT: %s" v)
          v)))
    resolver))



;; WRAPPERS
(defn sync-entity [entity-id data] (db/sync-entity *db* entity-id data))
(defn stack-entity [entity-id data] (db/stack-entity *db* entity-id data))
(defn slice-entity [entity-id args selection] (db/slice-entity *db* entity-id args selection))
(defn get-entity [entity-id args selection] (db/get-entity *db* entity-id args selection))
(defn get-entity-tree [entity-id root on selection] (db/get-entity-tree *db* entity-id root on selection))
(defn search-entity [entity-id args selection] (db/search-entity *db* entity-id args selection))
(defn search-entity-tree [entity-id on args selection] (db/search-entity-tree *db* entity-id on args selection))
(defn purge-entity [entity-id args selection] (db/purge-entity *db* entity-id args selection))
(defn aggregate-entity [entity-id args selection] (db/aggregate-entity *db* entity-id args selection))
(defn delete-entity [entity-id data] (db/delete-entity *db* entity-id data))


(defn deploy! [model] (dataset/deploy! *db* model))
(defn reload [] (dataset/reload *db*))


(defn bind-service-user
  [variable]
  (let [args (select-keys (var-get variable) [:euuid])
        data (get-entity 
               au/user args
               {:_eid nil :euuid nil :name nil 
                :avatar nil :active nil :type nil})]
    (log/debugf "Initializing %s\n%s" variable data)
    (alter-var-root variable (constantly data))))


(defn deploy-update-subscription 
  [{:keys [username]} _ upstream]
  (let [sub (async/chan)]
    (async/sub publisher :refreshedGlobalDataset sub)
    (async/go-loop [{:keys [data]
                     :as published} (async/<! sub)]
      (when published
        (log/tracef "Sending update of global model to user %s" username)
        (upstream data)
        (recur (async/<! sub))))
    (upstream
      {:name "Global" 
       :model (dataset/get-model *db*)})
    (fn []
      (async/unsub publisher :refreshedGlobalDataset sub)
      (async/close! sub))))


(defn get-version
  [euuid]
  (get-entity
    du/dataset-version
    {:euuid euuid}
    {:euuid nil
     :dataset [{:selections
                {:euuid nil
                 :name nil}}]
     :model nil
     :name nil}))


(defn get-version-model
  [euuid]
  (:model (get-version euuid)))


(comment
  (def entity-uuid #uuid "5338693b-9dbc-4434-b598-b15175da04c3")
  (def entity (dataset/get-entity (deployed-model) entity-uuid))
  (def relations (dataset/get-entity-relations (deployed-model) entity))
  (doseq [{relation :euuid} relations]
    (delete-entity du/dataset-relation {:euuid relation}))
  (delete-entity du/dataset-entity {:euuid entity-uuid})
  (def version
    (get-entity
      du/dataset-version
      {:euuid #uuid "1b14b5c9-44ab-4280-8f8a-37c2d419068a"}
      {:euuid nil
       :dataset [{:selections
                  {:euuid nil
                   :name nil}}]
       :model nil
       :name nil}))
  (def version
    )
  (def username "rgersak")
  (def user nil))


(defn deploy-dataset
  ([model] (deploy-dataset nil model nil))
  ([context model] (deploy-dataset context model nil))
  ([{:keys [username]
     :as context}
    {version :version}
    _]
   (let [selection (executor/selections-tree context)] 
     (log/infof
       "User %s deploying dataset %s@%s"
       username (-> version :dataset :name) (:name version))
     (try
       ; (let [{:keys [euuid]} (dataset/deploy! connector version)]
       ;; TODO - Rethink this. Should we use teard-down and setup instead
       ;; of plain deploy! recall!
       (let [{:keys [euuid]} (dataset/deploy! *db* version)]
         (async/put!
           wc {:topic :refreshedGlobalDataset
               :data {:name "Global" 
                      :model (dataset/get-model *db*)}})
         (log/infof
           "User %s deployed version %s@%s"
           username (-> version :dataset :name) (:name version))
         (when (not-empty selection) 
           ; (log/tracef
           ;   "Returning data for selection:\n%s"
           ;   (with-out-str (clojure.pprint/pprint selection)))
           (db/get-entity *db* du/dataset-version {:euuid euuid} selection)))
       (catch clojure.lang.ExceptionInfo e
         (log/errorf
           e "Couldn't deploy dataset version %s@%s"
           (-> version :dataset :name) (:name version))
         (resolve/with-error
           nil
           (assoc (ex-data e) :message (ex-message e))))
       (catch Throwable e
         (log/errorf
           e "Couldn't deploy dataset version %s@%s"
           (-> version :dataset :name) (:name version))
         ;; TODO - Decide if upon unsuccessfull deploy do we delete old table (commented)
         ; (let [{versions :versions} 
         ;       (graphql/get-entity
         ;         connector
         ;         datasets-uuid
         ;         {:euuid euuid}
         ;         {:name nil
         ;          :versions [{:args {:_order_by [{:modified_on :desc}]}
         ;                      :selections {:name nil
         ;                                   :euuid nil
         ;                                   :model nil}}]})]
         ;   (when (empty? versions)
         ;     (dataset/tear-down-module connector version)))
         ; (dataset/unmount connector version)
         #_(server/restart account)
         (resolve/with-error
           nil
           {:message (.getMessage e)
            :type ::dataset/error-unknown}))))))


;; @hook
(defn prepare-deletion-context
  [ctx {:keys [euuid] :as args} v]
  [(if (some? euuid)
     (if-let [dataset (db/get-entity
                        *db*
                        datasets
                        {:euuid euuid}
                        {:name nil
                         :euuid nil
                         :versions [{:args {:_order_by [{:modified_on :asc}]}
                                     :selections {:name nil
                                                  :euuid nil
                                                  :model nil}}]})]
       (do
         (log/infof "Preparing deletion context for dataset: %s" (:name dataset))
         (cond-> ctx
           (and (some? euuid) dataset)
           (assoc ::destroy dataset)))
       ctx)
     ctx)
   args
   v])


;; @hook
(defn destroy-linked-versions
  [{username :eywa/username
    destroy ::destroy
    :as ctx} args v]
  (when destroy
    (log/infof "User %s destroying dataset %s" username (:name destroy))
    (dataset/destroy! *db* destroy)
    (async/put! wc {:topic :refreshedGlobalDataset
                    :data {:name "Global" 
                           :model (dataset/get-model *db*)}}))
  [ctx args v])


(defn is-supported?
  [db]
  (when-not (some #(instance? % db) [neyho.eywa.Postgres])
    (throw
      (ex-info
        "Database is not supported"
        (if (map? db) db {:db db})))))


(defn setup
  "Function will setup initial dataset models that are required for EYWA
  datasets to work. That includes aaa.edm and dataset.edm models"
  ([db]
   (is-supported? db)
   db))

(comment
  (def db *db*)
  (def global-model (dataset/get-last-deployed db))
  (def euuid #uuid "ae3e0f7f-dd0a-468c-9885-caac4141a5c3")
  (dataset/get-relation (deployed-model) #uuid "ae3e0f7f-dd0a-468c-9885-caac4141a5c3")
  (lacinia/generate-lacinia-schema db)
  (do
    (def file "./first_ai_test.edm")
    (def model (neyho.eywa.transit/<-trasit (slurp file)))
    (def model (:model (neyho.eywa.transit/<-transit (slurp "/Users/robi/Downloads/Authentication_Authorization_&_Access_0_75.edm"))))
    (type global-model) (type model)
    (def projection (dataset/project model global-model))
    (filter
      dataset/entity-changed?
      (dataset/get-entities projection))))


(defn init
  "Function initializes EYWA datasets by loading last deployed model."
  ([] (init *db*))
  ([db]
   (log/info "Initializing Datasets...")
   (try
     (dataset/reload db {:model (dataset/get-last-deployed db)})
     (lacinia/add-directive :hook wrap-hooks)
     (lacinia/add-shard ::dataset-directives (slurp (io/resource "dataset_directives.graphql")))
     (lacinia/add-shard ::datasets (slurp (io/resource "datasets.graphql")))
     (catch Throwable e (log/errorf e "Couldn't initialize Datasets...")))
   ; (alter-var-root #'*datasets* (fn [_] db))
   (dataset/reload db)
   (binding [dataset/*return-type* :edn]
     (bind-service-user #'neyho.eywa.data/*EYWA*))
   nil))
