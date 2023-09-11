(ns neyho.eywa.storage.s3
  (:require
    clojure.string
    [environ.core :refer [env]]
    neyho.eywa
    [neyho.eywa.storage
     :as storage]
    [clojure.java.io :as io]
    [clojure.tools.logging :as log]
    cognitect.aws.client
    [cognitect.aws.client.api :as aws]))



(defn fix-bucket [n]
  (clojure.string/replace n #"[_\s]+" "."))


(defn init
  ([] (init (env :aws-s3-bucket-name)))
  ([bucket]
   (if (empty? bucket)
     (log/error "Couldn't initialize S3 bucket. Bucket name is not available")
     (let [bucket (fix-bucket bucket)
           client
           (aws/client 
             {:api :s3})
           S3 (neyho.eywa/map->S3 {:client client :bucket bucket})]
       (if-not (storage/-available? S3)
         (log/errorf "[%s]S3 storage is not available" bucket)
         (log/info "Connected to S3 storage"))
       (alter-var-root #'neyho.eywa.storage/*storage* (fn [_] S3))))
   nil))


(defn setup
  [bucket]
  (let [client (aws/client {:api :s3 })
        S3 (neyho.eywa/map->S3 {:client client :bucket bucket})]
    (aws/invoke
      client
      {:op :CreateBucket
       :request {:Bucket bucket}})
    S3))


(defn tear-down [bucket]
  )


(extend-type neyho.eywa.S3
  storage/StorageProtocol
  (storage/-available? [{:keys [client bucket]}]
    (when (and
            (some? client)
            (some
              (comp #{bucket} :Name)
              (:Buckets (aws/invoke client {:op :ListBuckets}))))
      client))
  (storage/-search 
    ([this] (storage/-search this nil))
    ([{:keys [bucket] :as this} options]
     (when-let [s3 (storage/-available? this)] 
       (aws/invoke s3
                   (cond-> {:op :ListObjectsV2
                            :request {:Bucket bucket}}
                     (not-empty options) (update :request merge options))))))
  (storage/-upload
    [{:keys [bucket] :as this} file key]
    (when-let [s3 (storage/-available? this)] 
      (if (instance? java.io.File file)
        (let [file' (io/file file)]
          (assert (.exists file') "File doesn't exist")
          (assert (.isFile file') "File is directory")
          (with-open [file'' (io/input-stream file')] 
            (aws/invoke s3
                        {:op :PutObject
                         :request {:Key key 
                                   :Bucket bucket 
                                   :Body file''}})))
        (aws/invoke 
          s3
          {:op :PutObject
           :request {:Key key 
                     :Bucket bucket 
                     :Body file}}))))
  (storage/-download 
    ([this key] (storage/-download this key nil))
    ([{:keys [bucket] :as this} key version] 
     (when-let [s3 (storage/-available? this)]
       (let [{body :Body
              error :Error}
             (aws/invoke 
               s3
               {:op :GetObject
                :request (cond->
                           {:Key key :Bucket bucket}
                           (some? version) (assoc :VersionId version))})]
         (if (not-empty error)
           (throw (Exception. (:Message error)))
           body))))
    ([this key version path]
     (let [{f :Body} (storage/-download this key version)
           tf (io/file path)]
       (io/make-parents tf)
       (io/copy f tf))))
  (-delete 
    ([this key] (storage/-delete this key nil))
    ([{:keys [bucket] :as this} key version]
     (when-let [s3 (storage/-available? this)]
       (aws/invoke
         s3
         {:op :DeleteObject
          :request (cond-> {:Bucket bucket 
                            :Key key}
                     (some? version) (assoc :VersionId version))})))))




