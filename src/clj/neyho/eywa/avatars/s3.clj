(ns neyho.eywa.avatars.s3
  (:require
    neyho.eywa.avatars
    [taoensso.nippy :as nippy]
    [clojure.data.codec.base64 :as b64]
    [cognitect.aws.client.api :as aws]))



(extend-type neyho.eywa.S3
  neyho.eywa.avatars/AvatarStoreProtocol
  (-get [{:keys [bucket]} hex]
    (let [{:keys [entity attribute record]} (nippy/thaw (b64/decode (.getBytes hex)))
          path (format "/avatars/%s/%s/%s" entity attribute record)
          s3 (aws/client {:api :s3})]
      (aws/invoke 
        s3
        {:op :GetObject
         :request {:Key path :Bucket bucket}})))
  (-set [{:keys [bucket]} {:keys [entity attribute record]} payload]
    (let [path (format "/avatars/%s/%s/%s" entity attribute record)
          s3 (aws/client {:api :s3})]
      (aws/invoke
        s3 {:op :PutObject
            :request {:Bucket bucket
                      :Key path
                      :Body payload}}))))
