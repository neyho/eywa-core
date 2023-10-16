(ns neyho.eywa.server.interceptors.avatars
  (:require
    clojure.string
    [taoensso.nippy :as nippy]
    [clojure.java.io :as io]
    [clojure.tools.logging :as log]
    [clojure.data.codec.base64 :as b64]
    [neyho.eywa.avatars :as avatars]
    [neyho.eywa.storage :as storage]))


(comment
  (def hex "TlBZAHAEagZlbnRpdHlbChC68j7ZTlOFen4M1Yz2+GoJYXR0cmlidXRlW7KIZWhd4UmjmSqH+2DtGXFqBnJlY29yZFvCfWgS5EpDALZQaUQ3XWGTagRzYWx0aQRlR2tZ")
  (def hex "TlBZAHAEagZlbnRpdHlb7cqx2+5vR0S/6kR4KIkyI2oJYXR0cmlidXRlW1zFi3Bol0kZu0905frqVaRqBnJlY29yZFvFpnkiNR5Mo5XC+lKno+K1agRzYWx0aQRkMUVa")
  (def record
    (select-keys
      (nippy/thaw (b64/decode (.getBytes hex)))
      [:entity :attribute :record])))


;; This should be in some other namespace
; (defonce ^:dynamic *rocks* nil)
(defn get-avatar
  [hex]
  (let [record (select-keys
                 (nippy/thaw (b64/decode (.getBytes hex)))
                 [:entity :attribute :record])]
    (if-let [avatar-cache (avatars/get hex)]
      (io/input-stream avatar-cache)
      (if (storage/available?)
        (try
          (let [img (storage/download (str "uploads/avatars/" (:entity record) \/ (:attribute record) \/ (:record record)))]
            (when img
              (avatars/set
                record
                (with-open [out (java.io.ByteArrayOutputStream.)]
                  (io/copy img out)
                  (.toByteArray out))))
            img)
          (catch Throwable _
            (log/error "Avatar not found: %s" record)))
        (log/error "Storage not available")))))


(def avatars
  {:name :eywa/avatars
   :enter (fn [{{:keys [path-params]} :request
                :as context}]
            (let [{:keys [avatar]} path-params
                  avatar (get-avatar avatar)]
              ;; TODO - huh what to decide here... Should i return 404 not found
              ;; If i do i get CORS errors on frontend. Damn
              (assoc context :response {:status 200
                                        :cache-control "max-age=60480"
                                        :headers {"Cache-Control" "max-age=60480"}
                                        :body avatar})))})
