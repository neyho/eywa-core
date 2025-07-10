(ns neyho.eywa.transit
  (:require
   neyho.eywa.dataset.core
   neyho.eywa.modeling.core
   [cognitect.transit :as transit])
  #?(:clj
     (:import
      [com.cognitect.transit WriteHandler ReadHandler]
      [java.io ByteArrayInputStream ByteArrayOutputStream])))

(def ^:dynamic *format* :json-verbose)
(def ^:dynamic *read-transit* (constantly nil))
(def ^:dynamic *write-transit* (constantly nil))

(defn ->transit [data] (*write-transit* data))
(defn <-transit [data] (*read-transit* data))

(defn init-transit-handlers
  ([{:keys [write read]}]
   #?(:clj
      (alter-var-root
       #'*write-transit*
       (fn [_]
         (fn writter [data]
           (when data
             (with-open [baos (ByteArrayOutputStream.)]
               (let [w (transit/writer
                        baos
                        :json-verbose
                        {:handlers write})
                     _ (cognitect.transit/write w data)
                     ret (.toString baos)]
                 (.reset baos)
                 ret))))))
      :cljs
      (set! *write-transit*
            (fn writter [data]
              (when data
                (let [wr (transit/writer
                          :json-verbose
                          {:handlers write})]
                  (transit/write wr data))))))
   #?(:clj
      (alter-var-root
       #'*read-transit*
       (fn [_]
         (fn reader [data]
           (when data
             (with-open [in (ByteArrayInputStream. (.getBytes data))]
               (transit/read (transit/reader in :json-verbose {:handlers read})))))))
      :cljs
      (set! *read-transit*
            (fn reader [data]
              (when data
                (let [reader (transit/reader :json-verbose {:handlers read})]
                  (transit/read reader data))))))))

(letfn [(record-write-handler
          [type ks]
          (reify #?(:clj WriteHandler :cljs Object)
            (tag [_ _]
              type)
            (rep [_ rec]
              (transit/tagged-value "map" (select-keys rec ks)))
            (stringRep [_ _] nil)
            (getVerboseHandler [_] nil)))
        (record-read-handler
          [ctor]
          #?(:clj
             (reify  ReadHandler
               (fromRep [_ m] (ctor m)))
             :cljs
             (fn [rep] (ctor rep))))]
  (let [wr [neyho.eywa.dataset.core.ERDRelation
            neyho.eywa.dataset.core.ERDEntityAttribute
            neyho.eywa.dataset.core.ERDEntity
            neyho.eywa.dataset.core.ERDModel
            neyho.eywa.modeling.core.Coordinate
            neyho.eywa.modeling.core.PathSegment
            neyho.eywa.modeling.core.Path]
        rdr ["neyho.eywa.dataset.core.ERDRelation"
             "neyho.eywa.dataset.core.ERDEntityAttribute"
             "neyho.eywa.dataset.core.ERDEntity"
             "neyho.eywa.dataset.core.ERDModel"
             "neyho.eywa.modeling.core.Coordinate"
             "neyho.eywa.modeling.core.PathSegment"
             "neyho.eywa.modeling.core.Path"]
        bds [neyho.eywa.dataset.core/map->ERDRelation
             neyho.eywa.dataset.core/map->ERDEntityAttribute
             neyho.eywa.dataset.core/map->ERDEntity
             neyho.eywa.dataset.core/map->ERDModel
             neyho.eywa.modeling.core/map->Coordinate
             neyho.eywa.modeling.core/map->PathSegment
             neyho.eywa.modeling.core/map->Path]]
    (letfn [(eywa-write-handlers []
              (zipmap wr (map record-write-handler rdr (map #(keys (% nil)) bds))))
            (eywa-read-handlers []
              (zipmap rdr (map record-read-handler bds)))]
      (defn init
        []
        (init-transit-handlers
         {:read (eywa-read-handlers)
          :write (eywa-write-handlers)})))))


