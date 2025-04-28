(ns neyho.eywa.json
  (:require
   [clojure.instant :refer [read-instant-date]]
   [clojure.string :as str]
   [clojure.data.json :as json]))

(def uuid-pattern #"[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}")

(def date-pattern #"^\d\d{3}-(0[1-9]|1[0-2])-[0-3]\dT[0-2]\d:[0-5]\d:[0-5]\d")

(comment
  (def data "ММ код//боја")
  (map int (seq data))
  (time (pkey-fn "ММ код//боја"))
  (time (pkey-fn "fobi/ksks")))

(defn pkey-fn [data]
  (let [has-letters? (re-find #"\p{L}" data)
        has-spaces? (re-find #"\s" data)
        has-slash? (re-find #"/" data)]
    (cond
      (not has-letters?) (read-string data)
      (re-find uuid-pattern data) data
      (and has-spaces? has-slash?) data
      has-slash? (let [[_namespace _keyword]
                       (str/split data #"/")]
                   (keyword
                    _namespace
                    (str/replace
                     _keyword
                     #"[_\s]+" "-")))
      :else data)))

; (defn pkey-fn [data]
;   (if (re-find #"[a-zA-Z]" data)
;       (if (re-find uuid-pattern data)
;         data
;         (let [[keyword-or-namespace _keyword]
;               (str/split data #"/")]
;           (if _keyword
;             (keyword
;              keyword-or-namespace
;              (str/replace
;               _keyword
;               #"[_\s]+" "-"))
;             (keyword
;              (str/replace
;               keyword-or-namespace
;               #"[_\s]+" "-")))))
;       (read-string data)))

(defn eywa-val-fn
  "Helper function for transforming dates and other objects to Clojure data
  objects"
  [_ data]
  (letfn [(cast-date [date]
            (try
              (read-instant-date date)
              (catch Exception _ nil)))]
    (cond
      (and (string? data) (re-find date-pattern data)) (cast-date data)
      (and (string? data) (= (count data) 36) (re-find uuid-pattern data)) (java.util.UUID/fromString data)
      (vector? data) (mapv #(eywa-val-fn nil %) data)
      (map? data) (reduce
                   (fn [r [k v]] (assoc r k (eywa-val-fn k v)))
                   {}
                   data)
      :else data)))

(defn <-json
  ([v & {:keys [keyfn valfn]
         :or {keyfn pkey-fn
              valfn eywa-val-fn}}]
   (json/read-str
    v
    :key-fn keyfn
    :value-fn valfn)))

(defn ->json [data]
  (json/write-str
   data
   :key-fn (fn [data]
             (if (keyword? data)
               (if-let [n (namespace data)]
                 (str n "/" (name data))
                 (name data))
               data))))

(comment
  (keyword "jiof/ieo")
  (def data
    {"jiofqi  82801" :moj
     :doijo #uuid "ed934519-98f8-46ca-bc6a-6136d5d0b1c4"
     "hello" #inst "2021-11-04T19:50:45.120-00:00"})
  (time (<-json (->json data))))
