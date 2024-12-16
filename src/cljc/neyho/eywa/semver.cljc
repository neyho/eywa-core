(ns neyho.eywa.semver
  (:refer-clojure :exclude [< > <= >=]))

(defn valid? [version]
  (if-let [_version (re-matches
                     #"^(0|[1-9]\d*)\.(0|[1-9]\d*)\.(0|[1-9]\d*)(?:-([0-9A-Za-z-]+(?:\.[0-9A-Za-z-]+)*))?(?:\+([0-9A-Za-z-]+(?:\.[0-9A-Za-z-]+)*))?$"
                     version)]
    _version
    (throw
     (ex-info "Not valid semver"
              {:version version}))))

(defn <-semver [version]
  (letfn [(->int [x]
            (try
              #?(:clj (Integer/parseInt x)
                 :cljs (js/parseInt x))
              (catch #?(:clj Throwable :cljs js/Error) _
                (throw
                 (ex-info
                  (str "Couldn't parse semantic verioning string: " version)
                  {:version version
                   :failed x})))))]
    (let [[_ major minor patch pre-release build] (valid? version)]
      {:major (->int major)
       :minor (->int minor)
       :patch (->int patch)
       :pre-release pre-release
       :build build})))

(defn compare-prerelease [pr1 pr2]
  (cond
    ;; No prerelease means higher precedence
    (and (nil? pr1) (nil? pr2)) 0
    (nil? pr1) 1
    (nil? pr2) -1
    :else
    (loop [a pr1
           b pr2]
      (cond
        (empty? a) (if (empty? b) 0 -1)
        (empty? b) 1
        :else
        (let [part-a (first a)
              part-b (first b)]
          (cond
            (and (re-matches #"\d+" part-a) (re-matches #"\d+" part-b))
            (let [num-a (Integer/parseInt part-a)
                  num-b (Integer/parseInt part-b)]
              (if (not= num-a num-b) (compare num-a num-b) (recur (rest a) (rest b))))

            (re-matches #"\d+" part-a) -1
            (re-matches #"\d+" part-b) 1
            :else
            (let [cmp (compare part-a part-b)]
              (if (not= cmp 0) cmp (recur (rest a) (rest b))))))))))

(defn compare-semver [v1 v2]
  (let [ver1 (<-semver v1)
        ver2 (<-semver v2)]
    (cond
      (nil? ver1) (throw (ex-info "Invalid semantic version" {:version v1}))
      (nil? ver2) (throw (ex-info "Invalid semantic version" {:version v2}))

      (not= (:major ver1) (:major ver2)) (compare (:major ver1) (:major ver2))
      (not= (:minor ver1) (:minor ver2)) (compare (:minor ver1) (:minor ver2))
      (not= (:patch ver1) (:patch ver2)) (compare (:patch ver1) (:patch ver2))
      :else (compare-prerelease (:prerelease ver1) (:prerelease ver2)))))

(defn ->semver [{:keys [major minor patch pre-release build] :as data}]
  (when (nil? major) (throw (ex-info "Can't create semver without major part" data)))
  (when (nil? minor) (throw (ex-info "Can't create semver without minor part" data)))
  (when (nil? patch) (throw (ex-info "Can't create semver without patch part" data)))
  (str major \. minor \. patch
       (when pre-release (str \- pre-release))
       (when build (str \+ build))))

(defn <
  "Returns non-nil if nums are in monotonically decreasing order,
  otherwise false."
  {:inline-arities #{2}
   :added "1.0"}
  ([_] true)
  ([x y] (clojure.core/< (compare-semver x y) 0))
  ([x y & more]
   (if (< x y)
     (if (next more)
       (recur y (first more) (next more))
       (< y (first more)))
     false)))

(defn >
  "Returns non-nil if nums are in monotonically decreasing order,
  otherwise false."
  {:inline-arities #{2}
   :added "1.0"}
  ([_] true)
  ([x y] (clojure.core/> (compare-semver x y) 0))
  ([x y & more]
   (if (> x y)
     (if (next more)
       (recur y (first more) (next more))
       (> y (first more)))
     false)))

(defn <=
  "Returns non-nil if nums are in monotonically non-decreasing order,
  otherwise false."
  {:inline-arities #{2}
   :added "1.0"}
  ([_] true)
  ([x y] (clojure.core/<= (compare-semver x y) 0))
  ([x y & more]
   (if (<= x y)
     (if (next more)
       (recur y (first more) (next more))
       (<= y (first more)))
     false)))

(defn >=
  "Returns non-nil if nums are in monotonically non-decreasing order,
  otherwise false."
  {:inline-arities #{2}
   :added "1.0"}
  ([_] true)
  ([x y] (clojure.core/>= (compare-semver x y) 0))
  ([x y & more]
   (if (>= x y)
     (if (next more)
       (recur y (first more) (next more))
       (>= y (first more)))
     false)))

(comment
  (def version "2.02.22-test.robi1")
  (>=  "3.0.0" "2.2.22-test.robi1" "2.1.33-test-robi2")
  (->semver (<-semver "2.x.1-test.robi+10921")))
