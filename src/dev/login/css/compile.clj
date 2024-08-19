(ns login.css.compile
  (:require
    [clojure.java.io :as io]
    [shadow.css.build :as cb]
    [shadow.cljs.devtools.server.fs-watch :as fs-watch]))


(defonce css-ref (atom nil))
(defonce css-watch-ref (atom nil))


(defn generate-css []
  (let [result
        (-> @css-ref
            (cb/generate '{:login {:include [neyho.eywa.iam.oauth.page.login]}
                           :device {:include [neyho.eywa.iam.oauth.page.device]}})
            (cb/write-outputs-to (io/file "frontend" "dist" "oauth" "css")))]
    (println "Refreshing frontend/dist/oauth/css")
    (doseq [mod (:outputs result)
            {:keys [warning-type] :as warning} (:warnings mod)]
      (prn [:CSS (name warning-type) (dissoc warning :warning-type)]))
    (println "Refresh finished")))


(defn init []
  (->
    (cb/init)
    (cb/start)
    (cb/index-path (io/file "src" "clj") {})))


(defn start
  {:shadow/requires-server true}
  []

  ;; first initialize my css
  (reset! css-ref (init))

  ;; then build it once
  (generate-css)

  ;; then setup the watcher that rebuilds everything on change
  (reset! css-watch-ref
          (fs-watch/start
            {}
            [(io/file "src" "clj")]
            ["cljs" "cljc" "clj"]
            (fn [updates]
              (try
                (doseq [{:keys [file event]} updates
                        :when (not= event :del)]
                  ;; re-index all added or modified files
                  (swap! css-ref cb/index-file file))

                (generate-css)
                (catch Exception e
                  (prn :css-build-failure)
                  (prn e))))))

  ::started)


(defn stop []
  (when-some [css-watch @css-watch-ref]
    (fs-watch/stop css-watch)
    (reset! css-ref nil))
  ::stopped)


(defn go []
  (stop)
  (start))


(defn release [_]
  ;; first initialize my css
  (reset! css-ref (init))

  ;; then build it once
  (generate-css))


(comment
  (-> css-ref deref keys)
  (-> css-ref deref :namespaces)
  (-> css-ref deref :aliases :box-border)
  (-> css-ref deref :aliases :select-none)
  (-> css-ref deref :aliases :text-eywa)
  (-> css-ref deref :aliases :bg-eywa)
  (-> css-ref deref :aliases :e-menu-logo)
  (-> css-ref deref :aliases :input-normal-click)
  (-> css-ref deref :colors)
  (-> css-ref deref :aliases :text-xxs)
  (-> css-ref deref :aliases :transition)
  (-> css-ref deref)
  (-> css-ref deref :namespaces keys)
  (get eywa.css/aliases :bg-eywa)
  (spit "aliases.edn" (-> css-ref deref :aliases keys))
  (io/resource "css/toddler_notifications.css")
  (go))
