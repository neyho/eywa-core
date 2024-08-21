(ns neyho.eywa.iam.oauth.page.status
  {:shadow.css/include
   ["neyho/eywa/iam/oauth/page/common.css"]}
  (:require
    [hiccup2.core :refer [html]]
    [shadow.css :refer [css]]))


(def $confirm-container
  (css
    :w-full
    :h-full
    :flex
    :grow
    :absolute
    :top-0
    :justify-center
    :items-center
    :flex-col))


(defn error
  ([{{{:keys [value error]} :query-params} :request}]
   (html
     [:head
      [:meta {:charset "UTF-8"}]
      [:title "EYWA Login"]
      [:link {:rel "icon" :href "https://www.eywaonline.com/eywa/logo/eywa.svg" :crossorigin true}]
      [:link {:rel "preconnect" :href "https://fonts.googleapis.com"}]
      [:link {:rel "preconnect" :href "https://fonts.gstatic.com" :crossorigin true}]
      [:link {:rel "stylesheet" :href "https://fonts.googleapis.com/css2?family=Montserrat:wght@200;300;400;500;600;800;900&family=Roboto&display=swap"}]
      [:link {:rel "stylesheet" :href "css/status.css"}]]
     [:body
      [:svg
       {:id "background-viewbox"
        :viewBox "0 0 1000 1000"
        :style {:width "100%"
                :height "100%"}}
       [:image
        {:id "background-image"
         :x 0 :y 0 :height 1000 :width 1000
         :href "https://www.eywaonline.com/eywa/images/login_normal.png"}]]
      [:div {:class $confirm-container}
       [:div {:class (css
                       :py-8
                       :px-7
                       :rounded-lg
                       {:background "#ffffffbd"
                        :max-width "500px"}
                       ["& .header" :py-4 :flex :justify-center :items-center :select-none]
                       ["& .logo" :w-10 :h-10 :mr-4]
                       ["& h1, & button" :select-none {:color "#3a3c3f"}])}
        [:div {:class (css :p-8 :bg-white :rounded-lg)}
         [:div.header
          [:div.logo
           [:image {:src "https://www.eywaonline.com/eywa/logo/eywa.svg"}]]
          [:div
           [:h1 (case value
                  "canceled" "Authentication Canceled"
                  "error" "Authentication Error" 
                  "Wrong page")]]]
         [:div {:class (css
                         :py-4
                         :text-center
                         :font-normal
                         :text-sm
                         :text-gray-700
                         :select-none)}
          (str
            (case error
              "already_authorized" "Somebody already authenticated using same code"
              "ip_address" "Registered potentially malicious IP address change action. "
              "user_agent" "Registered potentially malicious user agent change action. "
              "challenge" "Registered potentially malicious challenge action. ")
            "\n Please restart authentication process")]]]]
      [:script {:src "js/login.js"}]])))



(def status-page
  {:enter
   (fn [ctx]
     (assoc ctx :response
            {:status 200
             :headers {"Content-Type" "text/html"}
             :body (str (error ctx))}))})
