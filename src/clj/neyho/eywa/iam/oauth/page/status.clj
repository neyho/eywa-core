(ns neyho.eywa.iam.oauth.page.status
  {:shadow.css/include
   ["neyho/eywa/iam/oauth/page/common.css"]}
  (:require
    [hiccup2.core :refer [html]]
    [shadow.css :refer [css]]
    [neyho.eywa.iam.oauth.core :as core]))


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


(defn status
  ([{{{:keys [value error error_description user client]} :query-params} :request}]
   (html
     [:head
      [:meta {:charset "UTF-8"}]
      [:meta {:name "viewport" :content "width=device-width, initial-scale=1.0"}]
      [:title "EYWA OAuth Status"]
      [:link {:rel "icon" :href "https://my.eywaonline.com/images/eywa.svg" :crossorigin true}]
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
         :href "https://my.eywaonline.com/images/login_normal.png"}]]
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
           [:image {:src "https://my.eywaonline.com/images/eywa.svg"}]]
          [:div
           [:h1 (case value
                  "success" "Authentication Success"
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
          (cond
            ;;
            error
            (letfn [(with-please [x] (str x "\n Please restart authentication process"))
                    (contact-support [x] (str x "\n Please contact application support"))]
              (case error
                "broken_flow" (with-please "Authorization flow is broken.")
                "device_code_expired" (with-please "User code that you have entered has expired.")
                "already_authorized" (with-please "Somebody already authenticated using same code")
                "ip_address" (with-please "Registered potentially malicious IP address change action.")
                "user_agent" (with-please "Registered potentially malicious user agent change action.")
                "challenge" (with-please "Registered potentially malicious challenge action. ")
                "corrupt_session" (contact-support "Session id wasn't provided by access server")
                "missing_response_type" (contact-support "Client didn't specify response_type")
                "client_not_registered" (contact-support "Client is not registered")
                "missing_redirect" (contact-support "Client authorization request didn't specify response_type")
                "redirect_missmatch" (contact-support "Couldn't match requested redirect to any of configured redirects for client")
                "no_redirections" (contact-support "Client doesn't has 0 configured redirections")
                ;; 
                "unsupported_grant_type" (contact-support "Grant type specified isn't supported")
                error_description))
            ;;
            (and user client)
            (let [client (get @core/*clients* (java.util.UUID/fromString client))]
              [:span "Client " [:b (:name client)] " is authorized by " [:b user] " user."])
            ;;
            :else nil)]]]]
      [:script {:src "js/login.js"}]])))



(def status-page
  {:enter
   (fn [ctx]
     (assoc ctx :response
            {:status 200
             :headers {"Content-Type" "text/html"}
             :body (str (status ctx))}))})
