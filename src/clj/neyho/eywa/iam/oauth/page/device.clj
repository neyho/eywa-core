(ns neyho.eywa.iam.oauth.page.device
  {:shadow.css/include
   ["neyho/eywa/iam/oauth/page/common.css"]}
  (:require
    [hiccup2.core :refer [html raw]]
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


(defn authorize
  ([] (authorize nil))
  ([{challenge :neyho.eywa.iam.oauth.device-code/challenge
     error :neyho.eywa.iam.oauth.device-code/error
     user-code :neyho.eywa.iam.oauth.device-code/user-code
     complete? :neyho.eywa.iam.oauth.device-code/complete?}]
   (html
     [:head
      [:meta {:charset "UTF-8"}]
      [:title "EYWA Login"]
      [:link {:rel "icon" :href "https://www.eywaonline.com/eywa/logo/eywa.svg" :crossorigin true}]
      [:link {:rel "preconnect" :href "https://fonts.googleapis.com"}]
      [:link {:rel "preconnect" :href "https://fonts.gstatic.com" :crossorigin true}]
      [:link {:rel "stylesheet" :href "https://fonts.googleapis.com/css2?family=Montserrat:wght@200;300;400;500;600;800;900&family=Roboto&display=swap"}]
      [:link {:rel "stylesheet" :href "../css/device.css"}]]
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
           [:h1 "Device Confirmation"]]]
         [:div {:class (css
                         :py-4
                         :text-center
                         :font-normal
                         :text-sm
                         :text-gray-700
                         :select-none)}
          (if complete?
            "Confirm that following user code is used for device authorization"
            "Type in device code displayed on your device")]
         [:form {:method "POST"
                 :class (css
                          {:margin-block-end "0"}
                          ["& .input-wrapper"
                           :flex :grow
                           :justify-center :items-center
                           :border-b
                           :border-gray-200
                           :pt-4
                           :pb-2]
                          ["& .input-wrapper input"  {:text-align "center"}]
                          ["& .input-wrapper:focus-within" :border-gray-500])}
          (when complete?
            [:input {:type "hidden" 
                     :name "challenge"
                     :value challenge}])
          [:div.input-wrapper
           [:input (cond->
                     {:id "user_code"
                      :type "text"
                      :placeholder "ABCD-WXYZ"
                      :required true
                      :autocomplete "off"
                      :autocapitalize "off"
                      :spellcheck false}
                     complete? (assoc :value user-code
                                      :read-only true
                                      :disabled true)
                     (not complete?) (assoc :name "user_code"))]]
          [:div.error
           {:class (css
                     :flex
                     :items-center
                     :text-red-700
                     :font-medium
                     :text-sm
                     {:min-height "4rem"
                      :width "100%"})}
           (when error [:div.message
                        (case error
                          :expired "This user code has expired"
                          :malicous-code "Entered user code doesn't exist"
                          :malicous-ip "You are trying to enter code from wrong host"
                          :malicious-user-agent "You are trying to enter code from wrong app"
                          error)])]
          [:div {:class (css
                          :flex
                          :grow
                          :justify-center
                          ["& button"
                           :m-4 :p-2
                           :font-semibold
                           {:min-width "6rem" :min-height "2rem"}]
                          ["& button:hover" {:text-shadow "1px 1px 2px rgba(0,0,0,0.3)"}])}
           (if complete?
             [:div
              [:button.confirm {:type "submit" :name "action" :value "confirm"} "Confirm"]
              [:button.cancel {:type "submit" :name "action" :value "cancel"} "Cancel"]]
             [:div
              [:button.continue {:type "submit"} "Continue"]])]]]]]
      [:script {:src "../js/login.js"}]
      [:script
       (raw
         "window.onload = function () {
         document.getElementById(\"user_code\").focus()
         }")]])))




(defn status
  ([{{{:keys [value error]} :query-params} :request}]
   (html
     [:head
      [:meta {:charset "UTF-8"}]
      [:title "EYWA Login"]
      [:link {:rel "icon" :href "https://www.eywaonline.com/eywa/logo/eywa.svg" :crossorigin true}]
      [:link {:rel "preconnect" :href "https://fonts.googleapis.com"}]
      [:link {:rel "preconnect" :href "https://fonts.gstatic.com" :crossorigin true}]
      [:link {:rel "stylesheet" :href "https://fonts.googleapis.com/css2?family=Montserrat:wght@200;300;400;500;600;800;900&family=Roboto&display=swap"}]
      [:link {:rel "stylesheet" :href "../css/device.css"}]]
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
                  "canceled" "Device code flow canceled"
                  "error" (case error
                            "")
                  "Wrong page")]]]
         [:div {:class (css
                         :py-4
                         :text-center
                         :font-normal
                         :text-sm
                         :text-gray-700
                         :select-none)}]
         ]]]
      [:script {:src "../js/login.js"}]])))
