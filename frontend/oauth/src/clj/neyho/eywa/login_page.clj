(ns neyho.eywa.login-page
  {:shadow.css/include
   ["neyho/eywa/login_page.css"]}
  (:require
    [hiccup2.core :refer [html]]
    [shadow.css :refer [css]]
    [neyho.eywa.iam.oauth.login :as login]))


(def $login-wrapper
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


(def $login-greeting
  (css
    :fixed
    {:color "#B9B9B9"
     :top "4em"
     :left "4em"}
    ["& h1"
     {:line-height "64px"
      :font-size "48px"
      :font-weight "700"}]
    ["& h4"
     {:line-height "24px"
      :font-size "18px"
      :font-weight "400"}]))


(def $login-form
  (css
    :flex
    :flex-col
    :items-center
    :relative
    :p-10
    {:background-color "#101010d9"
     :border "1px solid #232323"}
    ["& .row"
     :relative
     {:width "15em"}]
    ["& .row .ficon"
     :absolute
     :top-2 :left-2
     :h-6 :w-6 {:color "#797979"}]
    ["& .row:not(:last-child)" :mb-2]
    ["& span.error" :h-8 :text-xs :block {:color "#B56B6B"}]))


(def $input
  (css 
    :pl-10 :pr-2 :w-full :h-10
    {:transition "border-color .3s ease-in-out"
     :color "#797979"
     :border-color "#232323"
     :background-color "#101010"
     :border "1px solid"}
    ["&:hover:not(:focus)" {:border-color "#689579"}]
    ["&:focus"
     {:border-color "#797979"}]))


(def $sign-in
  (css
    :mt-4
    :h-10
    :uppercase
    {:width "15em"
     :color "#B9B9B9"
     :background-color "#101010"
     :border "1px solid #232323"}
    ["&:hover" {:border-color "#689579"}]))


(def password-icon
  [:svg
   {:viewBox "0 0 24 24"
    :fill "currentColor"
    :class "ficon"}
   [:path
    {:d "M17.7161 8.66667H16.7638V6.7619C16.7638 4.13333 14.6304 2 12.0019 2C9.37329 2 7.23996 4.13333 7.23996 6.7619V8.66667H6.28757C5.23996 8.66667 4.38281 9.52381 4.38281 10.5714V20.0952C4.38281 21.1429 5.23996 22 6.28757 22H17.7161C18.7638 22 19.6209 21.1429 19.6209 20.0952V10.5714C19.6209 9.52381 18.7638 8.66667 17.7161 8.66667ZM12.0019 17.2381C10.9542 17.2381 10.0971 16.381 10.0971 15.3333C10.0971 14.2857 10.9542 13.4286 12.0019 13.4286C13.0495 13.4286 13.9066 14.2857 13.9066 15.3333C13.9066 16.381 13.0495 17.2381 12.0019 17.2381ZM14.9542 8.66667H9.04948V6.7619C9.04948 5.13333 10.3733 3.80952 12.0019 3.80952C13.6304 3.80952 14.9542 5.13333 14.9542 6.7619V8.66667Z"}]])

(def user-icon
  [:svg
   {:viewBox "0 0 16 16"
    :fill "currentColor"
    :class "ficon"}
   [:path
    {:d "M7.99984 8.00008C9.47317 8.00008 10.6665 6.80675 10.6665 5.33341C10.6665 3.86008 9.47317 2.66675 7.99984 2.66675C6.5265 2.66675 5.33317 3.86008 5.33317 5.33341C5.33317 6.80675 6.5265 8.00008 7.99984 8.00008ZM7.99984 9.33342C6.21984 9.33342 2.6665 10.2267 2.6665 12.0001V13.3334H13.3332V12.0001C13.3332 10.2267 9.77984 9.33342 7.99984 9.33342Z"}]])


(defn login-html
  ([{error ::login/error}]
   (html
     [:head
      [:meta {:charset "UTF-8"}]
      [:title "EYWA Login"]
      [:link {:rel "icon" :href "https://www.eywaonline.com/eywa/log/eywa.svg" :crossorigin true}]
      [:link {:rel "preconnect" :href "https://fonts.googleapis.com"}]
      [:link {:rel "preconnect" :href "https://fonts.gstatic.com" :crossorigin true}]
      [:link {:rel "stylesheet" :href "https://fonts.googleapis.com/css2?family=Montserrat:wght@200;300;400;500;600;800;900&family=Roboto&display=swap"}]
      [:link {:rel "stylesheet" :href "css/login.css"}]]
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
      [:div {:class $login-wrapper}
       [:div
        {:class $login-greeting}
        [:h1 "Hellow mate,"]
        [:h4 "Welcome to EYWA authentication page"]]
       [:form
        {:class $login-form
         :method "post"}
        [:div
         {:class :row}
         user-icon
         [:input
          {:id "username"
           :name "username"
           :class $input
           :autoComplete "new-password"}]]
        [:div
         {:class :row}
         password-icon
         [:input
          {:id "password"
           :name "password"
           :class $input
           :type "password"
           :autocomplete "new-password"
           :autocorrect "off"
           :spellcheck false}]]
        [:div
         {:class :row}
         [:span.error (case error
                        ::login/credentials "Invalid credentials. Check your username and password"
                        ::unknown "Something unexpected happened!")]]
        [:button {:class $sign-in}
         [:h4 "SIGN IN"]]]]
      [:script {:src "js/login.js"}]])))
