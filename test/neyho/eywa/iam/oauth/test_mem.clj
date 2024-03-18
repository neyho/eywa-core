(ns neyho.eywa.oauth.test-mem
  (:require
    [clojure.test :refer [deftest is use-fixtures]]
    [neyho.eywa.iam :as iam]
    [neyho.eywa.iam.oauth.mem
     :refer [authorization-request]]))


(def confidential-client
  {:euuid #uuid "3349f1ff-2118-4b3e-babf-a8b68b7e98df"
   :name "oauth_test_confidential"
   :password "testisi123$"
   :settings {:version 0
              :type "public"
              :login-page "http://localhost:8080/login/kbdev/"
              :redirections ["http://localhost:8080/eywa/"
                             "http://localhost:8080/app/kbdev"]}})


(def public-client
  {:euuid #uuid "62972fcf-3cfe-4d34-baea-055308612a0d"
   :name "oauth_test_public"
   :password nil
   :settings {:version 0
              :type "public"
              :login-page "http://localhost:8080/login/kbdev"
              :redirections ["http://localhost:8080/eywa/"
                             "http://localhost:8080/app/kbdev"]}})

(=
 {:status 302, :headers {"Location" "/oauth/redirection_error.html?type=missing_redirect", "Cache-Control" "no-cache"}}
 {:status 302, :headers {"Location" "/oauth/redirection_error.html?type=missing_redirect", "Cache-Control" "no-cache"}})

(deftest authorization-request-test
  (let [request {:client-id "oauth_test_confidential"
                 :client-password "testisi123$"
                 :redirect-uri "http://localhost:8080/eywa/"
                 :response-type "code"
                 :username "test_oauth"
                 :password "password"}]
    (is
      (=
       (authorization-request (assoc request :client-id "wrong_client_id"))
       {:status 302,
        :headers {"Location" "/oauth/registration_error.html?type=client_not_registered",
                  "Cache-Control" "no-cache"}})
      "Request from client that isn't registered should return registration_error.html page")
    (is
      (=
       (authorization-request (dissoc request :redirect-uri))
       {:status 302,
        :headers {"Location" "/oauth/redirection_error.html?type=missing_redirect", "Cache-Control" "no-cache"}})
      "When redirection is missing, redirect to access server controled error page")
    (is
      (=
       (authorization-request (assoc request :redirect-uri "http://localhost/eywa/"))
       {:status 302
        :headers {"Location" "/oauth/redirection_error.html?type=redirect_mismatch", "Cache-Control" "no-cache"}})
      "When redirection is wrong, redirect to access server controled error page")
    (is
      (=
       (authorization-request (dissoc request :client-password))
       {:status 302,
        :headers {"Location" "http://localhost:8080/eywa/?error=access_denied", "Cache-Control" "no-cache"}})
      "If client password isn't provided and client is configured to use password, than return error redirect to client")
    (is
      (=
       (authorization-request (assoc request :client-password "wrong-password"))
       {:status 302,
        :headers {"Location" "http://localhost:8080/eywa/?error=access_denied", "Cache-Control" "no-cache"}})
      "If client password is provided and it doesn't match configured password, than return error redirect to client")
    (let [{:keys [status]
           {location "Location"} :headers} (authorization-request request)
          [redirect session] (re-find #"http://localhost:8080/login/kbdev/\?(session=.*)" location)]
      (is (= status 302) "Successfull authentication request should redirect to login page")
      (is (.startsWith redirect "http://localhost:8080/login/kbdev") "Redirecting to wrong login url")
      (is (some? (re-find #"session=[\w\d]{8,30}" session)) "Session not present when redirecting to login page"))))






(defn init []
  (iam/add-client confidential-client)
  (iam/add-client public-client))


(defn destroy []
  (iam/remove-client confidential-client)
  (iam/remove-client public-client))


(use-fixtures :once
              (fn [f]
                (init)
                (f)
                (destroy)))

(comment
  (clojure.test/run-tests 'neyho.eywa.oauth.test-mem))
