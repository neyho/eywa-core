(ns neyho.eywa.iam.test-oauth
  (:require
    [clojure.test :refer [deftest is use-fixtures]]
    [clojure.data.json :as json]
    [vura.core :as vura]
    [neyho.eywa.iam :as iam]
    [neyho.eywa.iam.oauth2 :as oauth2
     :refer [authorization-request]]))


(def confidential-client
  {:id "XFYWDCONOFSZMTVAEOQHTZFHSUCTXQ",
   :password "e9w7BwGDTLBgaHYxMpctUrOy_aVA4tiZHlgfb2GrotWiBhr_u0",
   :euuid #uuid "3349f1ff-2118-4b3e-babf-a8b68b7e98df",
   :name "oauth_test_confidential",
   :type :confidential,
   :settings
   {:version 0,
    :allowed-grants
    ["refresh_token" "client_credentials" "password" "code"],
    :token-expiry {:access (vura/minutes 5)
                   :refresh (vura/days 1.5)}
    :refresh-tokens true,
    :login-page "http://localhost:8080/login/kbdev/",
    :redirections
    ["http://localhost:8080/eywa/" "http://localhost:8080/app/kbdev"]}})


(def public-client
  {:id "ZHXGGUGLQVOSJZHZCETLFTUZWSSRWG",
   :password nil,
   :euuid #uuid "62972fcf-3cfe-4d34-baea-055308612a0d",
   :name "oauth_test_public",
   :type :public,
   :settings
   {:version 0,
    :logo-url nil
    :login-page "http://localhost:8080/login/kbdev",
    :token-expiry {"access" (vura/minutes 5)
                   "refresh" (vura/days 1.5)}
    :allowed-grants
    ["refresh_token" "client_credentials" "password" "code"],
    :redirections
    ["http://localhost:8080/eywa/" "http://localhost:8080/app/kbdev"]}})


(def test-user
  {:euuid #uuid "bb3e4d1b-d3a0-434a-a60b-beb0d1bf52af"
   :name "oauth_test"
   :password "change-me"
   :active true})


(deftest confidential-authorization-test
  (let [request {:client_id (:id confidential-client)
                 :client_secret (:password confidential-client)
                 :redirect_uri "http://localhost:8080/eywa/"
                 :response_type "code"
                 :username "test_oauth"
                 :password "password"}]
    (is
      (=
       (authorization-request (assoc request :client_id "wrong_client_id"))
       {:status 302,
        :headers {"Location" "/oauth2/request_error?type=client_not_registered",
                  "Cache-Control" "no-cache"}})
      "Request from client that isn't registered should return registration_error.html page")
    (is
      (=
       (authorization-request (dissoc request :redirect_uri))
       {:status 302,
        :headers {"Location" "/oauth2/request_error?type=missing_redirect", "Cache-Control" "no-cache"}})
      "When redirection is missing, redirect to access server controled error page")
    (is
      (=
       (authorization-request (assoc request :redirect_uri "http://localhost/eywa/"))
       {:status 302
        :headers {"Location" "/oauth2/request_error?type=redirect_missmatch", "Cache-Control" "no-cache"}})
      "When redirection is wrong, redirect to access server controled error page")
    (is
      (=
       (authorization-request (dissoc request :client_secret))
       {:status 302,
        :headers {"Location" "http://localhost:8080/eywa/?error=access_denied", "Cache-Control" "no-cache"}})
      "If client password isn't provided and client is configured to use password, than return error redirect to client")
    (is
      (=
       (authorization-request (assoc request :client_secret "wrong-password"))
       {:status 302,
        :headers {"Location" "http://localhost:8080/eywa/?error=access_denied", "Cache-Control" "no-cache"}})
      "If client password is provided and it doesn't match configured password, than return error redirect to client")
    (let [{:keys [status]
           {location "Location"} :headers} (authorization-request request)
          [redirect session] (re-find #"http://localhost:8080/login/kbdev/\?(session=.*)" location)]
      (is (= status 302) "Successfull authentication request should redirect to login page")
      (is (.startsWith redirect "http://localhost:8080/login/kbdev") "Redirecting to wrong login url")
      (is (some? (re-find #"session=[\w\d]{8,30}" session)) "Session not present when redirecting to login page"))))



(deftest token-password-request-test
  (let [request {:client_id (:id confidential-client)
                 :client_secret (:password confidential-client)
                 :grant_type "password"
                 :username (:name test-user)
                 :password (:password test-user)}]
    ;;
    (let [{:keys [status body]} (oauth2/token-endpoint (assoc request :client_secret "wrong"))
          {:strs [error]} (json/read-str body)]
      (is (= status 400) "Token request didn't return proper HTTP status")
      (is (= error "invalid_client") "Wrong error code returned"))
    ;;
    (let [{:keys [status body]} (oauth2/token-endpoint (dissoc request :client_id))
          {:strs [error]} (json/read-str body)]
      (is (= status 400) "Token request didn't return proper HTTP status")
      (is (= error "invalid_client") "Wrong error code returned"))
    ;;
    (let [{:keys [status body]} (oauth2/token-endpoint (dissoc request :username))
          {:strs [error]} (json/read-str body)]
      (is (= status 400) "Token request didn't return proper HTTP status")
      (is (= error "invalid_request") "Wrong error code returned"))
    ;;
    (let [{:keys [status body]} (oauth2/token-endpoint (dissoc request :password))
          {:strs [error]} (json/read-str body)]
      (is (= status 400) "Token request didn't return proper HTTP status")
      (is (= error "invalid_request") "Wrong error code returned"))
    ;; Password OK request
    (let [{:keys [status body]} (oauth2/token-endpoint request)
          {:strs [access_token token_type refresh_token expires_in] :as response} (json/read-str body)]
      (is (= status 200) "Token request didn't return proper HTTP status")
      (is (some? access_token) "Token request didn't return access_token in HTTP response")
      (is (some? token_type) "Token type not specified in HTTP response")
      (is (some? expires_in) "Token expiration time not specified in HTTP response")
      (is (some? refresh_token) "Refresh token not specified for confidential client in HTTP response"))))



(deftest token-client-credentials-request-test
  (let [request {:client_id (:id confidential-client)
                 :client_secret (:password confidential-client)
                 :grant_type "client_credentials"}]
    ;;
    (let [{:keys [status body]} (oauth2/token-endpoint (assoc request :client_secret "wrong"))
          {:strs [error]} (json/read-str body)]
      (is (= status 400) "Token request didn't return proper HTTP status")
      (is (= error "invalid_client") "Wrong error code returned"))
    ;;
    (let [{:keys [status body]} (oauth2/token-endpoint (dissoc request :client_id))
          {:strs [error]} (json/read-str body)]
      (is (= status 400) "Token request didn't return proper HTTP status")
      (is (= error "invalid_client") "Wrong error code returned"))
    ;; Password OK request
    (let [{:keys [status body]} (oauth2/token-endpoint request)
          {:strs [access_token token_type expires_in] :as response} (json/read-str body)]
      (is (= status 200) "Token request didn't return proper HTTP status")
      (is (some? access_token) "Token request didn't return access_token in HTTP response")
      (is (some? token_type) "Token type not specified in HTTP response")
      (is (some? expires_in) "Token expiration time not specified in HTTP response"))))




(defn init []
  (iam/add-client confidential-client)
  (iam/add-client public-client)
  (iam/set-user test-user))


(defn destroy []
  (iam/remove-client confidential-client)
  (iam/remove-client public-client)
  (iam/delete-user test-user))


(use-fixtures :once
              (fn [f]
                (init)
                (f)
                (destroy)))

(comment
  (clojure.test/run-tests 'neyho.eywa.iam.test-oauth))
