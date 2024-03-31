(ns neyho.eywa.iam.oidc
  (:require
    [clojure.spec.alpha :as s]
    [ring.util.codec :as codec]
    [io.pedestal.interceptor.chain :as chain]
    [neyho.eywa.iam.oauth2 :as oauth2]))


(s/def ::id-token (s/keys :req-un [::iss ::sub ::aud ::exp ::iat]))


;; iss - Issuer identifier
;; sub - Subject identifier
;; aud - Audience(s)
;; exp - Expiration time for token
;; iat - issued at
;; auth_time - 
;; nonce - string value used to associate client session with id token, to mitigate replay attacks
;; acr - 
;; amr - Authentication methods reference
;; azp - Authorized party... Must contain OAuth Client ID


(defn implicit-flow
  [request]
  (oauth2/token-password-grant request))


(defn hybrid-flow
  [request])


(defn authorization-flow
  [{:keys [scope prompt] :as request}]
  (if (or
        (empty? scope)
        (not (contains? scope "openid")))
    (oauth2/authorization-code-flow request) 
    (let []
      (cond
        ;; interaction_required
        ; (and prompt (= prompt "none"))
        ; (oauth2/handle-request-error
        ;   {:type "interaction_required"
        ;    :request request})
        ;; login_required
        ;; account_selection_required
        ;; consent_required
        ;; invalid_request_ur
        ;; invalid_request_object
        ;; request_not_supported
        ;; request_uri_not_supported 
        ;; registration_not_supported
        :else
        (oauth2/authorization-code-flow request)))))


(defn authorization-request
  [{:keys [response_type username password redirect_uri]
    :as request}]
  (case response_type
    ;;
    ("code") (authorization-flow request) 
    ;;
    ("id_token" "id_token token") (implicit-flow request)
    ;;
    ("code id_token" "code token" "code id_token token") (hybrid-flow request)
    ;; TODO - check if this is valid
    (cond
      ;;
      (empty? redirect_uri)
      (oauth2/handle-request-error
        {:type "missing_redirect"
         :request request})
      ;;
      (empty? response_type)
      (oauth2/handle-request-error
        {:type "missing_response_type"
         :request request})
      ;;
      :else
      (oauth2/handle-request-error
        {:type "server_error"
         :request request}))))


(def authorize-request-interceptor
  {:name ::authorize-request
   :enter
   (fn [{{:keys [params]} :request :as context}]
     (chain/terminate
       (assoc context :response
              (authorization-request params))))})


(comment
  (s/valid? ::id-token {:iss 100})
  (s/valid? ::id-token {:iss 100 :sub "09102" :aud "291" :exp "2019" :iat 100}))
