(ns neyho.eywa.iam.oauth2
  (:require
    [nano-id.core :refer [nano-id]]
    [clojure.string :as str]
    [neyho.eywa.iam.oauth2.protocol
     :refer [redirect-login
             request-error
             validate-client
             validate-resource-owner
             return-token
             token-error
             return-code
             set-session-client
             set-session-resource-owner
             get-session
             set-session
             remove-session]]))


(def request-errors
  (reduce-kv
    (fn [r k v]
      (assoc r k (str/join "\n" v)))
    nil
    {"invalid_request"
     ["The request is missing a required parameter, includes an"
      "invalid parameter value, includes a parameter more than" 
      "once, or is otherwise malformed."]
     ;;
     "unauthorized_client"
     ["The client is not authorized to request an authorization"
      "code using this method."]
     ;;
     "access_denied"
     ["The resource owner or authorization server denied the"
      "request."]
     ;;
     "unsupported_response_type"
     ["The authorization server does not support obtaining an"
      "authorization code using this method."]
     ;;
     "invalid_scope"
     ["The requested scope is invalid, unknown, or malformed."]
     ;;
     "server_error"
     ["The authorization server encountered an unexpected"
      "condition that prevented it from fulfilling the request."]
     ;;
     "temporarily_unavailable"
     ["The authorization server is currently unable to handle"
      "the request due to a temporary overloading or maintenance"
      "of the server."]}))


;; If the request fails due to a missing, invalid, or mismatching
;; redirection URI, or if the client identifier is missing or invalid,
;; the authorization server SHOULD inform the resource owner of the
;; error and MUST NOT automatically redirect the user-agent to the
;; invalid redirection URI.
(defn authorization-request
  [impl {:keys [response-type username password]
         :as request}]
  (let [now (System/currentTimeMillis)
        session (nano-id 20)]
    (case response-type
      ("code" "token")
      (try
        (set-session impl session
                     {:request request
                      :at now})
        (let [client (validate-client impl session)]
          ;; Proper implementation
          (set-session-client impl session client)
          (redirect-login impl session))
        (catch clojure.lang.ExceptionInfo ex
          (request-error impl session (ex-data ex))))
      "password" (try
                   (let [user (validate-resource-owner impl username password)]
                     (do
                       (set-session-resource-owner impl session user)
                       (remove-session impl session)
                       (return-token impl session)))
                   (catch clojure.lang.ExceptionInfo ex
                     (remove-session impl session)
                     (request-error impl session (ex-data ex))))
      "client_credentials"
      (try
        (validate-client impl session)
        (return-token impl session)
        (catch clojure.lang.ExceptionInfo ex (request-error impl session (ex-data ex)))))))


(defn handle-login
  [impl {:keys [username password session]}]
  (if-let [{{:keys [response-type] :as request} :request} (get-session impl session)]
    (try
      (let [client (validate-client impl session)]
        ;;
        (if-some [resource-owner (validate-resource-owner impl username password)]
          ;;
          (case response-type
            "code" (doto impl
                     (set-session-resource-owner session resource-owner)
                     (return-code session))
            "token" (doto
                      impl 
                      (set-session-resource-owner session resource-owner)
                      (return-token session))
            (throw
              (ex-info "Couldn't handle login for uknown response type"
                       {:session session
                        :username username
                        :resource-owner resource-owner
                        :client client
                        :request request})))
          ;;
          (throw
            (ex-info "This user isn't authorized"
                     {:session session
                      :username username
                      :client client
                      :request request
                      :type ::user-not-authorized}))))
      (catch clojure.lang.ExceptionInfo ex
        ()))
    (throw
      (ex-info "Couldn't find authorization request for input session"
               {:session session
                :type ::unknown-authorization-request}))))




;; Tokens
(defn access-token-request
  [impl request]
  )


(defn refresh-token-request
  [impl request]
  )
