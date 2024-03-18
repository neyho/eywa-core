(ns neyho.eywa.iam.oauth2.protocol)


(defprotocol SessionControl
  )


(defprotocol OAuthProtocol
  (redirect-login
    [this session]
    "Serves page for some session. Return http response that redirects user to login page")
  (login-error
    [this session data])
  (validate-client
    [this request]
    "Returns found client if client is valid or nil if client isn't valid")
  (validate-resource-owner
    [this username password]
    "Returns resource owner data if credentials are ok or nil if credentials aren't ok")
  (validate-refresh-token
    [this token])
  (validate-authorization-token
    [this token])
  (request-error
    [this session data])
  (token-error
    [this session data])
  (return-token
    [this session]
    "Generates and returns token for username and password")
  (return-code
    [this session]
    "Returns authorization code that can be used to get first access/refresh token")
  ;;
  (get-redirection-uris [this session])
  ;;
  (set-session-client [this session client])
  (get-session-client [this session])
  (remove-session-client [this session])
  ;;
  (set-session-resource-owner [this session resource-owner])
  (get-session-resource-owner [this session])
  (remove-session-resource-owner [this session])
  ;;
  (get-session [this id] "Returns session data for id")
  (set-session [this id data] "Updates session data for id")
  (remove-session [this id] "Closes session"))
