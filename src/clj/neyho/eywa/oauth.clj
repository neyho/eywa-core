(ns neyho.eywa.oauth)


;; Client types
;; :confidential - controled or trusted servers
;; :public - client on resource owner device(s)

;; 2.3.1
;; Client password
;; - server mora podrzavati HTTP Basic autentikaciju klijenata: https://datatracker.ietf.org/doc/html/rfc2617
;; - Moze i form urlencoded gdje su body parametri client_id i client_secret,
;; al treba izbjegavati


;; 3. dva HTTP endpointa
;; * Authorization - client -> server: da bi se autorizirao
;; * Token endpoint - client -> server: authorization-grant za access token
;;    i jedan client endpoint
;; * Redirection endpoint


;; 3.1 Authorization Endpoint
;; GET je za implementaciju, a POST opcionalno

;; 3.1.1 Response Type
;; Client treba poslati response_type
;; prema auth serveru i treba biti jedan od:
;; * code - authorization code
;; * token
;; * registered extension

;; Token requests must be POST


(defrecord Client [id name description type redirect-urls])


;; 3.3 Acess Token Scope
;; nesto o scopu, kak moze imati vise scopeova i vazno da mora imati scope
;; u requestu


;; Request moze imati neki od grant_type
;; * authorization code
;; * implicit
;; * resource owner password credentials
;; * client credentials



;; OpenID Connect
;;

(defrecord OpenIDToken [iss sub aud exp iat auth_time nonce acr amr azp])
