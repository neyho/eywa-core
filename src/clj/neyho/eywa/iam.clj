(ns neyho.eywa.iam
  (:require
    clojure.string
    clojure.java.io
    clojure.pprint
    [vura.core :as ntime]
    [buddy.sign.jwt :as jwt]
    [buddy.hashers :as hashers]
    [buddy.core.keys :as keys]
    [buddy.sign.util :refer [to-timestamp]]
    [neyho.eywa.data :refer [*ROOT*]]
    [neyho.eywa.iam.uuids :as iu]
    [neyho.eywa.dataset
     :refer [get-entity
             sync-entity
             delete-entity]]
    [neyho.eywa.authorization
     :refer [get-role-schema]]
    [clojure.tools.logging :as log]
    [io.pedestal.interceptor.chain :as chain]
    [io.pedestal.http.body-params :as body-params])
  (:import
    [java.security KeyPairGenerator]))


(defonce ^:dynamic *private-key* nil)
(defonce ^:dynamic *public-key* nil)


(defn init-encryption
  [{:keys [public private]}]
  (if-not (keys/public-key? public) 
    (throw (ex-info "Unacceptable public key" {:key public}))
    (alter-var-root #'*private-key* (constantly private)))
  (if-not (keys/private-key? private)
    (throw (ex-info "Unacceptable private key" {:key private}))
    (alter-var-root #'*public-key* (constantly public))))


(defn init-default-encryption
  []
  (let [generator (KeyPairGenerator/getInstance "RSA")
        key-pair (.generateKeyPair generator)
        public (.getPublic key-pair)
        private (.getPrivate key-pair)]
    (init-encryption
      {:private private
       :public public})))


(defn sign-data
  "Function encrypts data that should be in map form and returns encrypted
  string."
  ([data] (sign-data
            data
            (->
              (ntime/date)
              ntime/date->value
              (+ ntime/day)
              ntime/value->date
              to-timestamp)))
  ([data valid]
   (jwt/sign
     data
     *private-key*
     {:alg :rs256
      :exp valid})))


(defn get-password [username]
  (:password
    (get-entity
      iu/user
      {:name username}
      {:password nil})))


(defn get-user-details [username]
  (->
    (get-entity
      iu/user
      {:name username}
      {:_eid nil
       :euuid nil
       :name nil
       :password nil
       :active nil
       :avatar nil
       :settings nil
       :groups [{:selections {:euuid nil}}]
       :roles [{:selections {:euuid nil}}]})
    (update :roles #(set (map :euuid %)))
    (update :groups #(set (map :euuid %)))))


(defn validate-password
  [user-password password-hash]
  (hashers/check user-password password-hash))


(defn jwt-token? [token]
  (= 2 (count (re-seq #"\." token))))


(defn get-client
  [name]
  (get-entity
    iu/user
    {:name name}
    {:euuid nil
     :name nil
     :type nil
     :active nil
     :password nil
     :settings nil}))


(defn add-client [{:keys [name password settings]}]
  (sync-entity
    iu/user
    (cond->
      {:name name
       :type :OAUTH_CLIENT
       :settings settings
       :active true}
      password (assoc :password password))))


(defn remove-client [{:keys [euuid]}]
  (delete-entity iu/user {:euuid euuid}))


(comment
  (time (get-client "oauth_test_confidential"))
  (sync-entity
    iu/user
    {:euuid #uuid "fdd36283-b476-404e-aafb-2c404443dbfc"
     :name "test_oauth"
     :type :PERSON
     :password "password"
     :active true})
  (sync-entity
    iu/user
    {:euuid #uuid "fdd36283-b476-404e-aafb-2c404443dbfc"
     :name "test_oauth"
     :type :PERSON
     :password "password"
     :active true})
  (get-client "oauth_test_confidential")
  (add-client
    {:euuid #uuid "3349f1ff-2118-4b3e-babf-a8b68b7e98df"
     :name "oauth_test_confidential"
     :password "testisi123$"
     :settings {:version 0
                :type "public"
                ; :login-page ["http://localhost:8080/eywa/login/default/"]
                :login-page "http://localhost:8080/login/kbdev/"
                :redirections ["http://localhost:8080/eywa/"
                               "http://localhost:8080/app/kbdev"]}})
  (add-client
    {:euuid #uuid "62972fcf-3cfe-4d34-baea-055308612a0d"
     :name "oauth_test_public"
     :password nil
     :settings {:version 0
                :type "public"
                :login-page "http://localhost:8080/login/kbdev"
                :redirections ["http://localhost:8080/eywa/"
                               "http://localhost:8080/app/kbdev"]}}))


;; Authorization endpoint


;; RESPONSE_TYPE
;; Kad se salje zahtjev od klijenta prema Authorization Endpointu
;; Potrebno je specificirati "response_type" koji moze biti "code" ili  "token"
;; I moguce je zatraziti response type-ova, kad se koriste extenzije


;; Redirection Endpoint
;; Potrebno je odraditi redirect na client applikaciju
;; Klijent ili ima registraciju, pa po tome znam gdje treba redirectati
;; resource ownera ili u samom authorization requestu pise gdje treba
;; redirectati nekog
;; Mora biti absolutni URI
;; I redirection uri dolazi kao x-form-urlencoded





;; Registration Requirements
;; Public clients MORAJU imati registrirani endpoint
;; Confidential clients utilizing implicit grant type


;; Grant Typovi
;; * authorization code - mora biti short lived i singleuse
;; * implicit
;; * resource owner password credentials
;; * client credentials 



;; Dynamic Configuration
;; Ako ima vise redirection URI-ja, ili je samo dijelomicno registriran
;; neki URI path ili ako nema redirection URI-a, onda klijent zatrazi
;; redirection na temelju request parametra, "redirect_uri"

;; Na error, vracamo error response bez redirecta


;; 3.1.2.5 Endpoint Content
;; Hmh, redirection client bi trebao kao redirectati na neko mijesto gdje nema 3rd
;; party scripti ili neke customizacije, jer tamo postoji mogucnost da ce neka skripta
;; procitati credentialse. Client bi trebao obraditi redirect response i onda opet
;; odraditi redirect na neki URL, al bez credentialsa


;; 3.2 Token Endpoint
;; Mora biti POST request
;; Koristi se u bilo kojoj varijanti osim u Implicit Grant varijanti


;; 3.2.1 Client Authentication
;; Treba povezati refresh tokene i authorization code-ove sa klijentom
;; Kod slucaja da je neki klijent kompromitiran, lakse je izmjeniti 
;; credentialse od klijenta, nego povuc sve refresh tokene
;; Tesko je rotirati cijeli set refresh tokena, al je lakse rotirati
;; klijent credentialse (valjda se credentialsi upisu kod buildanja aplikacije)
;; Opet, izgleda da ovaj dio vrijedi samo za situacije gdje nema implicit
;; grant_type-a


;; 3.3 Access Token Scope
;; 'scope' parametar se moze specificirati u authorization requestu,
;; pa onda authorization response moze vratiti 'scope' parametar
;; kojime se klijentu vraca info o tome koliki je scope izdanog access tokena
;;
;; Vrijednost scope tokena je odredena listom space-delimited case sensitive stringova
;; Authorization serveru je pusteno na volju da se brine za scope
;; tipa kaj se dogodi ako scope nije definiran, dal postoji neki defaultni
;; ili ce failat request zbog krivog scope parametra


;; 4.1 Authorization Code Grant
;; 4.1.1 Authorization Request
;; Klijent salje prema authorization endpointu request sa x-www-form-urlencoded
;; parametre
;; * response_type
;; * client_id
;; * redirect_uri
;; * scope
;; * state - (RECOMMENDED) Authorization server zadrzi ovu vrijednost kod redirecta
;; pa onda se moze ta vrijednost koristiti za csrf obranu
;; 
;;
;; 4.1.2 Authorization Response
;; Response opet vraca URL encoded briju sa parametrima
;; * code - authorization code koji je dodijeljen klijentu
;; * state - isti onaj state koji je dosao u requestu
;; status je 302, da oznaci da je redirect
;;
;;
;; 4.1.2.1 Error response
;; Detalji u RFC-u... Nema smisla kopirati
;; * error - code
;; * invalid_request -  ukljuci info sto nevalja, koji parametar je banana
;; * unauthorized_client - klijent nema prava traziti authorizaciju
;; * access_denied - Resource Owner ili autorizacijski server su odbili request
;; * unsupported_response_type - autorizacijski server ne podrzava response type
;; * invalid_scope - request scope nevalja
;; * server_error - autorizacijski server je naletion na error zbog kojeg nemre
;;   odraditi autorizaciju (error (code) = 500)
;; * temporarily_unavailable - Autorizacijski server nemre trenutno odraditi
;;   authentikaciju, probaj kasnije (error 503)
;; * error_description
;; * error_uri - (OPTIONAL) dokumentacija, sto se dogodilo i zkj netko nemre AT dobit
;; * state
;;
;;



;; 4.1.3 Authorization Request
;; * grant_type
;; * code
;; * redirect_uri
;; * client_id
;; Ak je potreban password, onda se klijent autenticira sa basic authenticationom


;; 4.1.4 Access Token Response
;; application/json; charset=UTF-8
;; Cache-Control: no-store
;; Pragma: no-cache
;;
;; {
;;        "access_token":"2YotnFZFEjr1zCsicMWpAA",
;;        "token_type":"example",
;;        "expires_in":3600,
;;        "refresh_token":"tGzv3JOkF0XG5Qx2TlKWIA",
;;        "example_parameter":"example_value"
;;      }


;; Hmh... Implicit grant doesn't support issuance of refresh tokens?
