(ns neyho.eywa.iam.ocid.auth0
  (:require
    [clojure.data.json :as json]
    [buddy.sign.jws :as jws]
    [buddy.sign.jwt :as jwt]
    [buddy.sign.jwk :as jwk]
    [buddy.core.keys :as ks]
    [neyho.eywa.iam.oauth2.client :as client])
  (:import
    [java.math BigInteger]
    [java.security.spec RSAPublicKeySpec]
    [java.security KeyFactory]))

(def oauth2-authorization-endpoint "")

(def oauth2-token-endpoint "https://dev-e1h3f0elzkc5z82h.us.auth0.com/oauth/token")


(defn get-tenant-keys
  ([tenant] (get-tenant-keys tenant "https://%s.us.auth0.com/.well-known/jwks.json"))
  ([tenant template]
   (json/read-str (slurp (format template tenant))))
   )
