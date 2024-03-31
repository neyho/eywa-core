(ns neyho.eywa.iam.oidc.azure
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

(def oauth2-authorization-endpoint "https://github.com/login/oauth/authorize")

(def oauth2-token-endpoint "https://github.com/login/oauth/access_token")


(defn get-tenant-keys
  ([tenant] (get-tenant-keys tenant "https://login.microsoftonline.com/%s/discovery/v2.0/keys"))
  ([tenant template]
   (json/read-str (slurp (format template tenant))))
   )
