(ns neyho.eywa.iam.ocid.azure
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

(def oauth2-authorization-endpoint "https://login.microsoftonline.com/96721954-b5c5-4a6b-8e03-879bdc60b95d/oauth2/v2.0/authorize")

(def oauth2-token-endpoint "https://login.microsoftonline.com/96721954-b5c5-4a6b-8e03-879bdc60b95d/oauth2/v2.0/token")


(def openid-endpoint "https://login.microsoftonline.com/96721954-b5c5-4a6b-8e03-879bdc60b95d/v2.0/.well-known/openid-configuration")


(defn get-tenant-keys
  ([tenant] (get-tenant-keys tenant "https://login.microsoftonline.com/%s/discovery/v2.0/keys"))
  ([tenant template]
   (json/read-str (slurp (format template tenant))))
   )
