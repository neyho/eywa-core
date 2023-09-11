; (ns neyho.eywa.dataset.pgp
;   (:require
;     [clojure.java.io :as io]
;     [clj-pgp.core :as pgp]
;     [clj-pgp.generate :as pgp-gen]
;     [clj-pgp.message :as pm]
;     [clj-pgp.keyring :as keyring])
;   (:import
;     [org.bouncycastle.openpgp 
;      PGPPublicKeyRingCollection
;      PGPSecretKeyRingCollection]))

; (def rsa (pgp-gen/rsa-keypair-generator 2048))
; (def ec (pgp-gen/ec-keypair-generator "secp160r2"))


; (def public-keys (atom nil))
; (def private-keys (atom nil))

; (defn generate-keyring [username password]
;   (pgp-gen/generate-keys
;     username
;     password
;     (master-key
;       (keypair rsa :rsa-general)
;       (prefer-symmetric :aes-256 :aes-128)
;       (prefer-hash :sha512 :sha256 :sha1)
;       (prefer-compression :zlib :bzip2))
;     (signing-key
;       (keypair rsa :rsa-general)
;       (expires 0))
;     (encryption-key
;       (keypair rsa :rsa-general))))

; (defn add-keyring 
;   ([collection keyring]
;    (assoc )))

; (defn load-keys []
;   ())

; (comment
;   (pgp-gen/generate-keypair ec :ecdsa)
;   (pgp/key-id (:public default))
;   (generate-keyring "masterblaset" "r3D!kul$")
;   (def default 
;     )
;   (def message
;     (pm/encrypt "mater ti" (:public default)
;                 :format :utf8
;                 :cipher :aes-256
;                 :compress :zip
;                 :armor true))
;   (pgp/key-info (pgp/public-key (:public default)))
;   (pgp/unlock-key (first (:secret default)) "r3D!kul$")
;   (->
;     (:public default)
;     first
;     pgp/encode-ascii)
;   (map pgp/key-info (:secret default))

;   (def eywa-public (keyring/load-public-keyring "eywa/administration/assets/eywa_public.gpg"))
;   (seq eywa-public)

;   (with-open [o (io/make-output-stream)]
;     (io/copy (.getEncoded (:public default)) (io/file "eywa/administration/assets/eywa_public.gpg"))
;     (io/copy o (io/file "eywa/administration/assets/eywa_public.gpg")))
;   (.encode (:public default) (io/output-stream (io/file "eywa/administration/assets/eywa_public.gpg")))
;   (.encode (:secret default) (io/output-stream (io/file "eywa/administration/assets/eywa_private.gpg")))
;   (pgp/encode-ascii (:secret default))
;   (pm/decrypt message (pgp/unlock-key (:secret default) "r3D!kul$")))
