(ns neyho.eywa.iam.gen
  (:require
    [nano-id.core :refer [nano-id] :as nano-id]))



(let [alphabet "ACDEFGHIJKLMNOPQRSTUVWXYZ"]
  (def client-id (nano-id/custom alphabet 48)))


(let [alphabet "ACDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz1234567890-"]
  (def client-secret (nano-id/custom alphabet 48)))
