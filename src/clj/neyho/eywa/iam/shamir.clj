(ns neyho.eywa.iam.shamir
  (:import
    (java.security SecureRandom)
    (java.util Random)
    (java.math BigInteger)))


(def prime-field (BigInteger. "340282366920938463463374607431768211297"))
; (def prime-field (BigInteger. "115792089237316195423570985008687907853269984665640564039457584007913129639937"))


(defn secure-random-int
  [upper-bound]
  (let [random-bytes (byte-array 32)
        _ (.nextBytes (SecureRandom.) random-bytes)
        random-int (BigInteger. 1 random-bytes)]
    (.mod random-int upper-bound)))


(defn random-coefficients
  "Generates secure random coefficients for a polynomial of degree t-1."
  [secret t]
  (cons secret (repeatedly (dec t) #(secure-random-int prime-field))))


(defn evaluate-polynomial
  "Evaluates the polynomial at a given x-value within the prime field."
  [coefficients x]
  (reduce
   (fn [acc [i coeff]]
     (.mod (.add acc (.multiply coeff (.pow (BigInteger/valueOf x) (long i)))) prime-field))
   BigInteger/ZERO
   (map-indexed vector coefficients)))


(defn create-shares
  "Creates shares from the secret with a minimum threshold of t shares."
  [secret n t]
  (let [coefficients (random-coefficients secret t)]
    (map #(vector % (evaluate-polynomial coefficients %)) (range 1 (inc n)))))


(defn modular-inverse
  "Calculates the modular inverse of a number within the prime field."
  [n prime-field]
  (.modPow n (.subtract prime-field BigInteger/TWO) prime-field))


(defn lagrange-interpolation
  "Reconstructs the secret using Lagrange interpolation within the prime field."
  [shares x]
  (reduce
    (fn [sum [xi yi]]
      (.mod (.add sum
                  (.multiply yi
                             (reduce
                               (fn [prod [xj _]]
                                 (if (= xi xj)
                                   prod
                                   (let [numinator (.subtract (BigInteger/valueOf x) (BigInteger/valueOf xj))
                                         denuminator (.subtract (BigInteger/valueOf xi) (BigInteger/valueOf xj))
                                         inverse-denuminator (modular-inverse denuminator prime-field)]
                                     (.mod
                                       #_(.multiply prod (.modInverse denuminator prime-field))
                                       (.multiply prod (.mod (.multiply numinator inverse-denuminator) prime-field))
                                       prime-field))))
                               BigInteger/ONE
                               shares)))
             prime-field))
    BigInteger/ZERO
    shares))


(defn reconstruct-secret
  "Reconstructs the secret from the shares."
  [shares]
  (lagrange-interpolation shares BigInteger/ZERO))


;; Usage Example
(comment
  (let [secret (BigInteger. 128 (Random.)) ;; Large integer secret
        n 5                                ;; Total number of shares
        t 3]                               ;; Minimum threshold of shares

    (def shares (create-shares secret n t))
    (println "Generated Shares:" shares)

    ;; Select a subset of shares for reconstruction
    (def selected-shares (take t shares))
    (def reconstructed-secret (reconstruct-secret (list (nth shares 0) (nth shares 3) (nth shares 4))))
    (println "Reconstructed Secret:" (= reconstructed-secret secret))))
