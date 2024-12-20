(ns neyho.eywa.server.ip
  (:require
   [clojure.string :as str])
  (:import
   [java.net InetAddress NetworkInterface]))

(defn reverse-dns-lookup [ip]
  (try
    (let [inet-addr (InetAddress/getByName ip)]
      (.getHostName inet-addr))
    (catch Exception e
      (println "An error occurred:" (.getMessage e)))))

(defn server-addresses
  []
  (for [interface (enumeration-seq (NetworkInterface/getNetworkInterfaces))
        addr (enumeration-seq (.getInetAddresses interface))]
    (let [[ip] (str/split (.getHostAddress addr) #"%")]
      ip)))

(def dns-hostname-regex
  #"^(?!-)(?:[a-zA-Z0-9-]{0,62}[a-zA-Z0-9]\.?)+[a-zA-Z]{2,63}$")

(defn server-dns-records
  []
  (keep
   (fn [ip]
     (if-let [dns (reverse-dns-lookup ip)]
       (when (re-find dns-hostname-regex dns)
         dns)))
   (server-addresses)))

(.getHostName (InetAddress/getLocalHost))

(comment
  (server-dns-records)
  (def addresses (server-addresses))
  (-> addresses (nth 10) reverse-dns-lookup)
  (map reverse-dns-lookup (server-addresses))
  (reverse-dns-lookup)
  (reverse-dns-lookup "18.184.133.211"))
