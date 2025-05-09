(ns neyho.eywa.health)

(defmulti doctor (fn [organ] organ))
