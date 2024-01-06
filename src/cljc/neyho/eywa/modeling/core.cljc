(ns neyho.eywa.modeling.core)


(defrecord Coordinate [x y])

(defrecord PathSegment [c1 c2])

(defrecord Path [coordinates])
