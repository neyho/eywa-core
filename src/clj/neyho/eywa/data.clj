(ns neyho.eywa.data)


(def ^:dynamic *EYWA*
  {:euuid #uuid "c5a67922-351e-4ca3-95c2-fa52a7a3e2b5"
   :name "EYWA"
   :type :SERVICE
   :active true
   :modified_by {:euuid #uuid "c5a67922-351e-4ca3-95c2-fa52a7a3e2b5"}})


(def ^:dynamic *ROOT*
  {:euuid #uuid "601ee98d-796b-43f3-ac1f-881851407f34"
   :name "SUPERUSER"
   :active true})


(defn upload-eywa-avatar
  []
  ())
