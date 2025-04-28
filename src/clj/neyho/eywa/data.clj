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

(def ^:dynamic *PUBLIC_ROLE*
  {:euuid #uuid "746a7348-4daf-4b5a-921a-efc8bd476d88"
   :name "Public"})

(def ^:dynamic *PUBLIC_USER*
  {:euuid #uuid "762d9076-4b78-4918-9eec-262a56a94e95"
   :name "__public__"
   :active false})

(defn upload-eywa-avatar
  []
  ())
