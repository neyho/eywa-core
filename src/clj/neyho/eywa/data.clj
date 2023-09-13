(ns neyho.eywa.data
  (:require
    [neyho.eywa.authorization.components :as c]))


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


; (def ^:dynamic *ROOT*
;   {:euuid #uuid "601ee98d-796b-43f3-ac1f-881851407f34"
;    :name "SUPERUSER"
;    :active true
;    :permissions
;    [{:euuid c/eywa
;      :name "Eywa"
;      :parent nil}
;     {:euuid c/administration
;      :name "Workspace"
;      :parent {:euuid c/eywa}}
;     {:euuid c/users
;      :name "Users"
;      :parent {:euuid c/administration}}
;     {:euuid c/user-add
;      :name "Add"
;      :parent {:euuid c/users}}
;     {:euuid c/user-modify
;      :name "Modify"
;      :parent {:euuid c/users}}
;     {:euuid c/user-delete
;      :name "Delete"
;      :parent {:euuid c/users}}
;     {:euuid c/groups
;      :name "Groups"
;      :parent {:euuid c/administration}}
;     {:euuid c/group-add
;      :name "Add"
;      :parent {:euuid c/groups}}
;     {:euuid c/group-modify
;      :name "Modify"
;      :parent {:euuid c/groups}}
;     {:euuid c/group-members
;      :name "Members"
;      :parent {:euuid c/groups}}
;     {:euuid c/group-delete
;      :name "Delete"
;      :parent {:euuid c/groups}}
;     {:euuid c/roles
;      :name "Roles"
;      :parent {:euuid c/administration}}
;     {:euuid c/role-add
;      :name "Add"
;      :parent {:euuid c/roles}}
;     {:euuid c/role-modify
;      :name "Modify"
;      :parent {:euuid c/roles}}
;     {:euuid c/role-members
;      :name "Members"
;      :parent {:euuid c/roles}}
;     {:euuid c/role-permissions
;      :name "Permissions"
;      :parent {:euuid c/roles}}
;     {:euuid c/role-delete
;      :name "Delete"
;      :parent {:euuid c/roles}}]})
