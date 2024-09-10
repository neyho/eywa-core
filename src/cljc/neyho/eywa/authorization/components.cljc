(ns neyho.eywa.authorization.components
  (:require  
    [clojure.string :as str]
    [clojure.zip :as zip]))



;; IAM
(def eywa #uuid "28d8b0bf-7647-4ff4-b799-fc72b22c06f0")
(def graphql-ui #uuid "d9ae32dc-9a39-4353-b0d4-bfe9031b127a")
(def iam #uuid "52e64d64-867c-4827-a2d8-c96a9ae173d8")
(def users #uuid "8500caf8-edde-411e-9079-cd06de4f870e") 
(def user-add #uuid "e93b302b-4f37-43a1-9d4b-f41435907ce7") 
(def user-modify #uuid "8a046ab5-3f18-4bb2-bacf-a0b3ef6616cc") 
(def user-delete #uuid "6734e2ae-45fe-470b-a9ac-68551ca64b36") 
(def groups #uuid "9a7927a4-2ae7-4f5d-8c09-a4ddcdc11645") 
(def group-add #uuid "c3c53259-5535-41b2-a7f7-a325107af0d7") 
(def group-modify #uuid "6b01fc38-dd3e-4139-9bb2-01b30355d69e") 
(def group-delete #uuid "bf0617dc-d522-4bc4-a382-15743310c1ea") 
(def group-members #uuid "9f7c16fd-68c6-4fae-90ff-69fba108d9b9") 
(def services #uuid "70ff2fce-c212-4aab-875c-72fb941c1ccd") 
(def service-add #uuid "5b3439d3-27ae-43f7-a10b-0244849a41f8") 
(def service-modify #uuid "6d0202ad-be77-4785-af83-8b4796b83da7") 
(def service-delete #uuid "4b72d28c-2c0d-47a6-acb2-22ba8b888622") 
(def roles #uuid "e2d6aac4-67bb-4242-96cb-b45cb0f0aa71") 
(def role-add #uuid "2f352f5c-f17c-4072-affa-2cc2a9e9b024") 
(def role-modify #uuid "e7a02410-084a-48fe-baac-c887634c79dc") 
(def role-delete #uuid "67f5108f-ee9b-4564-8cad-bba6bfc9643d") 
(def role-members #uuid "01340f6e-c948-4455-9490-58d6ff5e43d4") 
(def role-permissions #uuid "6d321670-6324-4a75-a35a-5477f64e18e9") 
(def apps #uuid "8d6e541b-2a11-459b-8ab0-0bc865e6ced7")
(def app-add #uuid "2c12154b-5b8b-4622-84eb-19c669d6ebaa")
(def app-edit #uuid "f8c18fcb-8f1f-4672-8275-1b36f1d67859")
(def app-delete #uuid "5f9d5fc4-18e0-4fa1-9e76-acd0ba03ec73")



;; INFRASTRUCTURE
(def reacher #uuid "0c8f95ce-374e-496e-aad6-c0d03faa6825") 
(def reacher-add #uuid "63a1ec19-edc4-4df4-96db-6761921a3560") 
(def reacher-modify #uuid "88e4a52b-e8d1-4363-8cad-1858326e30c1") 
(def reacher-delete #uuid "a7b731ed-db22-4b2f-a917-2e576e89454d")

(def git #uuid "bf9ae3f5-d425-4388-8ebb-c67ac3b48af8")
(def git-add #uuid "ca30a2d4-b8a3-4640-a955-d9a26e064772")
(def git-modify #uuid "228e7dd0-cbb3-47bc-8e91-472a89001354")
(def git-delete #uuid "8877fa06-91f5-405d-99ab-0488ddd09833")


(def storage #uuid "49cf0c7c-3eb4-48b1-9ebd-d453b7338749")
(def storage-add #uuid "fff90b1e-f20e-48a8-b412-9100c35f56d0")
(def storage-remove #uuid "c09d27ec-6fa9-40d7-b57b-371c58c6e5c7")
(def storage-modify #uuid "5d191a60-40c3-4da3-a942-89b8f3f180a4")

(def infrastructure #uuid "64baa095-29fa-4bad-be16-8d50b466871f")
(def processes #uuid "ef21cc1d-b35a-4f02-8e47-458582560e99")
(def process-add #uuid "2d332dca-5486-4d9c-9a8e-98f83a572315")
(def process-modify #uuid "889eaddb-3594-49ce-9d7c-b9423366a3ad")
(def process-delete #uuid "715033f3-a970-4f3d-b2a4-a966e6f29f04")
(def process-deploy #uuid "7ed2c675-10b4-460c-a9fe-c63345f20d49")


;; Service Control Center
(def scc #uuid "eacfa794-c45d-430b-8479-c03cdf14b3e8")
(def scc-edit #uuid "249e3357-6241-4d06-9b4f-65cfbf182656")
(def scc-add-location #uuid "98d49936-255f-45fe-877e-1739a3137166")
(def scc-remove-location #uuid "da5503c8-ad4b-404d-be47-cf0376994c08")
(def scc-prioritize #uuid "6a6f5a4e-aebe-4dc7-a03d-ab81cb6ab5f4")


;; Datasets
(def data #uuid "10c05ba4-de95-4f04-a90b-a2b1081a030a")
(def datasets #uuid "84447fe2-673e-4d39-8eda-aeb1fb54a7ca")
(def dataset-explorer #uuid "c466179d-a9be-4b9c-8a3c-ac368d09cae3") 
(def dataset-add #uuid "27209429-fe7c-469f-96ed-65faec81afc4")
(def dataset-modify #uuid "730bef1a-0081-4900-a319-bd20a54892ce")
(def dataset-delete #uuid "11786a5a-244b-4f16-b9a3-d482cb70bf56")
(def dataset-deploy #uuid "40308258-3463-4bf2-bd2f-7338504fdc4a")
(def dataset-version #uuid "fe24b031-4199-4af2-a917-137125209f0c")
(def dataset-version-import #uuid "b41ee692-168b-4260-8f67-3d739bcda184")
(def dataset-version-export #uuid "53079c0b-0693-4d63-aed0-37387271b344")
(def dataset-version-save #uuid "54bce61b-5089-4e5c-911a-756189abd251")
(def dataset-version-delete #uuid "b06d44ae-9de6-411b-9aff-a4a92069c744")


;; Robotics
(def robotics #uuid "08b9283e-d06c-44a0-84a5-4f9187f27389")
(def robotics-reacher #uuid "dde282e2-0e8f-4a09-9997-6883f9fa4dcc")
(def robotics-reacher-add #uuid "344b4455-a3d1-4b30-979c-b01626ca6c03")
(def robotics-reacher-edit #uuid "98b42b5b-6580-4698-9dfa-c24881257d9d")
(def robotics-reacher-delete #uuid "6db67f7d-65ba-4c55-8462-2fbdc432c369")
;;
(def robotics-pools #uuid "2200cc35-8205-4b92-bf26-204f746bbb31")
(def robotics-pool-add #uuid "825e006a-2d23-4f5a-ab3c-40977a7d6a93")
(def robotics-pool-edit #uuid "188c77ad-b112-404c-bcfa-71e573cffea0")
(def robotics-pool-delete #uuid "1b09df0f-81c4-4c8d-aed8-41fb2b13941a")
;;
(def robotics-robots #uuid "db7a7fcb-7e6a-4566-9274-750dd4ce387c")
(def robotics-robots-add #uuid "b6c29e34-c65d-4d09-9615-e2cca6b05362")
(def robotics-robots-edit #uuid "3ec373c4-0064-4916-92ed-fa2ecf0d85e9")
(def robotics-robots-delete #uuid "371d1a6a-8065-4963-a747-e0834a859e55")
(def robotics-robots-launch #uuid "4c7e8df9-1f4a-4004-988c-5c0a3e6ff525")
(def robotics-robots-control #uuid "6c74a6b1-5538-40f6-b3ef-618887ba3ab3")
(def robotics-robot-task-cancel #uuid "5b47f538-0163-4783-ad37-e37bab3e5571")
(def robotics-robots-log #uuid "5120d29a-79af-48d5-8e12-0cf866ca7f05")
;;
(def robotics-reports #uuid "f788cc51-edc2-4233-b77c-13be7f05de55")
(def robotics-reports-filter #uuid "bd4367db-0a0d-4d16-993c-5b384029c1d7")
(def robotics-reports-export #uuid "69c11669-88bd-4e54-bc21-3b1e665f6e04")
(def robotics-reports-log #uuid "61f2fdd3-09ee-4e03-af07-478b66e9b4c7")
;;



;; Flutter
(def flutter #uuid "0611e967-1ce7-47bd-a6f8-36c1ad5b4616")
(def flutter-devices #uuid "972e4068-ab61-47f1-8111-a2ebb3f5b4e6")
(def flutter-testing #uuid "4384b724-bba7-414f-87c5-c853c56d99f8")
(def flutter-reports #uuid "1ecb74c6-6a12-4b17-a767-7e73851686fb")

(comment 
  (java.util.UUID/randomUUID)
  (def euuid)
  (def components *1))



(defn components->tree 
  ([components {:keys [euuid]}]
   (when-let [node (some #(when (= euuid (:euuid %)) %) components)]
     (let [children (filter (comp #{euuid} :parent) components)
           components' (remove (comp #{euuid} :euuid) components)] 
       (if (seq children)
         (assoc
           (dissoc node :parent)
           :children (mapv #(components->tree components' %) children))
         (dissoc node :parent))))))


(defn component-tree-zipper [root]
  (zip/zipper
    :children
    :children
    (fn [node children] (assoc node :children (vec children)))
    root))


(defn tree->components
  ([tree]
   (sort-by 
     :parent
     (let [z (component-tree-zipper tree)]
       (loop [p z
              r []]
         (if (zip/end? p) r
           (recur
             (zip/next p)
             (let [n (zip/node p)
                   {puuid :euuid} (last (zip/path p))]
               (conj r (->
                         n
                         (dissoc :children)
                         (assoc :parent puuid)))))))))))


(defn get-component
  [tree euuid]
  (let [z (component-tree-zipper tree)]
    (loop [p z]
      (if (zip/end? p) nil
        (let [{euuid' :euuid :as n} (zip/node p)] 
          (if (#{euuid} euuid')
            n
            (recur (zip/next p))))))))

(defn component->position [tree euuid]
  (let [z (component-tree-zipper tree)]
    (loop [p z]
      (if (zip/end? p) nil
        (let [{euuid' :euuid} (zip/node p)] 
          (if (#{euuid} euuid')
            p
            (recur (zip/next p))))))))


(defn component->name [tree euuid]
  (let [z (component-tree-zipper tree)]
    (loop [p z]
      (if (zip/end? p) nil
        (let [{euuid' :euuid n :name} (zip/node p)] 
          (if (#{euuid} euuid')
            n
            (recur (zip/next p))))))))

(defn update-component! 
  "Updates component in component tree with id applying function f with additional
   arguments."
  [tree euuid f & args]
  (let [z (component-tree-zipper tree)]
    (loop [p z]
      (let [n (zip/node p)]
        (if (zip/end? p) 
          (zip/root p)
          ;; XXX - commented out to improve robustnes,.. Don't crash the application if
          (if (#{euuid} (:euuid n))
            (zip/root (apply zip/edit p f args))
            (recur (zip/next p))))))))

(defn normalize-tree
  [tree]
  (loop [p (component-tree-zipper tree)]
    (if (zip/end? p)
      (zip/node p)
      (recur 
        (zip/next
          (zip/edit 
            p 
            (fn [{roles :roles :as node}]
              (assoc node :roles (set roles)))))))))

(defn path->position 
  "Given access tree and component path as sequence of component names function
   returns node position in zipper for given path or throws exception if such component
   doesn't exist."
  [tree path]
  (let [z (component-tree-zipper tree)]
    (loop [position z
           path' path]
      (if (or (nil? position) (zip/end? position))
        (throw 
          (ex-info 
            "Node not found"
            {:tree tree
             :path path}))
        (let [{:keys [euuid]} (zip/node position)]
          (if (= (first path') euuid)
            (if (empty? (rest path'))
              position
              (recur (zip/down position) (rest path')))
            (recur (zip/right position) path')))))))




;; NEW COMPONENTS

(comment
  (reset! *component-tree*
          (component-tree-zipper
            {:id ::ROOT
             :segment ""
             :name nil
             :children []}))

  @*component-tree*
  (component->location @*component-tree* ::ROOT)
  (zip/root @*component-tree*)

  (set-component {:id :eywa/iam :name :eywa.iam :segment "iam" :roles #{} :scopes #{} :parent ::ROOT})
  (set-component {:id :eywa/robotics :name :eywa.robotics :segment "robotics" :parent ::ROOT})
  (set-component {:id :eywa.robotics/robots :name :eywa.robotics.robots :segment "robots" :parent :eywa/robotics})

  (def id :eywa.robotics/robots)
  (component-path :eywa.robotics/robots)
  (on-path? "roboics/robots" :eywa/robotics)
  )
