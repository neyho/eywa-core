(ns neyho.eywa.db)


(defonce ^:dynamic *db* nil)


(defprotocol ModelQueryProtocol
  (sync-entity
    [this entity-id data]
    "Sync entity takes dataset entity id and data and
    synchronizes DB with current state. This includes
    inserting/updating new records and relations as
    well as removing relations that were previously linked
    with input data and currently are not")
  (stack-entity
    [this entity-id data]
    "Stack takes dataset entity id and data to stack
    input data on top of current DB state")
  (slice-entity
    [this entity-id args selection]
    "Slice takes dataset entity id and data to slice
    current DB state based on input data effectively deleting
    relations between entities")
  (get-entity
    [this entity-id args selection]
    "Takes dataset entity id, arguments to pinpoint target row
    and selection that specifies which attributes and relations
    should be returned")
  (get-entity-tree
    [this entity-id root on selection]
    "Takes dataset entity id, root record and constructs tree based 'on'. 
    Selection that specifies which attributes and relations
    should be returned")
  (search-entity
    [this entity-id args selection]
    "Takes dataset entity id, arguments to pinpoint target rows
    and selection that specifies which attributes and relations
    should be returned")
  (search-entity-tree
    [this entity-id on args selection]
    "Takes dataset entity id, arguments to pinpoint target rows
    based 'on' recursion and selection that specifies which attributes and relations
    should be returned")
  (purge-entity
    [this entity-id args selection]
    "Find all records that match arguments, delete found records 
    and return deleted information based on selection input")
  (aggregate-entity
    [this entity-id args selection]
    "Takes dataset entity id, arguments and selection to return aggregated values
    for given args and selection. Possible fields in selection are:
    * count
    * max
    * min
    * avg")
  (aggregate-entity-tree
    [this entity-id on args selection]
    "Takes dataset entity id 'on' recursion with arguments 
    and selection to return aggregated values for given args and selection. 
    Possible fields in selection are:
    * count
    * max
    * min
    * avg")
  (verify-hashed
    [this entity-id args]
    "Function takes entity id and arguments to verify if argument fields match
    encrypted data")
  (delete-entity
    [this entity-id data]
    "Function takes dataset entity id and data to delete entities from
    from DB"))
