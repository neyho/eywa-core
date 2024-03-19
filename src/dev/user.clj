;; First load properties from eywa.properties file
;; if any so that environ can pick up those entries
(require '[neyho.eywa.properties])
(neyho.eywa.properties/load-properties)
(require 'server)
(require 'login.css.compile)
(login.css.compile/start)


(server/-main)
