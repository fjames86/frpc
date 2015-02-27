

(asdf:defsystem :frpc
  :name "frpc"
  :author "Frank James <frank.a.james@gmail.com>"
  :description "An ONC/RPC implementation."
  :license "MIT"
  :components
  ((:file "package")
   (:file "xdr" :depends-on ("package"))
   (:file "rpc" :depends-on ("xdr"))
   (:file "errors" :depends-on ("rpc"))
   (:file "client" :depends-on ("errors"))
   (:file "server" :depends-on ("client"))
   (:file "port-mapper" :depends-on ("server")))
  :depends-on (:alexandria :nibbles :flexi-streams 
	       :usocket :bordeaux-threads :log4cl))

