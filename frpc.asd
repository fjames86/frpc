

(asdf:defsystem :frpc
  :name "frpc"
  :author "Frank James <frank.a.james@gmail.com>"
  :description "Frank's ONC RPC library."
  :license "MIT"
  :components
  ((:file "package")
   (:file "xdr" :depends-on ("package"))
   (:file "rpc" :depends-on ("xdr"))
   (:file "errors" :depends-on ("rpc"))
   (:file "log" :depends-on ("package"))
   (:file "server" :depends-on ("errors" "log"))
   (:file "client" :depends-on ("server")))
  :depends-on (:alexandria :nibbles :flexi-streams 
	       :babel :usocket :bordeaux-threads
	       :log4cl))

