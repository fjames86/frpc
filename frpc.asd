;;;; Copyright (c) Frank James 2015 <frank.a.james@gmail.com>
;;;; This code is licensed under the MIT license.

(asdf:defsystem :frpc.xdr
  :name "frpc.xdr"
  :author "Frank James <frank.a.james@gmail.com>"
  :description "An XDR implementation."
  :license "MIT"
  :components
  ((:file "xdr"))
  :depends-on (:flexi-streams :nibbles :babel :alexandria))

(asdf:defsystem :frpc
  :name "frpc"
  :author "Frank James <frank.a.james@gmail.com>"
  :description "An ONC-RPC implementation."
  :license "MIT"
  :components
  ((:file "package")
   (:file "log" :depends-on ("package"))
   (:file "rpc" :depends-on ("log"))
   (:file "errors" :depends-on ("rpc"))
   (:file "streams")
   (:file "client" :depends-on ("errors" "streams"))
   (:file "server" :depends-on ("client"))
   (:file "port-mapper" :depends-on ("server")))
  :depends-on (:frpc.xdr :alexandria :nibbles :flexi-streams 
	       :usocket :bordeaux-threads :pounds :babel))

