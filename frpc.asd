;;;; Copyright (c) Frank James 2015 <frank.a.james@gmail.com>
;;;; This code is licensed under the MIT license.

(asdf:defsystem :frpc
  :name "frpc"
  :author "Frank James <frank.a.james@gmail.com>"
  :description "An ONC-RPC implementation."
  :license "MIT"
  :version "1.2.3"
  :components
  ((:file "package")
   (:file "log" :depends-on ("package"))
   (:file "xdr" :depends-on ("log"))
   (:file "gss" :depends-on ("xdr"))
   (:file "des" :depends-on ("gss"))
   (:file "rpc" :depends-on ("xdr" "gss" "des"))
   (:file "errors" :depends-on ("rpc"))
   (:file "streams")
   (:file "client" :depends-on ("errors" "streams" "gss"))
   (:file "bind" :depends-on ("client"))
   (:file "server" :depends-on ("client" "gss" "bind"))
   (:file "extras" :depends-on ("bind")))
  :depends-on (:alexandria :nibbles :flexi-streams :ironclad
	       :usocket :bordeaux-threads :pounds :babel :glass))


