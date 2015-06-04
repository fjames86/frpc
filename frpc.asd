;;;; Copyright (c) Frank James 2015 <frank.a.james@gmail.com>
;;;; This code is licensed under the MIT license.

(asdf:defsystem :frpc
  :name "frpc"
  :author "Frank James <frank.a.james@gmail.com>"
  :description "An ONC-RPC implementation."
  :license "MIT"
  :version "1.2.5"
  :components
  ((:file "package")
   (:file "log" :depends-on ("package"))
   (:file "xdr" :depends-on ("log"))
   (:file "rpc" :depends-on ("xdr"))
   (:file "unix" :depends-on ("rpc"))
   (:file "gss" :depends-on ("rpc"))
   (:file "errors" :depends-on ("rpc"))
   (:file "streams")
   (:file "client" :depends-on ("errors" "streams" "gss"))
   (:file "bind" :depends-on ("client"))
   (:file "server" :depends-on ("client" "gss" "bind"))
   (:file "extras" :depends-on ("bind")))
  :depends-on (:alexandria :nibbles :flexi-streams 
	       :usocket :bordeaux-threads :pounds :babel :glass))

(asdf:defsystem :frpc-des
  :name "frpc-des"
  :author "Frank James <frank.a.james@gmail.com>"
  :description "Provides AUTH_DES authentication flavour for frpc."
  :license "MIT"
  :components
  ((:module :auth
            :pathname "auth"
            :components 
            ((:file "keyserv")
             (:file "des" :depends-on ("keyserv"))
             (:file "keyserv-server" :depends-on ("des")))))
  :depends-on (:frpc :ironclad))


(asdf:defsystem :frpc-gss
  :name "frpc-gss"
  :author "Frank James <frank.a.james@gmail.com>"
  :description "Provides AUTH_GSS (Kerberos) authentication support for frpc."
  :license "MIT"
  :depends-on (:frpc :cerberus))

