;;;; Copyright (c) Frank James 2015 <frank.a.james@gmail.com>
;;;; This code is licensed under the MIT license.

(asdf:defsystem :frpc
  :name "frpc"
  :author "Frank James <frank.a.james@gmail.com>"
  :description "An ONC-RPC implementation."
  :license "MIT"
  :version "1.3.1"
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
  :description "Provides AUTH-DES authentication flavour for frpc."
  :license "MIT"
  :components
  ((:module :auth
            :pathname "auth"
            :components 
	    ((:module :des
		      :pathname "des"
		      :components
		      ((:file "package")
		       (:file "database" :depends-on ("package"))
		       (:file "interface" :depends-on ("database"))
		       (:file "des" :depends-on ("database")))))))
  :depends-on (:frpc :ironclad))


(asdf:defsystem :frpc-gss
  :name "frpc-gss"
  :author "Frank James <frank.a.james@gmail.com>"
  :description "Provides AUTH-GSS (Kerberos) authentication support for frpc."
  :license "MIT"
  :depends-on (:frpc :cerberus))

