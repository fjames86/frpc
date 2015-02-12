

(asdf:defsystem :frpc
  :name "FRPC"
  :author "Frank James <frank.a.james@gmail.com>"
  :description "ONC RPC"
  :license "MIT"
  :components
  ((:file "package")
   (:file "xdr" :depends-on ("package"))
   (:file "rpc" :depends-on ("xdr"))
   (:file "errors" :depends-on ("rpc"))
   (:file "pmapper" :depends-on ("rpc"))
   (:file "client" :depends-on ("errors"))
   (:file "server" :depends-on ("errors")))
  :depends-on (:alexandria :nibbles :flexi-streams :babel :usocket))
