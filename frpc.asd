

(asdf:defsystem :frpc
  :name "FRPC"
  :author "Frank James <frank.a.james@gmail.com>"
  :description "ONC RPC"
  :license "MIT"
  :components
  ((:file "package")
   (:file "xdr" :depends-on ("package"))
   (:file "rpc" :depends-on ("xdr"))
   (:file "pmapper" :depends-on ("rpc"))
   (:file "client" :depends-on ("rpc"))
   (:file "server" :depends-on ("rpc")))
  :depends-on (:alexandria :nibbles :flexi-streams :babel :usocket))
