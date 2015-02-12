

(defpackage #:frpc
  (:use #:cl)
  (:export #:with-rpc-program
	   #:with-rpc-version
	   #:defrpc
	   #:defhandler
	   #:defxtype
	   #:defxunion
	   #:defxenum
	   #:defxstruct
	   #:defxtype*
	   #:start-rpc-server
	   #:stop-rpc-server))

   
