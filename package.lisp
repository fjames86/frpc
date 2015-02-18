;;;; Copyright (c) Frank James 2015 <frank.a.james@gmail.com>
;;;; This code is licensed under the MIT license.


(defpackage #:frpc
  (:use #:cl)
  (:export #:with-rpc-program
	   #:use-rpc-program
	   #:defrpc
	   #:defhandler

	   ;; type definitions
	   #:defxtype
	   #:defxunion
	   #:defxenum
	   #:defxstruct	   
	   #:defreader
	   #:defwriter
	   #:defxtype*

	   ;; enums/unions
	   #:enum
	   #:enump
	   #:make-xunion
	   #:xunion-tag
	   #:xunion-val

	   ;; local type definitions
	   #:with-reader
	   #:with-writer
	   #:with-reader/writer

	   ;; serializing to/from buffer 
	   #:pack
	   #:unpack

	   ;; for testing/debugging
;;	   #:with-local-stream
;;	   #:with-local-server

	   ;; client
	   #:rpc-connect
	   #:rpc-close
	   #:with-rpc-connection
	   #:call-rpc-server
	   #:call-rpc
	   #:*rpc-host*
	   #:*rpc-port*

	   ;; server
	   #:start-rpc-server
	   #:stop-rpc-server
	   ))

   
