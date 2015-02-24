;;;; Copyright (c) Frank James 2015 <frank.a.james@gmail.com>
;;;; This code is licensed under the MIT license.


(defpackage #:frpc
  (:use #:cl)
  (:export #:with-rpc-program
	   #:use-rpc-program
	   #:defrpc
	   #:defhandler
	   #:find-handler

	   ;; type definitions
	   #:defxtype
	   #:defxunion
	   #:defxenum
	   #:defxstruct	   
	   #:defreader
	   #:defwriter
	   #:defxtype*
	   #:read-xtype
	   #:write-xtype

	   ;; enums/unions
	   #:enum
	   #:enump
	   #:make-xunion
	   #:xunion-tag
	   #:xunion-val

	   ;; local type definitions
	   #:with-reader
	   #:with-writer

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
	   #:*rpc-msgid*
	   #:make-msgid

	   ;; server
	   #:make-rpc-server
	   #:start-rpc-server
	   #:stop-rpc-server
	   #:wait-for-reply	   
	   ))

;; package for the port mapper program. we define it here so that we have access to the 
;; external symbols in frpc itself. this is needed in server.lisp to add the port mappings
;; when starting a server   
(defpackage #:port-mapper
  (:use #:cl #:frpc)
  (:nicknames #:pmap)
  (:export #:mapping
	   #:make-mapping
	   #:mapping-program
	   #:mapping-version
	   #:mapping-protocol
	   #:mapping-port
	   ;; the rpc functions
	   #:*pmap-port*
	   #:call-null
	   #:call-set
	   #:call-unset
	   #:call-get-port
	   #:call-dump
	   #:call-callit
	   ;; underlying API
	   #:add-mapping
	   #:add-all-mappings
	   #:rem-mapping
	   #:find-mapping))
