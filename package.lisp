;;;; Copyright (c) Frank James 2015 <frank.a.james@gmail.com>
;;;; This code is licensed under the MIT license.


(defpackage #:frpc
  (:use #:cl)
  (:export #:use-rpc-program
	   #:use-rpc-host
	   #:defrpc
	   #:find-handler
	   #:generate-program-number

	   ;; type definitions
	   #:defxtype
	   #:defxunion
	   #:defxenum
	   #:defxstruct	   
	   #:defxtype*
	   #:read-xtype
	   #:write-xtype
	   #:xtype-reader
	   #:xtype-writer

	   ;; enums/unions
	   #:enum
	   #:enump
	   #:make-xunion
	   #:xunion-tag
	   #:xunion-val

	   ;; functions for type definitions
	   #:defreader
	   #:defwriter
	   #:with-reader
	   #:with-writer

	   ;; serializing to/from buffer 
	   #:pack
	   #:unpack

	   ;; client
	   #:rpc-connect
	   #:rpc-close
	   #:with-rpc-connection
	   #:call-rpc
	   #:*rpc-host*
	   #:*rpc-port*
	   #:*rpc-msgid*
	   #:make-msgid
	   #:call-create-gss-context 

	   ;; server
	   #:make-rpc-server
	   #:start-rpc-server
	   #:run-rpc-server
	   #:stop-rpc-server
	   
	   ;; specials that are binded in the context of a handler
	   #:*rpc-remote-host*
	   #:*rpc-remote-port*
	   #:*rpc-remote-protocol*
	   #:*rpc-remote-auth*

	   ;; errors
	   #:rpc-error
	   #:rpc-accept-error
	   #:rpc-prog-mismatch-error
	   #:rpc-timeout-error
	   #:rpc-auth-error
	   #:rpc-mismatch-error

	   ;; structures
	   #:make-opaque-auth
	   #:opaque-auth-flavour
	   #:opaque-auth-data
	   #:make-unix
	   #:auth-unix-stamp
	   #:auth-unix-machine-name
	   #:auth-unix-uid
	   #:auth-unix-gid
	   #:auth-unix-gids

	   ;; debug logging
	   #:*frpc-log*
	   #:*frpc-log-path*
	   #:frpc-log

	   ;; for gss security
	   #:des-init
	   #:gss-init

	   ))

