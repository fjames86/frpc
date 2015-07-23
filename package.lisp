;;;; Copyright (c) Frank James 2015 <frank.a.james@gmail.com>
;;;; This code is licensed under the MIT license.


(defpackage #:frpc
  (:use #:cl)
  (:export #:use-rpc-program
	   #:use-rpc-host
	   #:defrpc
;;	   #:defhandler
	   #:find-handler
	   #:generate-program-number
	   #:defprogram
	   #:find-program
	   #:list-all-programs
	   #:program-id 

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
	   #:read-xtype-list
	   #:write-xtype-list

	   ;; runtime XDR parsing 
	   #:read-xdr-object
	   #:write-xdr-object 
	   #:make-xdr-reader
	   #:make-xdr-writer 

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

	   ;; connections 
	   #:rpc-connect
	   #:rpc-close
	   #:with-rpc-connection

	   ;; client
	   #:rpc-client 
	   #:call-rpc
	   #:*rpc-host*
	   #:*rpc-port*

	   ;; server
	   #:make-rpc-server
	   #:start-rpc-server
	   #:run-rpc-server
	   #:stop-rpc-server
	   
	   ;; specials that are bound in the context of a handler
	   #:*rpc-remote-host*
	   #:*rpc-remote-port*
	   #:*rpc-remote-protocol*
	   #:*rpc-remote-auth*
	   #:rpc-auth-principal

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

	   ;; unix auth
	   #:unix-client
	   #:auth-unix
	   #:auth-unix-stamp
	   #:auth-unix-machine-name
	   #:auth-unix-uid
	   #:auth-unix-gid
	   #:auth-unix-gids
	   #:get-unix-creds
	   #:unix-init
	   
	   ;; debug logging
	   #:*frpc-log*
	   #:*frpc-log-path*
	   #:*frpc-log-levels*
	   #:frpc-log

	   ;; for gss security
	   #:gss-client
	   #:gss-init

	   ;; for defining authentication providers
	   #:define-auth-flavour 
	   #:pack-auth-data
	   #:unpack-auth-data
	   #:authenticate
	   #:auth-principal-name
	   #:verify 
	   #:rpc-client-auth
	   #:rpc-client-verf
	   #:rpc-client-initial

	   ;; default handler for null procedure
	   #:default-null-handler

	   ))

