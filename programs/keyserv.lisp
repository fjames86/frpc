;;;; Copyright (c) Frank James 2015 <frank.a.james@gmail.com>
;;;; This code is licensed under the MIT license.

;;
;; http://www.revathi.in/symsssss/root/usr/include/rpcsvc/key_prot.x

(defpackage #:keyserv
  (:use #:cl #:frpc))

(in-package #:keyserv)

(defxtype* keybuffer () (:array :octet 48))

(defxenum keystat 
  (:success 0)
  (:nosecret 1)
  (:unknown 2)
  (:systemerr 3))

(defxtype* crypt-key-arg ()
  (:plist :name :string
	  :key (:array :octet 8)))

(defxtype* crypt-key-arg2 ()
  (:plist :name :string
	  :remotekey (:varray* :octet 1024)
	  :key (:array :octet 8)))

(defxunion crypt-key-res (keystat)
  (:success (:array :octet 8))
  (otherwise :void))

(defxunion get-cred-res (keystat)
  (:success (:plist :uid :uint32
		    :gid :uint32
		    :gids (:varray :uint32)))
  (otherwise :void))

(defxtype* netstarg ()
  (:plist :priv keybuffer
	  :pub keybuffer
	  :name :string))

(defxunion neystres (keystat)
  (:success netstarg)
  (otherwise :void))

;; ----------------------

;; version1
(use-rpc-program 100029 1)

(defrpc call-null 0 :void :void)

(defrpc call-key-set 1 
  keybuffer 
  keystat)

(defrpc call-encrypt 2 
  crypt-key-arg 
  keystat)

(defrpc call-decrypt 3
  crypt-key-arg
  crypt-key-res)

(defrpc call-gen 4
  :void
  (:array :octet 8))

(defrpc call-get-cred 5
  :string
  get-cred-res)


;; version 2
(use-rpc-program 100029 2)

(defrpc call-null2 0 :void :void)

(defrpc call-key-set2 1 
  keybuffer 
  keystat)

(defrpc call-encrypt2 2 
  crypt-key-arg 
  keystat)

(defrpc call-decrypt2 3
  crypt-key-arg
  crypt-key-res)

(defrpc call-gen2 4
  :void
  (:array :octet 8))

(defrpc call-get-cred2 5
  :string
  get-cred-res)

(defrpc call-encrypt-pk 6
  crypt-key-arg2
  crypt-key-res)

(defrpc call-decrypt-pk 7
  crypt-key-arg2
  crypt-key-res)

(defrpc call-net-put 8
  netstarg 
  keystat)

(defrpc call-net-get 9
  :void
  netstres)

(defrpc call-get-conv 10
  keybuffer
  crypt-key-res)


	    
