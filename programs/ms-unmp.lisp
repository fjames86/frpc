;;;; Copyright (c) Frank James 2015 <frank.a.james@gmail.com>
;;;; This code is licensed under the MIT license.

;;; Implements the MS-UNMP protcol https://msdn.microsoft.com/en-us/library/cc249016.aspx
;;; Maps Windows account names to Unix UID/GIDs

;;; Note: the documentation says to use AUTH_NULL, but when I've called it I've only ever 
;;; got back RPC-AUTH-ERROR:AUTH-BADCRED. I've tried AUTH-UNIX and got the same result, so I don't know 
;;; what the correct thing to do it. Basically I can't verify if these even work.

(defpackage #:ms-unmp
  (:use #:cl #:frpc))

(in-package #:ms-unmp)

(use-rpc-host '*rpc-host* '*rpc-port*)
(use-rpc-program 351455 1)

(defxtype* windows-account () :string)

(defxstruct unix-account ()
  (option :uint32) ;; 1 == unix account name, 2 == id, 3 == unix account name and id
  (reserved :uint32)
  (id :uint32)
  (name :string))

(defxstruct windows-creds ()
  (status :boolean) ;; 0 == success 1 == failure
  (reserved :uint32)
  (name :string))

(defxstruct unix-creds ()
  (name :string)
  (id :uint32)
  (gids (:varray :uint32)))

(defxstruct unix-auth ()
  (name :string)
  (id :uint32)
  (gids (:varray :uint32)))

(defxstruct unix-user-auth ()
  (username :string)
  (password :string))

(defxstruct sequence-number ()
  (low :uint32)
  (high :uint32))

;; ---------------

(defrpc call-null 0 :void :void)

(defrpc call-get-windows-creds 1
  unix-account 
  windows-creds
  (:arg-transformer (&key name id)
    (make-unix-account :option (cond ((and name id) 3) (id 2) (name 1) (t (error "must provide a name or id")))
                       :id (or id 0)
                       :name (or name ""))))

(defrpc call-get-unix-creds-from-user 2
  windows-account 
  unix-creds
  (:arg-transformer (name)
    name))

(defrpc call-auth-using-unix 3
  unix-user-auth
  unix-auth
  (:arg-transformer (username password)
    (make-unix-user-auth :username username :password password)))

(defrpc call-current-version-token 5
  sequence-number 
  windows-creds)

(defrpc call-windows-group-from-unix-group 7
  unix-account
  windows-creds)

(defrpc call-get-unix-creds-from-group 8
  windows-account
  unix-creds)


;; -----------------------------

;; there is also a version 2 of the protocol


