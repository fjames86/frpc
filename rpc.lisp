;;;; Copyright (c) Frank James 2015 <frank.a.james@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package #:frpc)

;; ------------------

;; enums 

(defxenum msg-type 
 (:call 0)
 (:reply 1))

(defxenum reply-stat
  (:msg-accepted 0)
  (:msg-denied 1))

(defxenum accept-stat
  (:success 0)
  (:prog-unavail 1)
  (:prog-mismatch 2)
  (:proc-unavail 3)
  (:garbage-args 4))

(defxenum reject-stat
  (:rpc-mismatch 0)
  (:auth-error 1))

(defxenum auth-stat
 (:auth-badcred 1)
  (:auth-rejected 2)
  (:auth-badverf 3)
  (:auth-rejectedverf 4)
  (:auth-tooweak 5)
  (:gss-cred-problem 13)
  (:gss-context-problem 14))

(defxenum auth-flavour 
  (:auth-null 0)
  (:auth-unix 1)
  (:auth-short 2)
  (:auth-des 3)
  (:auth-kerb4 4)
  (:auth-gss 6))

;; --------------

;; structs

(defxtype* opaque-auth ()
  (:plist :flavour auth-flavour 
	  :data (:varray* :octet 400)))
(defun %make-opaque-auth (&key flavour data)
  (list :flavour (or flavour :auth-null)
	:data data))
(defun opaque-auth-flavour (auth)
  (getf auth :flavour))
(defun opaque-auth-data (auth)
  (getf auth :data))

(defparameter *default-opaque-auth* (%make-opaque-auth))

(defxstruct call-body ()
  (rpcvers :uint32 2) ;; must always be 2
  (prog :uint32)
  (vers :uint32)
  (proc :uint32)
  (auth opaque-auth *default-opaque-auth*)
  (verf opaque-auth *default-opaque-auth*)) ;; parameters start here

(defxstruct accepted-reply () 
  (verf opaque-auth *default-opaque-auth*)
  (reply-data 
   (:union accept-stat
     (:success ;; results follow
      :void)
     (:prog-mismatch 
      (:alist (low :uint32) (high :uint32)))
     (otherwise :void))))

(defxunion rejected-reply (reject-stat)
  (:rpc-mismatch
    (:alist (low :uint32) (high :uint32)))
  (:auth-error auth-stat))

(defxunion reply-body (reply-stat)
  (:msg-accepted accepted-reply)
  (:msg-denied rejected-reply))

(defxstruct rpc-msg ()
  (xid :uint32 0)
  (body 
   (:union msg-type 
     (:call call-body)
     (:reply reply-body))))

(defparameter *rpc-msgid* 0)

(defun make-msgid ()
  (prog1 *rpc-msgid*
    (incf *rpc-msgid*)))

(defun make-rpc-request (program proc &key (version 0) auth verf id)
  (unless auth (setf auth *default-opaque-auth*))
  (unless verf (setf verf *default-opaque-auth*))

  (make-rpc-msg :xid (or id (make-msgid))
		:body (make-xunion :call
				   (make-call-body :prog program
						   :vers version
						   :proc proc
						   :auth auth
						   :verf verf))))

(defun make-rpc-response (&key accept reject verf (id 0) (high 0) (low 0) auth-stat)
  (unless verf (setf verf *default-opaque-auth*))

  (make-rpc-msg 
   :xid id
   :body 
   (make-xunion 
    :reply 
    (if accept
	(make-xunion 
	 :msg-accepted 
	 (make-accepted-reply 
	  :verf verf
	  :reply-data 
	  (case accept
	    (:success (make-xunion :success nil))
	    (:prog-mismatch (make-xunion :prog-mismatch `((high . ,high) (low . ,low))))
	    (otherwise (make-xunion accept nil)))))
	(make-xunion 
	 :msg-denied
	 (ecase reject
	   (:rpc-mismatch (make-xunion :rpc-mismatch `((high . ,high) (low . ,low))))
	   (:auth-error (make-xunion :auth-error auth-stat))))))))
	    
      
;; ------ todo: implement the authentication stuff ------------

;; 9.1 null authentication

;; 9.2 UNIX authentication

(defxstruct auth-unix ()
  (stamp :uint32)
  (machine-name :string)
  (uid :uint32)
  (gid :uint32)
  (gids (:varray :uint32 16)))

;; 9.3 DES authentication

(defxenum authdes-namekind 
  (:adn-fullname 0)
  (:adn-nickname 1))

(defxtype des-block ()
  ((stream)
   (read-fixed-array #'read-octet stream 8))
  ((stream obj)
   (write-fixed-array #'write-octet stream obj)))
(defun make-des-block ()
  (nibbles:make-octet-vector 8))

(defxstruct authdes-fullname ()
  (name :string)
  (key des-block)
  (window (:array :octet 4)))

(defxunion authdes-cred (authdes-namekind)
  (:adn-fullname authdes-fullname (make-des-block))
  (:adn-nickname :int32))

(defxtype* authdes-timestamp ()
  (:plist :seconds :uint32
	  :useconds :uint32))

(defxstruct authdes-verf-client ()
  (adv-timestamp des-block (make-des-block))
  (adv-winverf (:array :octet 4) (nibbles:make-octet-vector 4)))

(defxstruct authdes-verf-server ()
  (adv-timeverf des-block (make-des-block))
  (adv-nickname :int32))

;; 9.3.5 Diffie-Hellman 

;; these constants are specified in the rfc
;; (defconstant +dh-base+ 3)
;; (defconstant +dh-modulus+ (parse-integer "d4a0ba0250b6fd2ec626e7efd637df76c716e22d0944b88b" 
;; 					 :radix 16))

;; (defun dh-public-key (secret)
;;   (mod (expt +dh-base+ secret) +dh-modulus+))

;; (defun dh-common-key (secret public)
;;   "Local private key, remote public key"
;;   (mod (expt public secret) +dh-modulus+))

;; (defun dh-conversation-key (common)
;;   (let ((bytes (nibbles:make-octet-vector 8)))
;;     (setf (nibbles:ub64ref/be bytes 0) (ash common -64))
;;     (dotimes (i 8)
;;       (let ((parity (logcount (aref bytes i))))
;; 	(unless (zerop parity)
;; 	  (setf (aref bytes i)
;; 		(logior (aref bytes i) 1)))))
;;     (nibbles:ub64ref/be bytes 0)))

;; (defun make-dh-cipher (private public)
;;   "Local private key, remote public key"
;;   (ironclad:make-cipher :des
;; 			:mode :cbc
;; 			:key (dh-conversation-key (dh-common-key private publc))))

;; (defun dh-encrypt (cipher data)
;;   (let ((result (nibbles:make-octet-vector 8)))
;;     (ironclad:encrypt cipher data result)
;;     result))
;; (defun dh-decrypt (cipher data)
;;   (let ((result (nibbles:make-octet-vector 8)))
;;     (ironclad:decrypt cipher data result)
;;     result))

;; ------------------------

(defun unpack-opaque-auth (auth)
  (let ((data (opaque-auth-data auth)))
    (list :flavour (opaque-auth-flavour auth)
	  :data
	  (case (opaque-auth-flavour auth)
	    (:auth-null nil)
	    (:auth-unix (unpack #'%read-auth-unix data))
	    (:auth-short data)
	    (otherwise data)))))

(defun pack-opaque-auth (auth)
  (let ((data (opaque-auth-data auth)))
    (list :flavour (opaque-auth-flavour auth)
	  :data 
	  (case (opaque-auth-flavour auth)
	    (:auth-null nil)
	    (:auth-unix (pack #'%write-auth-unix data))
	    (:auth-short data)
	    (otherwise data)))))

(defun make-opaque-auth (flavour data)
  (pack-opaque-auth (%make-opaque-auth :flavour flavour
				       :data data)))

;; ----------------------------------------


(defparameter *rpc-program* 0)
(defparameter *rpc-version* 0)

(defmacro use-rpc-program (program version)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf *rpc-program* ,program
	   *rpc-version* ,version)))

