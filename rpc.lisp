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

;; wrap the opaque-auth structure so that we can automatically decode the opaque data component 
;; into the structure it represents, dispatching on the flavour
;; by default, leave it alone 
(defun make-opaque-auth (flavour data)
  (list :flavour flavour :data data))
(defun opaque-auth-flavour (auth)
  (getf auth :flavour))
(defun opaque-auth-data (auth)
  (getf auth :data))
(defun (setf opaque-auth-data) (value auth)
  (setf (getf auth :data) value))

(defxtype* %opaque-auth ()
  (:plist :flavour auth-flavour 
	  :data (:varray* :octet 400)))

(defgeneric pack-auth-data (flavour data))
(defmethod pack-auth-data (flavour data)
  data)

(defgeneric unpack-auth-data (flavour data))
(defmethod unpack-auth-data (flavour data)
  data)

;; wrap the opaque auth so that we can unpack the data, dispatching on flavour
;; FIXME: if verfifiers and authenticators contain different data for the same flavour (as happens with DES?)
;; then we should define two different wrapper-types for auth and verf, which each dispatch to different
;; generic functions.
(defxtype opaque-auth ()
  ((stream)
   (let ((auth (read-xtype '%opaque-auth stream)))
     (setf (opaque-auth-data auth)
	   (unpack-auth-data (opaque-auth-flavour auth)
			     (opaque-auth-data auth)))
     auth))
  ((stream auth)
   (write-xtype '%opaque-auth 
		stream
		(make-opaque-auth (opaque-auth-flavour auth)
				  (pack-auth-data (opaque-auth-flavour auth)
						  (opaque-auth-data auth))))))

(defparameter *default-opaque-auth* (make-opaque-auth :auth-null nil))

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
      (:plist :low :uint32 :high :uint32))
     (otherwise :void))))

(defxunion rejected-reply (reject-stat)
  (:rpc-mismatch
    (:plist :low :uint32 :high :uint32))
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

(defun rpc-msg-verifier (msg)
  (let ((body (rpc-msg-body msg)))
    (ecase (xunion-tag body)
      (:call 
       (let ((call (xunion-val body)))
	 (call-body-verf call)))
      (:reply 
       (when (eq (xunion-tag (xunion-val body)) :msg-accepted)
	 (accepted-reply-verf (xunion-val (xunion-val body))))))))

;; ----------------------------------

(defparameter *rpc-msgid* 0
  "Global incrementing message counter, used by the client to generate unique message IDs")
(defun make-msgid ()
  (prog1 *rpc-msgid*
    (incf *rpc-msgid*)))

;; -----------------------------------

(defun make-rpc-request (program proc &key (version 0) auth verf id)
  "Make an RPC message for a request."
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
  "Make an RPC message for a response."
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
	    (:prog-mismatch (make-xunion :prog-mismatch `(:high ,high :low ,low)))
	    (otherwise (make-xunion accept nil)))))
	(make-xunion 
	 :msg-denied
	 (ecase reject
	   (:rpc-mismatch (make-xunion :rpc-mismatch `(:high ,high :low ,low)))
	   (:auth-error (make-xunion :auth-error auth-stat))))))))
	    
(defun generate-program-number (&optional transient)
  "Generate a program identifier in the user-defined range, as specified by the RFC.
If TRANSIENT is true, a runtime program number is generated. These should be used by programs which
need to generate program numbers at runtime."
  (if transient 
      (+ #x40000000 (random #x20000000))
      (+ #x20000000 (random #x20000000))))


;; -------------- authentication stuff ----------------

(defgeneric authenticate (flavour data verf)
  (:documentation "Authenticate the transaction. 
FLAVOUR is the authentication flavour, data is the authentication data. VERF is the opaque-auth verifier.

Returns a response verifier to be sent back to the client or nil in the case of failure."))

;; default method for authentication rejects all requests
(defmethod authenticate (flavour data verf) nil)


;; 9.1 null authentication
(defmethod authenticate ((flavour (eql :auth-null)) data verf)
  (make-opaque-auth :auth-null nil))

(defmethod pack-auth-data ((flavour (eql :auth-null)) data) nil)
(defmethod unpack-auth-data ((flavour (eql :auth-null)) data) nil)

;; 9.2 UNIX authentication

(defxstruct auth-unix ()
  (stamp :uint32)
  (machine-name :string)
  (uid :uint32)
  (gid :uint32)
  (gids (:varray :uint32 16)))

(defun make-unix (uid &optional gid gids)
  "Make an AUTH-UNIX opaque auth structure."
  (make-opaque-auth :auth-unix
		    (make-auth-unix :stamp (- (get-universal-time) 
					      (encode-universal-time 0 0 0 1 1 1970 0))
				    :machine-name (machine-instance)
				    :uid uid
				    :gid (or gid 0)
				    :gids gids)))

(defmethod pack-auth-data ((type (eql :auth-unix)) data)
  (pack #'%write-auth-unix data))

(defmethod unpack-auth-data ((type (eql :auth-unix)) data)
  (unpack #'%read-auth-unix data))

(defvar *unix-contexts* (make-cyclic-buffer 10)
  "Table of AUTH-UNIX contexts that have been granted.")

(defstruct unix-context unix short)

(defun add-unix-context (unix)
  (let ((c (make-unix-context :unix unix
			      :short (let ((v (nibbles:make-octet-vector 4)))
				       (setf (nibbles:ub32ref/be v 0) (random (expt 2 32)))
				       v))))
    (cyclic-push *unix-contexts* c)
    c))

(defun find-unix-context (short)
  (cyclic-find-if (lambda (c)
		    (equalp (unix-context-short c) short))
		  *unix-contexts*))

(defmethod authenticate ((flavour (eql :auth-unix)) data verf)
  (declare (ignore verf))
  (let ((c (add-unix-context data)))
    (make-opaque-auth :auth-short (unix-context-short c))))

(defmethod authenticate ((flavour (eql :auth-short)) data verf)
  (declare (ignore verf))
  (let ((c (find-unix-context data)))
    (if c
	(make-opaque-auth :auth-null nil)
	nil)))

;; 9.3 DES authentication

;; DES authentication is different to the other flavours because its authenticator and verifier structures
;; do not match. This means we can't define a functions to pack/unpack its data. We therefore leave it as
;; an opaque buffer and parse it as required.

(defmethod authenticate ((flavour (eql :auth-des)) data verf)
  ;; start by parsing the data 
  (let ((auth (unpack #'%read-authdes-cred data))
	(v (unpack #'%read-authdes-verf-client (opaque-auth-data verf))))
    (handler-case (des-valid-client-request auth v)
      (error (e)
	(frpc-log :info "DES authentication failed: ~S" e)
	nil))))

;; GSS authentication 

(defmethod pack-auth-data ((type (eql :auth-gss)) data)
  (pack #'%write-gss-cred data))

(defmethod unpack-auth-data ((type (eql :auth-gss)) data)
  (unpack #'%read-gss-cred data))

;; GSS authentication requires special treatment, i.e. hard-coding into the rpc server codes.
;; This means this function is essentially redudant, but we need to fill it in anyway so that 
;; the server does not immediately reject it 
(defmethod authenticate ((flavour (eql :auth-gss)) data verf)
  (make-opaque-auth :auth-null nil))

;; ----------------------------------------

;; these are used to keep track of the current program that is being compiled
(defparameter *rpc-program* 0)
(defparameter *rpc-version* 0)

(defmacro use-rpc-program (program version)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf *rpc-program* ,program
	   *rpc-version* ,version)))

