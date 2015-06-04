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

;; ---------------------------------------
;; authentication definition

(defvar *auth-flavours* nil 
  "List of known authentication flavours.")

(defxtype auth-flavour ()
  ((stream) 
   (let ((n (read-uint32 stream)))
     (let ((f (find n *auth-flavours* :key #'cadr :test #'=)))
       (if f (car f) (error "Unknown authentication flavour ~S" n)))))
  ((stream flavour)
   (let ((f (find flavour *auth-flavours* :key #'car :test #'eq)))
     (if f (write-uint32 stream (cadr f)) (error "Unknown authentication flavour ~S" flavour)))))

(defun %define-auth-flavour (name val)
  (let ((pair (assoc name *auth-flavours*)))
    (if pair
        (setf (cdr pair) (list val))
        (push (list name val) *auth-flavours*))))

(defmacro define-auth-flavour (name val)
  `(%define-auth-flavour ',name ,val))

(define-auth-flavour :auth-null 0)
(define-auth-flavour :auth-unix 1)
(define-auth-flavour :auth-short 2)
(define-auth-flavour :auth-des 3)
(define-auth-flavour :auth-kerb4 4)
(define-auth-flavour :auth-rsa 5) ;; AUTH_RSA/Gluster, the Gluster protocols use this for their own flavour 
(define-auth-flavour :auth-gss 6)

;; -------------------------------------------

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

;; this is used by the server
(defgeneric authenticate (flavour data verf)
  (:documentation "Authenticate the transaction. 
FLAVOUR is the authentication flavour, data is the authentication data. VERF is the opaque-auth verifier.

Returns a response verifier to be sent back to the client or nil in the case of failure."))

;; default method for authentication rejects all requests
(defmethod authenticate (flavour data verf) nil)


(defgeneric auth-principal-name (type data)
  (:documentation "Returns a string representing the principal that was authenticated, or nil if none available."))

;; default method returns nil
(defmethod auth-principal-name (type data) nil)


;; null authentication just returns nil
(defmethod authenticate ((flavour (eql :auth-null)) data verf)
  (make-opaque-auth :auth-null nil))
(defmethod pack-auth-data ((flavour (eql :auth-null)) data) nil)
(defmethod unpack-auth-data ((flavour (eql :auth-null)) data) nil)

;; -------- DEPRECATED --------------------------------
;; this was the old way to define RPC interfaces. It involved modifying 
;; a pair of globals at compile time so that the macroexpander could insert
;; program/version numbers. 

;; these are used to keep track of the current program that is being compiled
(defparameter *rpc-program* 0)
(defparameter *rpc-version* 0)

(defmacro use-rpc-program (program version)
  (alexandria:simple-style-warning "USE-RPC-PROGRAM is deprecated. Replace with DEFPROGRAM and a (:program progname version) option to DEFRPC.")
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf *rpc-program* ,program
	   *rpc-version* ,version)))

;; ------------------------------------------------------


(defvar *programs* nil
  "List of program ID mappings.")

(defun %defprogram (name number)
  (let ((p (assoc name *programs*)))
    (cond
      ((and p (= (cadr p) number))
       ;; program already defined, do nothing
       nil)
      ((and p (not (= (cadr p) number)))
       (alexandria:simple-style-warning "Redefining program ~A to ~A" name number)
       (setf (cdr p) (list number)))
      (t 
       (push (list name number) *programs*))))
  name)
       
(defmacro defprogram (name number)
  "Define a program identifier mapping. 
NAME ::= a symbol naming the program. 
NUMBER ::= a positive integer specifying the program number.
"
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (%defprogram ',name ,number)))

(defun find-program (id)
  "Lookup the program identifier. The type of ID changes behaviour:
Integer: the program with a this program number is returned. 
String: the first program with a case-insensitive program name is returned.
Symbol: the program with a matching symbol name is returned.

In all cases returns a list of (name number) or nil if not found."
  (etypecase id
    (symbol
     (assoc id *programs*))
    (string 
     (find id *programs* 
           :key (lambda (x) (symbol-name (car x)))
           :test #'string-equal))
    (integer 
     (find id *programs* :key #'cadr))))

(defun list-all-programs ()
  "List all known program mappings."
  *programs*)

(defun program-id (id)
  "Lookup the program name or number."
  (let ((p (find-program id)))
    (when p 
      (etypecase id
        ((or symbol string) (cadr p))
        (integer (car p))))))


;; ----------------------------------------------------

(defparameter *rpc-host* "localhost")
(defparameter *rpc-port* 111)

(defclass rpc-client ()
  ((host :initarg :host :initform *rpc-host* :accessor rpc-client-host)
   (port :initarg :port :initform *rpc-port* :accessor rpc-client-port)
   (protocol :initarg :protocol :initform :udp :accessor rpc-client-protocol)
   (timeout :initarg :timeout :initform 1 :accessor rpc-client-timeout)
   ;; extras
   (program :initarg :program :initform nil :accessor rpc-client-program)
   (version :initarg :version :initform nil :accessor rpc-client-version)
   (initial :initform t :accessor rpc-client-initial)
   (connection :initarg :connection :initform nil :accessor rpc-client-connection)))

(defmethod print-object ((client rpc-client) stream)
  (print-unreadable-object (client stream :type t :identity t)
    (format stream ":HOST ~S :PORT ~A :PROGRAM ~A"
	    (rpc-client-host client)
	    (rpc-client-port client)
	    (rpc-client-program client))))

;; reinitializing the instance just means setting its initial flag again
(defmethod reinitialize-instance :after ((instance rpc-client) &key)
  (setf (rpc-client-initial instance) t))

(defgeneric rpc-client-auth (client)
  (:documentation "The authenticator to use for the client request."))

(defgeneric rpc-client-verf (client)
  (:documentation "The verifier to use for the client request."))

(defgeneric verify (client verf)
  (:documentation "Verify the response received from the server. Signals an error on failure."))


;; default methods for auth-null flavour authentication 
(defmethod rpc-client-auth ((client rpc-client))
  (make-opaque-auth :auth-null nil))

(defmethod rpc-client-verf ((client rpc-client))
  (make-opaque-auth :auth-null nil))

(defmethod verify ((client rpc-client) verf) t)
