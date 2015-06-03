;;;; Copyright (c) Frank James 2015 <frank.a.james@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package  #:frpc)

;; there is often a need to allocate a number of contexts, which may periodically be flushed
;; to avoid over-allocating contexts, store them in a fixed-size array. when it is exhausted, they 
;; are overwritten
(defun make-cyclic-buffer (len)
  (cons 0
	(make-array len :initial-element nil)))

(defun cyclic-push (cbuffer val)
  (setf (aref (cdr cbuffer) (car cbuffer))
	val)
  (setf (car cbuffer)
	(mod (1+ (car cbuffer)) (length (cdr cbuffer)))))

(defun cyclic-find-if (predicate cbuffer)
  (dotimes (i (length (cdr cbuffer)))
    (let ((item (aref (cdr cbuffer) i)))
      (when (and item (funcall predicate item))
	(return-from cyclic-find-if item))))
  nil)

;; Authentication using GSS requires the following:
;; 1. context creation
;; 2. data exchange
;; 3. context destruction
;;
;; The messages for parts 1 and 3 (creation and destruction) are NOT sent to the
;; normal RPC procedures. Instead, they are sent to the NULLPROC (proc = 0). This is safe
;; because this function should ALWAYS accept void argument, i.e. no argument. It is therefore possible
;; to custom data to the RPC server itself rather than the procedure.
;; A field of the credential information (gss-cred.proc) signals whether this is a control message or not.
;; If this field is :data then it is a regular message, otherwise it is a control message.
;; 


(defxenum gss-proc-t 
  (:data 0)
  (:init 1)
  (:continue 2)
  (:destroy 3))

(defxenum gss-service-t 
  (:none 1)
  (:integrity 2)
  (:privacy 3))

(defconstant +gss-version+ 1)

;; this is defined as a union(version) but since there is only a single permitted version (1) 
;; lets just put it in a structure
(defxstruct gss-cred ()
  (version :uint32 +gss-version+)
  (proc gss-proc-t)
  (seqno :uint32)
  (service gss-service-t)
  (handle (:varray* :octet)))

;; the arguments sent as a part of the control message (to the nullproc)
(defxtype* gss-init-arg () (:varray* :octet))

(defxstruct gss-init-res ()
  (handle (:varray* :octet))
  (major :uint32)
  (minor :uint32)
  (window :uint32)
  (token (:varray* :octet)))

;; this is sent in place of the arguments if the service level is :integrity
(defxstruct gss-integ-data ()
  (integ (:varray* :octet)) ;; packed gss-data-t 
  (checksum (:varray* :octet))) ;; computed using GSS_GetMIC(integ) or equivalent

;; this is packed and placed in the integ field of the gss-integ-data structure
;;(defxstruct gss-data-t ()
;;  (seqno :uint32)
;;  (arg proc-req-arg-t)) ;; here the argument type is whatever the normal argument type is

;; this is sent in place of the arguments if the service level is :privacy
;; In this case, the arguments have been encrypted using GSS wrap() call or equivalent
(defxtype* gss-priv-data () (:varray* :octet))


(defxenum gss-major-stat
 (:complete #x00000000)
  (:continue-needed #x00000001)
  (:duplicate-token #x00000002)
  (:old-token #x00000004)
  (:unseq-token #x00000008)
  (:gap-token #x00000010)
  (:bad-mech #x00010000)
  (:bad-name #x00020000)
  (:bad-nametype #x00030000)
  (:bad-bindings #x00040000)
  (:bad-status #x00050000)
  (:bad-mic #x00060000)
  (:bad-sig #x00060000)
  (:no-cred #x00070000)
  (:no-context #x00080000)
  (:defective-token #x00090000)
  (:defective-credential #x000A0000)
  (:credentials-expired #x000B0000)
  (:context-expired #x000C0000)
  (:failure #x000D0000)
  (:bad-qop #x000E0000)
  (:unauthorized #x000F0000)
  (:unavailable #x00100000)
  (:duplicate-element #x00110000)
  (:name-not-mn #x00120000)
  (:call-inaccessible-read #x01000000)
  (:call-inaccessible-write #x02000000)
  (:call-bad-structure #x03000000))

;; ---------------------------------------

(defvar *server-credentials* nil
  "The application server's GSS context, as returned from GSS-ACQUIRE-CREDENTIALS")

(defvar *gss-contexts* (make-cyclic-buffer 10)
  "List of currently active gss session contexts.")

(defstruct gss-context 
  handle context timestamp seqno window)

(defun add-gss-context (context)
  (let ((cxt (make-gss-context :context context
			       :handle (let ((v (nibbles:make-octet-vector 4)))
					 (setf (nibbles:ub32ref/be v 0) (random (expt 2 32)))
					 v)
			       :timestamp (get-universal-time)
			       :seqno 0
			       :window 10)))
    (cyclic-push *gss-contexts* cxt)
    cxt))

(defun find-gss-context (handle)
  (cyclic-find-if (lambda (c)
		    (equalp handle (gss-context-handle c)))
		  *gss-contexts*))

(defun gss-init (&optional (max-contexts 10))
  "Setup the application server's GSS support."
  (setf *server-credentials* (glass:acquire-credentials :kerberos nil)
	*gss-contexts* (make-cyclic-buffer max-contexts)))

(defmethod auth-principal-name ((type (eql :auth-gss)) data)
  "users need a way of converting the gss context into a principal name"
  (let ((c (find-gss-context (gss-cred-handle data))))
    (if c 
	(glass:context-principal-name (gss-context-context c))
	(error "Context not found for handle"))))
      
;; -----------------------------------------

;; used by the server 

(defun gss-authenticate (token)
  "Parse the GSS token and authenticate it. This should be used in initial context requests. Only support kerberos.

Returns the GSS cred on success, signals an RPC-AUTH-ERROR on failure."
  (declare (type (vector (unsigned-byte 8)) token))
  (handler-case 
      (multiple-value-bind (context response-buffer) (glass:accept-security-context *server-credentials* token)
	(let ((c (add-gss-context context)))
	  (make-gss-init-res :handle (gss-context-handle c)
			     :token response-buffer)))
    (error (e)
      (frpc-log :info "GSS failed: ~A" e)
      nil)))
    
(defun gss-authenticate-handle (cred)
  "Validate the handle belongs to the host, and that the credential is still valid."
  (declare (type gss-cred cred))
  (let ((c (find-gss-context (gss-cred-handle cred))))
    ;; the request is valid when:
    ;; 1. we have a context registerd for that handle
    ;; 2. the host for that handle matches the requesting host
    ;; 3. the seqno of the request is within the seqno window for the context
    (let ((valid (and c 
		      (<= (- (gss-cred-seqno cred) 
			     (gss-context-seqno c))
			 (gss-context-window c)))))
      (cond
	(valid 
	 ;; when it is a valid request, update the context seqno if the 
	 ;; new seqno is greater than the previously highest seen seqno 
	 (when (> (gss-cred-seqno cred) (gss-context-seqno c))
	   (setf (gss-context-seqno c) (gss-cred-seqno cred)))
	 t)
	(t 
	 nil)))))




;; --------------------------------

(defun read-gss-integ (stream reader context seqno)
  (let ((integ (%read-gss-integ-data stream)))
    (unless (glass:verify-mic context 
			      (gss-integ-data-integ integ)
			      (gss-integ-data-checksum integ))
      (error 'checksum-error))
    (flexi-streams:with-input-from-sequence (s (gss-integ-data-integ integ))
      (unless (= seqno (read-uint32 s))
	(error "Seqnos don't match"))
      (read-xtype reader s))))

(defun write-gss-integ (stream writer obj context seqno)
  (let ((msg (flexi-streams:with-output-to-sequence (s)
	       (write-uint32 s seqno)
	       (write-xtype writer s obj))))
    (%write-gss-integ-data stream
			   (make-gss-integ-data :integ msg
						:checksum (glass:get-mic context msg)))))

(defun read-gss-priv (stream reader context seqno)
  (let ((buffer (read-octet-array stream)))
    (let ((data (glass:unwrap context buffer)))
      (flexi-streams:with-input-from-sequence (s data)
	(read-gss-integ s reader context seqno)))))

(defun write-gss-priv (stream writer obj context seqno)
  (let ((buffer (flexi-streams:with-output-to-sequence (s)
		  (write-gss-integ s writer obj context seqno))))
    (write-octet-array stream 
		       (glass:wrap context buffer))))

