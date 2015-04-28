;;;; Copyright (c) Frank James 2015 <frank.a.james@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package  #:frpc)

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


;; (defparameter *krb5-keys* nil
;;   "The application server's keylist (i.e. contents of a keytab file or equivalent).")

;; (defun gss-init (keylist)
;;   "Setup the application server's GSS support."
;;   (setf *krb5-keys* keylist))

;; ;; ---------------------------------------

;; (defvar *gss-contexts* nil
;;   "List of currently active gss session contexts.")

;; (defstruct gss-context 
;;   host key handle timestamp seqno)

;; (defun add-gss-context (host req)
;;   (push (make-gss-context :host host
;; 			  :key (cerberus:ap-req-session-key req)
;; 			  :handle (let ((v (nibbles:make-octet-vector 4)))
;; 				    (setf (nibbles:ub32ref/be v 0) (random (expt 2 32)))
;; 				    v)
;; 			  :timestamp (get-universal-time)
;; 			  :seqno 0
;; 			  :window 10)
;; 	*gss-contexts*))

;; (defun find-gss-context (handle)
;;   (find-if (lambda (c)
;; 	     (equalp handle (gss-context-handle c)))
;; 	   *gss-contexts*))

;; (defun purge-gss-context (age)
;;   (let ((now (get-universal-time)))
;;     (setf *gss-contexts*
;; 	  (remove-if (lambda (c)
;; 		       (> (- (gss-context-timestamp c) now) age))
;; 		     *gss-contexts*))))
		       
;; ;; -----------------------------------------

;; (defun gss-authenticate (token)
;;   "Parse the GSS token and authenticate it. This should be used in initial context requests. Only support kerberos.

;; Returns the GSS cred on success, signals an RPC-AUTH-ERROR on failure."
;;   (declare (type (vector (unsigned-byte 8)) token))
;;   (handler-case 
;;       (let ((tok (cerberus:unpack-initial-context-token token)))
;; 	(typecase tok
;; 	  (cerberus:ap-req (cerberus:valid-ticket-p *krb5-keys* tok))
;; 	  (otherwise (error 'rpc-auth-error :stat :auth-rejected))))
;;     (error (e)
;;       (frpc-log :info "GSS failed: ~A" e)
;;       (error 'rpc-auth-error :stat :auth-rejected))))
    
;; (defun gss-authenticate-handle (host cred)
;;   "Validate the handle belongs to the host, and that the credential is still valid."
;;   (declare (type gss-cred cred))
;;   (let ((c (find-gss-context (gss-cred-handle cred))))
;;     ;; the request is valid when:
;;     ;; 1. we have a context registerd for that handle
;;     ;; 2. the host for that handle matches the requesting host
;;     ;; 3. the seqno of the request is within the seqno window for the context
;;     (let ((valid (and c 
;; 		      (equalp host (gss-context-host c))
;; 		      (<= (- (gss-cred-seqno cred) 
;; 			     (gss-context-seqno c))
;; 			 (gss-context-window c)))))
;;       (cond
;; 	(valid 
;; 	 ;; when it is a valid request, update the context seqno if the 
;; 	 ;; new seqno is greater than the previously highest seen seqno 
;; 	 (when (> (gss-cred-seqno cred) (gss-context-seqno c))
;; 	   (setf (gss-context-seqno c) (gss-cred-seqno cred)))
;; 	 t)
;; 	(t 
;; 	 nil)))))


