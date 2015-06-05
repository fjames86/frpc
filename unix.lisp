;;;; Copyright (c) Frank James 2015 <frank.a.james@gmail.com>
;;;; This code is licensed under the MIT license.


(in-package #:frpc)

;; UNIX authentication

(define-auth-flavour :auth-unix 1)

(defxstruct auth-unix ()
  (stamp :uint32)
  (machine-name :string)
  (uid :uint32)
  (gid :uint32)
  (gids (:varray :uint32 16)))

(defmethod pack-auth-data ((type (eql :auth-unix)) data)
  (pack #'%write-auth-unix data))

(defmethod unpack-auth-data ((type (eql :auth-unix)) data)
  (unpack #'%read-auth-unix data))

(defmethod auth-principal-name ((type (eql :auth-unix)) data)       
  (format nil "~A@~A" (auth-unix-uid data) (auth-unix-machine-name data)))

(defvar *unix-contexts* (make-cyclic-buffer 10)
  "Table of AUTH-UNIX contexts that have been granted.")

(defun unix-init (&optional (max-contexts 10))
  "Initialize the UNIX authentication context table. MAX-CONTEXTS is the maximum number of valid contexts that will be granted, slots in the table will be cleared and reused once the table has been filled."
  (setf *unix-contexts* (make-cyclic-buffer max-contexts)))

(defstruct unix-context unix short)

(defmethod auth-principal-name ((type (eql :auth-short)) data)
  (let ((c (find-unix-context data)))
    (when c 
      (let ((u (unix-context-unix c)))
        (format nil "~A@~A" (auth-unix-uid u) (auth-unix-machine-name u))))))

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

;; ---------------- unix ----------------

(defclass unix-client (rpc-client)
  ((uid :initarg :uid :initform 0 :accessor unix-client-uid)
   (gid :initarg :gid :initform 0 :accessor unix-client-gid)
   (gids :initarg :gids :initform nil :accessor unix-client-gids)
   (machine-name :initarg :machine-name :initform (machine-instance) :reader unix-client-machine-name)
   (nickname :initform nil :accessor unix-client-nickname)))

(defmethod print-object ((client unix-client) stream)
  (print-unreadable-object (client stream :type t)
    (format stream ":NICKNAME ~A" (unix-client-nickname client))))

(defmethod rpc-client-auth ((client unix-client))
  (if (rpc-client-initial client)
      (make-opaque-auth :auth-unix
			(make-auth-unix :stamp (- (get-universal-time) 
						  (encode-universal-time 0 0 0 1 1 1970 0))
					:machine-name (unix-client-machine-name client)
					:uid (unix-client-uid client)
					:gid (unix-client-gid client)
					:gids (unix-client-gids client)))
      (make-opaque-auth :auth-short 
			(unix-client-nickname client))))

(defmethod verify ((client unix-client) verf)
  (when (eq (opaque-auth-flavour verf) :auth-short)
    (setf (unix-client-nickname client) (opaque-auth-data verf)
	  (rpc-client-initial client) nil))
  t)
