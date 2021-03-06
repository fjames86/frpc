;;;; Copyright (c) Frank James 2015 <frank.a.james@gmail.com>
;;;; This code is licensed under the MIT license.


(in-package #:frpc)

;; general error
(define-condition rpc-error (error)
  ((description :initform "" :initarg :description :reader rpc-error-description))
  (:report (lambda (condition stream)
	     (format stream "RPC-ERROR: ~A" (rpc-error-description condition)))))

;; accept errors
(define-condition rpc-accept-error (rpc-error)
  ((stat :initform nil :initarg :stat :reader rpc-accept-error-stat))
  (:report (lambda (condition stream)
	     (format stream "RPC-ACCEPT-ERROR: ~A" 
		     (rpc-accept-error-stat condition)))))

(define-condition rpc-prog-mismatch-error (rpc-accept-error)
  ((low :initarg :low :initform 0 :reader rpc-prog-mismatch-error-low)
   (high :initarg :high :initform 0 :reader rpc-prog-mismatch-error-high))
  (:report (lambda (c stream)
	     (format stream "RPC-PROG-MISMATCH-ERROR :LOW ~A :HIGH ~A" 
		     (rpc-prog-mismatch-error-low c)
		     (rpc-prog-mismatch-error-high c)))))

(define-condition rpc-timeout-error (rpc-error)
  ()
  (:report (lambda (condition stream) 
	     (declare (ignore condition))
	     (format stream "RPC-TIMEOUT-ERROR"))))

;; reply errors 
(define-condition rpc-auth-error (rpc-error)
  ((stat :initform nil :initarg :stat :reader auth-error-stat))
  (:report (lambda (condition stream)
	     (format stream "AUTH-ERROR: ~A" (auth-error-stat condition)))))

(define-condition rpc-mismatch-error (rpc-error)
  ((high :initform 0 :initarg :high :reader rpc-mismatch-error-high)
   (low :initform 0 :initarg :low :reader rpc-mismatch-error-low))
  (:report (lambda (condition stream)
	     (format stream "RPC-MISMATCH :LOW ~A :HIGH ~A" 
		     (rpc-mismatch-error-low condition)
		     (rpc-mismatch-error-high condition)))))

