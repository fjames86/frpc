;;;; Copyright (c) Frank James 2015 <frank.a.james@gmail.com>
;;;; This code is licensed under the MIT license.


(in-package #:frpc)

;; general error
(define-condition rpc-error (error)
  ((description :initform "" :initarg :description :reader rpc-error-description))
  (:report (lambda (condition stream)
	     (format stream "RPC ERROR: ~S" (rpc-error-description condition)))))

;; accept errors
(define-condition rpc-accept-error (rpc-error)
  ((stat :initform nil :initarg :stat :reader rpc-accept-error-stat))
  (:report (lambda (condition stream)
	     (format stream "ACCEPT ERROR ~A" 
		     (rpc-accept-error-stat condition)))))

(define-condition rpc-prog-mismatch-error (rpc-accept-error)
  ())

(define-condition rpc-timeout-error (rpc-error)
  ()
  (:report (lambda (condition stream) 
	     (declare (ignore condition))
	     (format stream "RPC-TIMEOUT-ERROR"))))

;; reply errors 
(define-condition rpc-auth-error (rpc-error)
  ((stat :initform nil :initarg :stat :reader auth-error-stat))
  (:report (lambda (condition stream)
	     (format stream "AUTH-ERROR ~A" (auth-error-stat condition)))))

(define-condition rpc-mismatch-error (rpc-error)
  ((high :initform 0 :initarg :high :reader rpc-mismatch-error-high)
   (low :initform 0 :initarg :low :reader rpc-mismatch-error-low))
  (:report (lambda (condition stream)
	     (format stream "RPC-MISMATCH ~A:~A" 
		     (rpc-mismatch-error-low condition)
		     (rpc-mismatch-error-high condition)))))

