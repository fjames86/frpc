
(defpackage #:frpc.test.multi
  (:use #:cl #:frpc))

(in-package #:frpc.test.multi)

(use-rpc-program 100012 3)
(use-rpc-host "localhost" 8000)

;; -------------------------

(defun %handle-null (void)
  (declare (ignore void))
  nil)

(defrpc call-null 0 :void :void
  (:handler #'%handle-null))

;; ---------------------------

(defun %handle-upcase (string)
  (string-upcase string))

(defrpc call-upcase 1 :string :string
  (:handler #'%handle-upcase))

;; -----------------------------

(defvar *server* nil)

(defun start ()
  (setf *server* (make-rpc-server :tcp-ports '(8000) :udp-ports '(8000)))
  (start-rpc-server *server*))

(defun stop ()
  (stop-rpc-server *server*))
