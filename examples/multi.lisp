
(defpackage #:frpc.test.multi
  (:use #:cl #:frpc))

(in-package #:frpc.test.multi)

(use-rpc-program 100012 3)
(use-rpc-host "localhost" 8000)

;; -------------------------

(defrpc call-null 0 :void :void)

(defhandler %handle-null (void 0)
  (declare (ignore void))
  nil)

;; ---------------------------

(defrpc call-upcase 1 :string :string)

(defhandler %handle-upcase (string 1)
  (string-upcase string))


;; -----------------------------

(defvar *server* nil)

(defun start ()
  (setf *server* (make-rpc-server))
  (start-rpc-server *server* :tcp-ports '(8000) :udp-ports '(8000)))

(defun stop ()
  (stop-rpc-server *server*))
