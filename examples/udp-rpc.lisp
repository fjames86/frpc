

(defpackage #:frpc.test.udp
  (:use #:cl #:frpc))

(in-package #:frpc.test.udp)

(defvar *server* nil)

(defun start ()
  (setf *server* (make-udp-rpc-server))
  (start-rpc-server *server* :port 8000))

(defun stop ()
  (stop-rpc-server *server*))
