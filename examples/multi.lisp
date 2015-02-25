
(defpackage #:frpc.test.multi
  (:use #:cl #:frpc))

(in-package #:frpc.test.multi)

(defparameter *port* 8000)

(use-rpc-program 100012 3)

(defrpc %call-null 0 :void :void)
(defun call-null (&optional udp)
  (%call-null "localhost" nil :port *port* :protocol (if udp :udp :tcp)))
(defhandler %handle-null (void 0)
  (declare (ignore void))
  nil)

(defrpc %call-upcase 1 :string :string)
(defun call-upcase (string &optional udp)
  (%call-upcase "localhost" string :port *port* :protocol (if udp :udp :tcp)))
(defhandler %handle-upcase (string 1)
  (string-upcase string))


;; -----------------------------

(defvar *server* (make-rpc-server))

(defun start ()
  (start-rpc-server *server* :tcp-ports (list *port*) :udp-ports (list *port*)))

(defun stop ()
  (stop-rpc-server *server*))
