

(defpackage #:hello
  (:use #:cl #:frpc))

(in-package #:hello)

;; define the RPC interface 
(use-rpc-program 1 1)

;; --------------

(defrpc call-null 0
  :string
  :string)

(defhandler handle-null (void 0) 
  (declare (ignore void))
  nil)

;; -----------------------

(defxenum hstat 
  ((:ok 0)
   (:error 1)))

(defrpc call-hello 1
  :string
  (:union hstat
    (:ok :string)
    (otherwise :void)))

(defhandler handle-hello (msg 1)
  (make-xunion :ok
	       (string-upcase msg)))

;; --------------------

(defrpc call-goodbye 2
  (:list :uint32 :string)
  (:union hstat
    (:ok (:varray :string))
    (otherwise :void)))

(defhandler handle-goodbye (args 2)
  (destructuring-bind (len str) args
    (make-xunion :ok
		 (loop for i below len collect str))))

;; --------------------


;; the server 
(defvar *server* nil)

(defun start ()
  (setf *server* (start-rpc-server 8000)))

(defun stop ()
  (stop-rpc-server *server*))
