

(defpackage #:frpc.test.hello
  (:use #:cl #:frpc))

(in-package #:frpc.test.hello)

(use-rpc-program 1 1)
(use-rpc-host "localhost" 8000)

;; --------------------------------------------

(defun handle-null (void) 
  (declare (ignore void))
  nil)

(defrpc call-null 0 :void :void
  (:handler #'handle-null))

;; ---------------------------------------------

(defxenum hstat 
 (:ok 0)
 (:error 1))

(defun handle-hello (msg)
  (make-xunion :ok
	       (string-upcase msg)))

(defrpc call-hello 1
  :string
  (:union hstat
    (:ok :string)
    (otherwise :void))
  (:handler #'handle-hello))


;; ---------------------------------------------


(defun handle-goodbye (args)
  (destructuring-bind (len str) args
    (make-xunion :ok
		 (loop for i below len collect str))))

(defrpc call-goodbye 2
  (:list :uint32 :string)
  (:union hstat
    (:ok (:varray :string))
    (otherwise :void))
  (:arg-transformer (n string) (list n string))
  (:handler #'handle-goodbye))

;; --------------------

(defun handle-plist (string)
  (list :name string :age 123))

(defrpc call-plist 3
  :string
  (:plist :name :string :age :uint32)
  (:handler #'handle-plist))

;; ------------------

(defun handle-varray* (strings)
  (make-array (length strings) :initial-contents strings))

(defrpc call-varray* 4
  (:varray :string)
  (:varray* :string)
  (:handler #'handle-varray*))

;; ---------------------


;; the server 
(defvar *server* nil)

(defun start ()
  (setf *server* (make-rpc-server))
  (start-rpc-server *server* :tcp-ports '(8000)))

(defun stop ()
  (stop-rpc-server *server*))
