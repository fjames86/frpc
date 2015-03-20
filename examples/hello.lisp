

(defpackage #:frpc.test.hello
  (:use #:cl #:frpc))

(in-package #:frpc.test.hello)

(use-rpc-program 1 1)
(use-rpc-host "localhost" 8000)

;; --------------------------------------------

(defrpc call-null 0 :void :void)

(defhandler handle-null (void 0) 
  (declare (ignore void))
  nil)

;; ---------------------------------------------

(defxenum hstat 
 (:ok 0)
 (:error 1))

(defrpc call-hello 1
  :string
  (:union hstat
    (:ok :string)
    (otherwise :void)))

(defhandler handle-hello (msg 1)
  (make-xunion :ok
	       (string-upcase msg)))

;; ---------------------------------------------

(defrpc call-goodbye 2
  (:list :uint32 :string)
  (:union hstat
    (:ok (:varray :string))
    (otherwise :void))
  (:arg-transformer (n string) (list n string)))

(defhandler handle-goodbye (args 2)
  (destructuring-bind (len str) args
    (make-xunion :ok
		 (loop for i below len collect str))))

;; --------------------

(defrpc call-plist 3
  :string
  (:plist :name :string :age :uint32))

(defhandler handle-plist (string 3)
  (list :name string :age 123))


;; ------------------

(defrpc call-varray* 4
  (:varray :string)
  (:varray* :string))

(defhandler handle-varray* (strings 4)
  (make-array (length strings) :initial-contents strings))

;; ---------------------


;; the server 
(defvar *server* nil)

(defun start ()
  (setf *server* (make-rpc-server))
  (start-rpc-server *server* :tcp-ports '(8000)))

(defun stop ()
  (stop-rpc-server *server*))
