

(defpackage #:frpc.test.udp
  (:use #:cl #:frpc))

(in-package #:frpc.test.udp)


(use-rpc-program 1234 1)
(use-rpc-host "localhost" 8000)

;; ----------------------------------------------

(defun %handle-null (void) 
  (declare (ignore void))
  nil)

(defrpc call-null 0 :void :voi
  (:handler #'%handle-null))

;; ------------------------
 
(defun %handle-hello (string)
  (format nil "Hello ~A!" string))

(defrpc call-hello 1 :string :string
  (:handler #'%handle-hello))

;; ------------------------------------

(defxenum stat
  (:ok 0)
  (:error 1))

(defun %handle-people (string)
  (make-xunion :ok (string-upcase string)))

(defrpc call-people 2
  :string
  (:union stat
    (:ok :string)
    (otherwise :void))
  (:handler #'%handle-people))

;; ---------------------------

(defvar *server* nil)
(defun start ()
  (setf *server* (make-rpc-server :udp-ports '(8000)))
  (start-rpc-server *server*))

(defun stop ()
  (stop-rpc-server *server*))
