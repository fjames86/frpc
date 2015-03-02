

(defpackage #:frpc.test.udp
  (:use #:cl #:frpc))

(in-package #:frpc.test.udp)


(use-rpc-program 1234 1)

(use-rpc-port 8000)

;; ----------------------------------------------

(defrpc call-null 0 :void :void)

(defhandler %handle-null (void 0) 
  (declare (ignore void))
  nil)

;; ------------------------

(defrpc call-hello 1 :string :string)
  
(defhandler %handle-hello (string 1)
  (format nil "Hello ~A!" string))

;; ------------------------------------

(defxenum stat
  ((:ok 0)
   (:error 1)))

(defrpc call-people 2
  :string
  (:union stat
    (:ok :string)
    (otherwise :void)))

(defhandler %handle-people (string 2)
  (make-xunion :ok (string-upcase string)))


;; ---------------------------

(defvar *server* nil)
(defun start ()
  (setf *server* (make-rpc-server))
  (start-rpc-server *server* :udp-ports '(8000)))

(defun stop ()
  (stop-rpc-server *server*))
