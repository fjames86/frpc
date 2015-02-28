

(defpackage #:frpc.test.udp
  (:use #:cl #:frpc))

(in-package #:frpc.test.udp)


(use-rpc-program 1234 1)


;; ----------------------------------------------

(defrpc %call-null 0 :void :void)
(defun call-null ()
  (%call-null nil :port 8000 :protocol :udp))

(defhandler %handle-null (void 0) 
  (declare (ignore void))
  nil)

;; ------------------------

(defrpc %call-hello 1 :string :string)
(defun call-hello (string)
  (%call-hello string :port 8000 :protocol :udp))
  
(defhandler %handle-hello (string 1)
  (format nil "Hello ~A!" string))

;; ------------------------------------

(defxenum stat
  ((:ok 0)
   (:error 1)))

(defrpc %call-people 2
  :string
  (:union stat
    (:ok :string)
    (otherwise :void)))

(defun call-people (string)
  (%call-people string :port 8000 :protocol :udp))

(defhandler %handle-people (string 2)
  (make-xunion :ok (string-upcase string)))


;; ---------------------------

(defvar *server* nil)
(defun start ()
  (setf *server* (make-rpc-server))
  (start-rpc-server *server* :udp-ports '(8000)))

(defun stop ()
  (stop-rpc-server *server*))
