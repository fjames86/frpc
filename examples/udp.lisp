

(defpackage #:frpc.test.udp
  (:use #:cl #:frpc))

(in-package #:frpc.test.udp)


(use-rpc-program 1234 1)


(defvar *server* nil)

(defrpc %call-null 0 :void :void)
(defun call-null ()
  (let ((msgid *rpc-msgid*))
    (%call-null "localhost" nil :port 8000 :protocol :udp)
    (wait-for-reply *server* msgid)))

(defhandler %handle-null (void 0) 
  (declare (ignore void))
  nil)

(defrpc %call-hello 1 :string :string)
(defun call-hello (string)
  (let ((msgid *rpc-msgid*))
    (%call-hello "localhost" string :port 8000 :protocol :udp)
    (let ((reply (wait-for-reply *server* msgid)))
      (unpack (lambda (s) (read-xtype :string s)) reply))))
  
(defhandler %handle-hello (string 1)
  (format nil "Hello ~A!" string))


(defxenum stat
  ((:ok 0)
   (:error 1)))

(defxunion call-people-res (stat)
  ((:ok :string)
   (otherwise :void)))

(defrpc %call-people 2
  :string
  call-people-res)

(defun call-people (string)
  (let ((msgid *rpc-msgid*))
    (%call-people "localhost" string :port 8000 :protocol :udp)
    (unpack #'%read-call-people-res (wait-for-reply *server* msgid))))

(defhandler %handle-people (string 2)
  (make-xunion :ok
	       (string-upcase string)))


(defun start ()
  (setf *server* (make-udp-rpc-server))
  (start-rpc-server *server* :port 8000))

(defun stop ()
  (stop-rpc-server *server*))
