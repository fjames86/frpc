;;;; Copyright (c) Frank James 2015 <frank.a.james@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package #:frpc)

(defparameter *frpc-log-path* (merge-pathnames (user-homedir-pathname) "frpc.log"))
(defvar *frpc-log* nil
  "The log to write messages to. Gets opened on the first call to FRPC-LOG.")

(defun frpc-log (lvl control-string &rest args)
  "Write a message to the debug log."
  (unless *frpc-log*
    (let ((path (namestring *frpc-log-path*))) 
      #+(or windows win32)(setf path (substitute #\\ #\/ path))
      (setf *frpc-log*
	    (pounds.log:open-log :path path
				 :tag "FRPC"))))
  (pounds.log:write-message *frpc-log* lvl 
			    (apply #'format nil control-string args)))

