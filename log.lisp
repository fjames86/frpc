;;;; Copyright (c) Frank James 2015 <frank.a.james@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package #:frpc)

(defparameter *frpc-log-path* (merge-pathnames (user-homedir-pathname) "frpc.log")
  "The path to the log file. If this is NIL then no log will be opened, no logging will be performed.")

(defvar *frpc-log* nil
  "The log to write messages to. Gets opened on the first call to FRPC-LOG.")

;; can turn on :trace level logging if it's really needed, generally it's not
(defparameter *frpc-log-levels* '(:info :warning :error))

(defun frpc-log (lvl control-string &rest args)
  "Write a message to the debug log."
  (when *frpc-log-path*
    (unless *frpc-log*
      (let ((path (namestring *frpc-log-path*))) 
	#+(or windows win32)(setf path (substitute #\\ #\/ path))
	(setf *frpc-log*
	      (pounds.log:open-log :path path
				   :tag "FRPC"))))
    (when (member lvl *frpc-log-levels*)
      (pounds.log:write-message *frpc-log* lvl 
				(apply #'format nil control-string args)))))

