;;;; Copyright (c) Frank James 2015 <frank.a.james@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package #:frpc)

(defvar *frpc-log* nil)

(defun frpc-log (lvl control-string &rest args)
  "Write a messagee to the debug log"
  (unless *frpc-log*
    (let ((path (format nil "~A" (merge-pathnames (user-homedir-pathname) "frpc.log"))))
      #+(or windows win32)(setf path (substitute #\\ #\/ path))
      (setf *frpc-log*
	    (pounds.log:open-log :path path
				 :tag "FRPC"))))
  (apply #'pounds.log:write-message *frpc-log* lvl control-string argS))

