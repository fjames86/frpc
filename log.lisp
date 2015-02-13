;;;; Copyright (c) Frank James 2015 <frank.a.james@gmail.com>
;;;; This code is licensed under the MIT license.


(in-package #:frpc)

(defparameter *rpc-debug* nil)

(defmacro info (&rest args)
  `(when *rpc-debug*
     (log:info ,@args)))
