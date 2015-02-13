
(in-package #:frpc)

(defparameter *rpc-debug* nil)

(defmacro info (&rest args)
  `(when *rpc-debug*
     (log:info ,@args)))
