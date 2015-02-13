

(defpackage #:hello-rpc
  (:use #:cl #:frpc))

(in-package #:hello-rpc)

;; define a structure
(defxstruct hello-s ()
  ((name :string "")
   (age :uint32 0)
   (location :string "")))

;; define an interface and server handlers
(with-rpc-program (1)
  (with-rpc-version (0)
    (defrpc call-hello (:string :string) 0)
    (defhandler handle-hello (msg) 0
      (format nil "Hello ~A!" msg))

    (defrpc call-goodbye (:int32 :uint32) 1)
    (defhandler handle-goodbye (i) 1
      (1+ (abs i)))

    (defrpc call-person (:string hello-s) 2)
    (defhandler handle-person (name) 2
      (make-hello-s :name (string-upcase name)
		    :age 123
		    :location "yy"))))








