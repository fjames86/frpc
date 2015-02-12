

(defpackage #:hello-rpc
  (:use #:cl))

(in-package #:hello-rpc)

(frpc:with-rpc-program (0)
  (frpc:with-rpc-version (0)
    (frpc:defrpc hello-world (:int32 :string) 0)
    (frpc:defhandler hello-world-handler (i) 0
      (log:info "i: ~A" i)
      (format nil "Hello ~A!" i))))

