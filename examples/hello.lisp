

(defpackage #:hello-rpc
  (:use #:cl #:frpc))

(in-package #:hello-rpc)

;; define an interface and server handlers
(use-rpc-program 1)
(use-rpc-version 1)

(defrpc call-hello (:string :string) 0)
(defhandler handle-hello (msg) 0
  (format nil "Hello ~A!" msg))

(defrpc call-goodbye (:int32 :uint32) 1)
(defhandler handle-goodbye (i) 1
  (1+ (abs i)))

;; a newer version
(use-rpc-version 2)

(defxtype* name-list () (:varray :string))

(defrpc call-hello2 (:string name-list) 2)
(defhandler handle-hello2 (name) 2
  (make-array 3 :initial-contents (list "hello" name "!!!!")))

;; a different program
(use-rpc-program 2)
(use-rpc-version 1)

(defxstruct person ()
  ((name :string)
   (age :uint32)))

(defxtype* person-array () (:varray person))

(defrpc call-list-people (:void person-array) 1)
(defhandler handle-list-people (void) 1
  (declare (ignore void))
  (make-array 1 
	      :initial-contents (list (make-person :name "fred" :age 55))))


(defvar *server* nil)

(defun start ()
  (setf *server* (start-rpc-server 8000 '(1))))

(defun stop ()
  (stop-rpc-server *server*))

;; oh no! the server only accepts requests for program 1 RPCs, but we 
;; want it to also accept program 2 RPCs (i.e. LIST-PEOPLE). 
;; We can fix this without taking the server down, by running:
;; (push 2 (frpc::rpc-server-programs *server*))
;; the server now accepts program=2 RPCs




