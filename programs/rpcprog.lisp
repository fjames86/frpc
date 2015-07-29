;;;; Copyright (c) Frank James 2015 <frank.a.james@gmail.com>
;;;; This code is licensed under the MIT license.

;;; This little program scrapes the IANA webpage to get a list of RPC 
;;; program number assignments. It can be used to resolve stringy names to RPC program numbers.
;;; 
;;; Requires drakma and cl-html-parse systems.
;;;

(defpackage #:rpcprog
  (:use #:cl #:drakma #:cl-html-parse)
  (:export #:find-program
           #:list-programs
           #:program-number 
           #:program-name 
           #:program-description))

(in-package #:rpcprog)

;; We scrape the IANA webpage to get the program names 
(defparameter *iana* "http://www.iana.org/assignments/rpc-program-numbers/rpc-program-numbers.xhtml")

(defun %get-rpc-programs1 ()
  (parse-html (http-request *iana*)))

;; Hopefully IANA won't change their webpage any time soon.
(defun %get-rpc-programs2 ()
  (cdddr (caddr (car (cdddr (cddddr (caddr (cadr (%get-rpc-programs1)))))))))

(defun destructure-entry (entry)
  (destructuring-bind (tr left center right &rest ignores) entry 
    (declare (ignore tr ignores))
    (let ((desc (cadr left))
          (number (cadr center))
          (name (cadr right)))
      (let ((n (parse-integer number :junk-allowed t)))
        (when (and name n)
          (list name n desc))))))

(defvar *entries* nil)
(defun get-entries (&optional force)
  (when (or force (not *entries*))
    (setf *entries*
          (mapcan (lambda (entry)
                    (let ((val (destructure-entry entry)))
                      (when val
                        (list val))))
                  (%get-rpc-programs2))))
  nil)

(defun find-program (id)
  "Lookup a program given its program number or string name."
  (declare (type (or integer string) id))
  (get-entries)
  (etypecase id 
    (string (find id *entries* :test #'string-equal :key #'car))
    (integer (find id *entries* :test #'= :key #'cadr))))

(defun program-number (name)
  "Lookup an RPC program number given its string name."
  (declare (type string name))
  (let ((prog (find-program name)))
    (when prog (cadr prog))))

(defun program-name (number)
  "Lookup a program name given its RPC number."
  (declare (type integer number))
  (let ((prog (find-program number)))
    (when prog (car prog))))

(defun program-description (id)
  "Lookup the program description as given by IANA."
  (let ((prog (find-program id)))
    (when prog (caddr prog))))

(defun list-programs ()
  "List all known RPC programs."
  (get-entries)
  *entries*)


