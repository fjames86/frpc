;;;; Copyright (c) Frank James 2015 <frank.a.james@gmail.com>
;;;; This code is licensed under the MIT license.

;;; Port mapper program, as specified in the RFC.
;;; Allows remote machines to query the ports on which to communicate
;;; with local RPC programs. Can also be used as a proxy to directly 
;;; execute RPCs via the CALLIT procedure.

(defpackage #:pmapper
  (:use #:cl #:frpc)
  (:export #:mapping
	   #:make-mapping
	   #:mapping-protocol
	   #:call-null
	   #:call-set
	   #:call-unset
	   #:call-get-port
	   #:call-dump
	   #:call-callit))

(in-package #:pmapper)

;; ------- port mapper structs ----------

(defconstant +pmapper-program+ 100000)
(defconstant +pmapper-version+ 2)

(use-rpc-program +pmapper-program+ +pmapper-version+)

(defxenum mapping-protocol
  ((:tcp 6)
   (:udp 17)))

(defxstruct mapping ()
  ((prog :uint32)
   (version :uint32)
   (protocol mapping-protocol :tcp)
   (port :uint32)))

;; ----------------------------------

;; Port mapper:
;; needs to keep a mapping of ports to mapping structs
;; 

(defparameter *mappings* nil)

(defun add-mapping (port mapping)
  "Add a port mapping."
  (let ((pair (assoc port *mappings*)))
    (if pair
	(setf (cdr pair) mapping)
	(push (cons port mapping) *mappings*))))

(defun rem-mapping (port)
  "Remove a port mapping."
  (setf *mappings* (remove port *mappings* :key #'car)))

(defun find-mapping-by-port (port)
  "Lookup a mapping given its port."
  (cdr (assoc port *mappings*)))

(defun find-mapping-by-program (program version &optional (protocol :tcp))
  "Lookup a mapping given its program/version and optionally communication protocol."
  (cdr (find-if (lambda (mapping)
		  (and (= (mapping-prog mapping) program)
		       (= (mapping-version mapping) version)
		       (if protocol 
			   (eq (mapping-protocol mapping) protocol)
			   t)))
		*mappings*
		:key #'cdr)))
		     


;; ----------------------

;; NULL -- test communication to the port mapper 

(defrpc %portmapper-null 0 :void :void)
(defun call-null (&key (host *rpc-host*) (port *rpc-port*))
  (%portmapper-null host nil :port port))

(defhandler %handle-null (void 0)
  (declare (ignore void))
  nil)

;; ---------------

;; SET -- set a port mapping 

(defrpc %portmapper-set 1 mapping :boolean)

(defun call-set (mapping &key (host *rpc-host*) (port *rpc-port*))
  (%portmapper-set host mapping :port port))

(defhandler %handle-set (mapping 1)
  (add-mapping (mapping-port mapping) mapping)
  t)

;; -------------------

;; UNSET -- remove a port mapping 

(defrpc %portmapper-unset 2 mapping :boolean)

(defun call-unset (mapping &key (host *rpc-host*) (port *rpc-port*))
  (%portmapper-unset host mapping :port port))

(defhandler %handle-unset (mapping 2)
  (cond
    ((find-mapping-by-port (mapping-port mapping))
     (rem-mapping (mapping-port mapping))
     t)
    (t nil)))
    
;; ----------------------

;; GET-PORT -- lookup a port mapping for a given program/version

(defrpc %portmapper-get-port 3 mapping :uint32)

(defun call-get-port (mapping &key (host *rpc-host*) (port *rpc-port*))
  (%portmapper-get-port host mapping :port port))

(defhandler %handle-get-port (mapping 3)
  (let ((mapping (find-mapping-by-program (mapping-prog mapping)
					  (mapping-version mapping)
					  (mapping-protocol mapping))))
    (if mapping
	(mapping-port mapping)
	0)))

;; ------------------------

;; DUMP -- list all mappings

(defxstruct mapping-list ()
  ((map mapping)
   (next (:optional mapping-list))))

(defrpc %portmapper-dump 4 :void (:optional mapping-list))
    
(defun call-dump (&key (host *rpc-host*) (port *rpc-port*))
  (do ((mlist (%portmapper-dump host nil :port port) 
	      (mapping-list-next mlist))
       (ms nil))
      ((null mlist) ms)
    (push (mapping-list-map mlist) ms)))

(defhandler %handle-dump (void 4)
  (declare (ignore void))
  (do ((mappings *mappings* (cdr mappings))
       (mlist nil))
      ((null mappings) mlist)
    (let ((map (cdr (car mappings))))
      (setf mlist 
	    (make-mapping-list :map map :next mlist)))))

;; ---------------------

;;(defxstruct mapping-call-result ()
;;  ((port :uint32)
;;   (res (:varray :octet))))

(defrpc %portmapper-callit 5 
  (:list :uint32 :uint32 :uint32 (:varray* :octet)) ;;prog version proc args)
  (:list :uint32 (:varray* :octet)))

(defun call-callit (proc packed-args &key (host *rpc-host*) (port *rpc-port*) (program 0) (version 0))
  (%portmapper-callit host (list program version proc packed-args)
		    :port port))

;; In the spec it says we should be able to call any (mapped) rpc on the local machine communicating
;; only via UDP. We run all RPC programs from within the same Lisp image so we can directly 
;; execute the handler without having to do any real proxy RPCs.
(defhandler %handle-callit (args 5)
  (destructuring-bind (program version proc arg-buffer) args
    ;; find the handler and port mapping 
    (let ((mapping (find-mapping-by-program program version))
	  (h (find-handler program version proc)))
      (cond
	((or (not mapping) (not h))
	 ;; error: no handler
	 (list 0 nil))
	(t 
	 ;; found the handler, run it
	 (destructuring-bind (reader writer handler) h
	   (let ((res (funcall handler (unpack reader arg-buffer))))
	     (list (mapping-port mapping)
		   (pack writer res)))))))))


