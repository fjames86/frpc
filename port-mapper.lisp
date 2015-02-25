;;;; Copyright (c) Frank James 2015 <frank.a.james@gmail.com>
;;;; This code is licensed under the MIT license.

;;; Port mapper program, as specified in the RFC.
;;; Allows remote machines to query the ports on which to communicate
;;; with local RPC programs. Can also be used as a proxy to directly 
;;; execute RPCs via the CALLIT procedure.

;; TODO: add support for the newer versions 3 and 4
;; TODO: add UDP broadcast to find services on the network

(in-package #:port-mapper)

(defparameter *pmap-port* 111)

;; ------- port mapper structs ----------

(defconstant +pmapper-program+ 100000)
(defconstant +pmapper-version+ 2)

(use-rpc-program +pmapper-program+ +pmapper-version+)

(defxenum mapping-protocol
  ((:tcp 6)
   (:udp 17)))

(defxstruct mapping ()
  ((program :uint32)
   (version :uint32)
   (protocol mapping-protocol :tcp)
   (port :uint32)))

(defun mapping-eql (m1 m2)
  (and (= (mapping-program m1) (mapping-program m2))
       (= (mapping-version m1) (mapping-version m2))
       (eq (mapping-protocol m1) (mapping-protocol m2))))
;;       (= (mapping-port m1) (mapping-port m2))))

;; ----------------------------------

;; Port mapper:
;; needs to keep a mapping of ports to mapping structs
;; 

(defparameter *mappings* nil)

(defun add-mapping (mapping)
  "Add a port mapping."
  (push mapping *mappings*))

(defun rem-mapping (mapping)
  "Remove a port mapping."
  (setf *mappings* 
	(remove-if (lambda (m)
		     (mapping-eql m mapping))
		   *mappings*)))

(defun find-mapping (mapping &optional map-port)
  "Lookup a port mapping matching the program, version and protocol specified
in the mapping structure. if MAP-PORT is provided, will also match this port."
  (with-slots (program version protocol port) mapping
    (find-if (lambda (m)
	       (and (mapping-eql m mapping)
		    (if map-port
			(= map-port port)
			t)))
	     *mappings*)))

(defun add-all-mappings (tcp-ports udp-ports)
  "Add mappings for all defined RPCs to the TCP and UDP ports specified."
  (setf *mappings* nil)
  (dolist (ppair frpc::*handlers*)
    (destructuring-bind (program . versions) ppair
      (dolist (vpair versions)
	(let ((version (car vpair)))
	  (dolist (port tcp-ports)
	    (add-mapping (make-mapping :program program
				       :version version
				       :port port)))
	  (dolist (port udp-ports)
	    (add-mapping (make-mapping :program program
				       :version version
				       :protocol :udp
				       :port port)))))))
  nil)

;; ----------------------

;; NULL -- test communication to the port mapper 

(defrpc %portmapper-null 0 :void :void)
(defun call-null (&key (host *rpc-host*) (port *pmap-port*) protocol)
  (%portmapper-null nil :host host :port port :protocol protocol))

(defhandler %handle-null (void 0)
  (declare (ignore void))
  nil)

;; ---------------

;; SET -- set a port mapping 

(defrpc %portmapper-set 1 mapping :boolean)

(defun call-set (mapping &key (host *rpc-host*) (port *pmap-port*) protocol)
  (%portmapper-set mapping :host host :port port :protocol protocol))

(defhandler %handle-set (mapping 1)
  (add-mapping mapping)
  t)

;; -------------------

;; UNSET -- remove a port mapping 

(defrpc %portmapper-unset 2 mapping :boolean)

(defun call-unset (mapping &key (host *rpc-host*) (port *pmap-port*) protocol)
  (%portmapper-unset mapping :host host :port port :protocol protocol))

(defhandler %handle-unset (mapping 2)
  (when (find-mapping mapping)
    (rem-mapping mapping)
    t))

;; ----------------------

;; GET-PORT -- lookup a port mapping for a given program/version

(defrpc %portmapper-get-port 3 mapping :uint32)

(defun call-get-port (mapping &key (host *rpc-host*) (port *pmap-port*) protocol)
  (%portmapper-get-port mapping :host host :port port :protocol protocol))

(defhandler %handle-get-port (mapping 3)
  (let ((m (find-mapping mapping)))
    (if m
	(mapping-port m)
	0)))

;; ------------------------

;; DUMP -- list all mappings

(defxstruct mapping-list ()
  ((map mapping)
   (next (:optional mapping-list))))

(defrpc %portmapper-dump 4 :void (:optional mapping-list))
    
(defun call-dump (&key (host *rpc-host*) (port *pmap-port*) protocol)
  (do ((mlist (%portmapper-dump nil :host host :port port :protocol protocol) 
	      (mapping-list-next mlist))
       (ms nil))
      ((null mlist) ms)
    (push (mapping-list-map mlist) ms)))

(defhandler %handle-dump (void 4)
  (declare (ignore void))
  (do ((mappings *mappings* (cdr mappings))
       (mlist nil))
      ((null mappings) mlist)
    (let ((map (car mappings)))
      (setf mlist 
	    (make-mapping-list :map map :next mlist)))))

;; ---------------------

;;(defxstruct mapping-call-result ()
;;  ((port :uint32)
;;   (res (:varray :octet))))

(defrpc %portmapper-callit 5 
  (:list :uint32 :uint32 :uint32 (:varray* :octet)) ;;prog version proc args)
  (:list :uint32 (:varray* :octet)))

(defun call-callit (proc packed-args &key (host *rpc-host*) (port *pmap-port*) (program 0) (version 0) protocol)
  "Execute an RPC via the remote port mapper proxy. Returns (PORT ARGS) where ARGS is an opaque array
of the packed result. The result needs to be extracted using FRPC:UNPACK. The result type is 
recommended to be a well-defined type, i.e. represented by a symbol, so that it has an easy reader
function available."
  (%portmapper-callit (list program version proc packed-args)
		      :host host
		      :port port
		      :protocol protocol))

;; In the spec it says we should be able to call any (mapped) rpc on the local machine communicating
;; only via UDP. We run all RPC programs from within the same Lisp image so we can directly 
;; execute the handler without having to do any real proxy RPCs.
(defhandler %handle-callit (args 5)
  (destructuring-bind (program version proc arg-buffer) args
    ;; find the handler and port mapping 
    (let ((mapping (find-mapping (make-mapping :program program
					       :version version
					       :protocol :udp)))
	  (h (find-handler program version proc)))
      (cond
	((or (not mapping) (not h))
	 ;; error: no mapping or no handler
	 (list 0 nil))
	(t 
	 ;; found the handler, run it
	 (destructuring-bind (reader writer handler) h
	   (if handler 
	       (let ((res (funcall handler (unpack reader arg-buffer))))
		 (list (mapping-port mapping)
		       (pack writer res)))
	       (list 0 nil))))))))

