;;;; Copyright (c) Frank James 2015 <frank.a.james@gmail.com>
;;;; This code is licensed under the MIT license.

;;; This file implements portmap (version 2) and rpcbind (versions 3 and 4)

(defpackage #:frpc.bind 
  (:use #:cl #:frpc)
  (:export #:mapping
           #:make-mapping
           #:mapping-program
           #:mapping-version
           #:mapping-protocol
           #:mapping-port
           #:*pmap-port*

	   #:binding
	   #:binding-program
	   #:binding-version
	   #:binding-netid
	   #:binding-addr
	   #:binding-owner
	   
           ;; the rpc functions
           #:call-null
           #:call-set
           #:call-unset
           #:call-get-port
           #:call-dump
           #:call-callit

           #:call-null3
           #:call-set3
           #:call-unset3
           #:call-get-addr3
           #:call-dump3
           #:call-broadcast3
	   #:call-get-time3
	   #:call-uaddr2taddr3
	   #:call-taddr2uaddr3

           #:call-null4
           #:call-set4
           #:call-unset4
           #:call-get-addr4
           #:call-dump4
           #:call-broadcast4
	   #:call-get-time4
	   #:call-uaddr2taddr4
	   #:call-taddr2uaddr4
	   #:call-get-version-addr
	   #:call-indirect
	   #:call-get-addr-list
	   #:call-stat-by-version

           ;; underlying API
           #:add-mapping
           #:rem-mapping
           #:add-all-mappings
           #:remove-all-mappings
           #:find-mapping))

(in-package #:frpc.bind)

(defconstant +pmapper-program+ 100000)
(defprogram port-mapper 100000)
(use-rpc-host '*rpc-host* 111)


;; ------- port mapper structs ----------

(defxenum mapping-protocol
  (:tcp 6)
  (:udp 17))

(defxstruct mapping ()
  (program :uint32)
  (version :uint32)
  (protocol mapping-protocol :tcp)
  (port :uint32))

(defun mapping-eql (m1 m2)
  (and (= (mapping-program m1) (mapping-program m2))
       (= (mapping-version m1) (mapping-version m2))
       (eq (mapping-protocol m1) (mapping-protocol m2))))
;;       (= (mapping-port m1) (mapping-port m2))))

;; ----------------------------------

;; Port mapper:
;; needs to keep a mapping of ports to mapping structs
;; 

(defvar *mappings* nil)

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
  (let ((port (mapping-port mapping)))
    (find-if (lambda (m)
	       (and (mapping-eql m mapping)
		    (if map-port
			(= map-port port)
			t)))
	     *mappings*)))

(defun add-all-mappings (tcp-ports udp-ports &key rpc)
  "Add mappings for all defined RPCs to the TCP and UDP ports specified. If RPC is non-nil, the local port-mapper program will contacted using RPC, otherwise the local Lisp port-mapper program will have the mappings added directly."
  ;; if we are using RPC to contact the local port mapper, 
  ;; then ensure the port mapper program is actually running!
  (when rpc
    (handler-case (call-null :host "localhost" :protocol :udp)
      (rpc-timeout-error ()
        (error "Failed to contact local portmapper"))))
  (setf *mappings* nil)
  (dolist (ppair frpc::*handlers*)
    (destructuring-bind (program . versions) ppair
      (unless (and rpc (= program +pmapper-program+))
        (dolist (vpair versions)
          (let ((version (car vpair)))
            ;; add all TCP port mappings
            (dolist (port tcp-ports)
              (let ((mapping (make-mapping :program program
                                           :version version
                                           :port port)))
                (add-mapping mapping)
                (when rpc 
                  (call-set mapping :protocol :udp))))
            ;; add all UDP port mappings
            (dolist (port udp-ports)
              (let ((mapping (make-mapping :program program
                                           :version version
                                           :protocol :udp
                                           :port port)))
                (add-mapping mapping)
                (when rpc 
                  (call-set mapping :protocol :udp)))))))))
  nil)

(defun remove-all-mappings (&key rpc)
  "Remove mappings for all defined RPCs to the TCP and UDP ports specified. 
If RPC is non-nil, the local port-mapper program will contacted using RPC, 
otherwise the local Lisp port-mapper program will have the mappings 
removed from the Lisp list."
  ;; if we are using RPC to contact the local port mapper, 
  ;; then ensure the port mapper program is actually running!
  (when rpc
    (handler-case (call-null :host "localhost" :protocol :udp)
      (rpc-timeout-error ()
        (error "Failed to contact local portmapper"))))
  (when rpc 
    (dolist (mapping *mappings*)
      (call-unset mapping :protocol :udp)))
  (setf *mappings* nil)
  nil)


;; ----------------------

;; NULL -- test communication to the port mapper 

(defun %handle-null (void)
  (declare (ignore void))
  nil)

(defrpc call-null 0 :void :void
  (:program port-mapper 2)
  (:handler #'%handle-null))

;; ---------------

;; SET -- set a port mapping 

(defun %handle-set (mapping)
  (add-mapping mapping)
  t)

(defrpc call-set 1 mapping :boolean
  (:program port-mapper 2)
  (:arg-transformer (mapping) mapping)
  (:documentation "Set a port mapping.")
  (:handler #'%handle-set))

;; -------------------

;; UNSET -- remove a port mapping 

(defun %handle-unset (mapping)
  (when (find-mapping mapping)
    (rem-mapping mapping)
    t))

(defrpc call-unset 2 mapping :boolean
  (:program port-mapper 2)
  (:arg-transformer (mapping) mapping)
  (:documentation "Remove a port mapping.")
  (:handler #'%handle-unset))

;; ----------------------

;; GET-PORT -- lookup a port mapping for a given program/version

(defun %handle-get-port (mapping)
  (let ((m (find-mapping mapping)))
    (if m
	(mapping-port m)
	0)))

(defrpc call-get-port 3 mapping :uint32
  (:program port-mapper 2)
  (:arg-transformer (program version &key (query-protocol :udp))
    (make-mapping :program program
                  :version version
                  :protocol query-protocol))
  (:documentation "Query the port for the specified program.")
  (:handler #'%handle-get-port))

;; ------------------------

;; DUMP -- list all mappings

;; define our own type for a list of mappings
;; should be something like 
;; (defxstruct mapping-list () 
;;   (map mapping)
;;   (next (:optional mapping-list)))
;; but this doesn't scale well if the list is long 
;; so we use a hand-written iterative function instead

(defxtype mapping-list ()
  ((stream)
   (do ((maps nil)
	(done nil))
       (done maps)
     (let ((map (read-xtype 'mapping stream)))
       ;; modify the program id to a symbol (if its a known program)
;;       (let ((name (frpc::program-id (mapping-program map))))
;;	 (when name 
;;	   (setf (mapping-program map) name)))
       (push map maps)
       (let ((next (read-xtype :boolean stream)))
	 (unless next (setf done t))))))
  ((stream mlist)
   (do ((mlist mlist (cdr mlist)))
       ((null mlist))
     (write-xtype 'mapping stream (car mlist))
     (if (cdr mlist)
	 (write-xtype :boolean stream t)
	 (write-xtype :boolean stream nil)))))
       
(defun %handle-dump (void)
  (declare (ignore void))
  *mappings*)

(defrpc call-dump 4 :void (:optional mapping-list)
  (:program port-mapper 2)
  (:documentation "List all available port mappings.")
  (:handler #'%handle-dump))

;; ----------------------------------------

;; In the spec it says we should be able to call any (mapped) rpc on the local machine communicating
;; only via UDP. We run all RPC programs from within the same Lisp image so we can directly 
;; execute the handler without having to do any real proxy RPCs.
(defun %handle-callit (args)
  (destructuring-bind (program version proc arg-buffer) args
    ;; find the handler and port mapping 
    (frpc-log :info "CALLIT ~A:~A:~A" program version proc)
    (let ((mapping (find-mapping (make-mapping :program program
					       :version version
					       :protocol :udp)))
	  (h (find-handler program version proc)))
      (cond
	((or (not mapping) (not h))
	 ;; error: no mapping or no handler
	 (error "no handler"))
;;	 (list 0 nil))
	(t 
	 ;; found the handler, run it
	 (destructuring-bind (reader writer handler) h
	   (if handler 
	       (let ((res (funcall handler (unpack reader arg-buffer))))
		 (list (mapping-port mapping)
		       (pack writer res)))
	       (list 0 nil))))))))

(defrpc call-callit 5 
  (:list :uint32 :uint32 :uint32 (:varray* :octet)) ;;prog version proc args)
  (:list :uint32 (:varray* :octet))
  (:program port-mapper 2)
  (:arg-transformer (program version proc packed-args)
    (list program version proc packed-args))
  (:documentation 
   "Execute an RPC via the remote port mapper proxy. Returns (PORT ARGS) where ARGS is an opaque array
of the packed result. The result needs to be extracted using FRPC:UNPACK. The result type is 
recommended to be a well-defined type, i.e. represented by a symbol, so that it has an easy reader
function available.")
  (:handler #'%handle-callit))


;; ------------------------------------------------------------



(defxstruct binding ()
  (program :uint32)
  (version :uint32)
  (netid :string)
  (addr :string)
  (owner :string))

(defun read-type-list (stream type)
  (do ((list nil)
	(done nil))
       (done list)
     (push (read-xtype type stream) list)
     (let ((b (read-xtype :boolean stream)))
       (unless b (setf done t)))))

(defun write-type-list (stream type list)
  (do ((list list (cdr list)))
      ((null list))
    (write-xtype type stream (car list))
    (if (cdr list)
	(write-xtype stream :boolean t)
	(write-xtype stream :boolean nil))))

(defxtype binding-list ()
  ((stream)
   (read-type-list stream 'binding))
  ((stream mappings)
   (write-type-list stream 'binding mappings)))
       
(defxstruct rpcb-remote-call-arg ()
  (program :uint32)
  (version :uint32)
  (proc :uint32)
  (args (:varray* :octet)))

(defxtype* rpb-remote-call-res () 
  (:list :string ;; addr
	 (:varray* :octet))) ;; results

(defxstruct rpcb-entry ()
  (maddr :string)
  (netid :string)
  (semantics :uint32)
  (protof :string) ;; protocol family 
  (proto :string)) ;; protocol

(defxtype rpcb-entry-list ()
  ((stream)
   (read-type-list stream 'rpcb-entry))
  ((stream list)
   (write-type-list stream 'rpcb-entry list)))


(defconstant +rpcbs-highproc+ 13)
(defconstant +rpcb-vers-stat+ 3)

(defxstruct rpcbs-addr ()
  (program :uint32)
  (version :uint32)
  (success :int32)
  (failure :int32)
  (netid :string))

(defxtype rpcbs-addr-list ()
  ((stream) (read-type-list stream 'rpcbs-addr))
  ((stream list) (write-type-list stream 'rpbs-addr list)))
  
(defxstruct rpcbs-rmtcall ()
  (program :uint32)
  (version :uint32)
  (proc :uint32)
  (success :int32)
  (failure :int32)
  (indirect :int32)
  (netid :string))

(defxtype rpcbs-rmtcall-list () 
  ((stream) (read-type-list stream 'rpcbs-rmtcall))
  ((stream list) (write-type-list stream 'rpcbs-rmtcall list)))
  
(defxtype* rpcbs-proc () (:array :int32 +rpcbs-highproc+))

(defxstruct rpcb-stat ()
  (info rpcbs-proc)
  (setinfo :int32)
  (unsetinfo :int32)
  (addrinfo (:optional rpcbs-addr-list))
  (rmtinfo (:optional rpcbs-rmtcall-list)))

(defxtype* rpcb-stat-byvers () (:array rpcb-stat +rpcb-vers-stat+))

(defxtype* netbuf () (:list :uint32 (:varray* :octet)))

;; -------------- version 3 -----------

(defrpc call-null3 0 :void :void
  (:program port-mapper 3))

(defrpc call-set3 1 binding :boolean
  (:program port-mapper 3))

(defrpc call-unset3 2 binding :boolean
  (:program port-mapper 3))

(defrpc call-get-addr3 3
  binding
  :string
  (:program port-mapper 3))

(defrpc call-dump3 4
  :void
  (:optional binding-list)
  (:program port-mapper 3))

(defrpc call-broadcast3 5
  rpcb-rmtcall-args 
  rpcb-rmtcall-res
  (:program port-mapper 3))

(defrpc call-get-time3 6
  :void :uint32
  (:program port-mapper 3))

(defrpc call-uaddr2taddr3 7
  :string
  netbuf
  (:program port-mapper 3))

(defrpc call-taddr2uaddr3 8
  netbuf
  :string
  (:program port-mapper 3))

;; ------------- version 4 -------------

(defrpc call-null4 0 :void :void
  (:program port-mapper 4))

(defrpc call-set4 1 binding :boolean
  (:program port-mapper 4))

(defrpc call-unset4 2 binding :boolean
  (:program port-mapper 4))

(defrpc call-get-addr4 3
  binding
  :string
  (:program port-mapper 4))

(defrpc call-dump4 4
  :void
  (:optional binding-list)
  (:program port-mapper 4))

(defrpc call-broadcast4 5
  rpcb-rmtcall-args 
  rpcb-rmtcall-res
  (:program port-mapper 4))

(defrpc call-get-time4 6
  :void :uint32
  (:program port-mapper 4))

(defrpc call-uaddr2taddr4 7
  :string
  netbuf
  (:program port-mapper 4))

(defrpc call-taddr2uaddr4 8
  netbuf
  :string
  (:program port-mapper 4))

(defrpc call-get-version-addr 9
  binding :string
  (:program port-mapper 4))

(defrpc call-indirect 10
  rpcb-rmtcall-args
  rpcb-rmtcall-res
  (:program port-mapper 4))

(defrpc call-get-addr-list 11
  binding
  (:optional rpcb-entry-list)
  (:program port-mapper 4))

(defrpc call-stat-by-version 12
  :void rpcb-stat-by-vers
  (:program port-mapper 4))

;;; -----------------------

(defun call-null (&key (host *rpc-host*) (port 111) (protocol :udp) (version 4) (timeout 1) connection client)
  (ecase version
    (4 
     (call-null4 :host host :port port :protocol protocol
		 :timeout timeout :connection connection :client client))
    (3 
     (call-null3 :host host :port port :protocol protocol
		 :timeout timeout :connection connection :client client))))


