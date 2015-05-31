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
       (eq (mapping-protocol m1) (mapping-protocol m2))
       (if (and (mapping-port m1) (mapping-port m2)
                (not (zerop (mapping-port m1)))
                (not (zerop (mapping-port m2))))
           (= (mapping-port m1) (mapping-port m2))
           t)))

;; ----------------------------------

;; Port mapper:
;; needs to keep a mapping of ports to mapping structs
;; 

(defvar *mappings* nil)

(defun add-mapping (mapping)
  "Add a port mapping."
  ;; only add the mapping if it's not already mapped 
  (pushnew mapping *mappings* :test #'mapping-eql))

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

(defun generate-mapping-list (tcp-ports udp-ports)
  "Generate a list of all mappings for the programs hosted by this Lisp image."
  (let (mappings-to-add)
    (dolist (ppair frpc::*handlers*)
      (destructuring-bind (program . versions) ppair
        (dolist (vpair versions)
          (let ((version (car vpair)))
            (dolist (uport udp-ports)
              ;; only advertise the portmap program on port 111.
              ;; Yes, it can be contacted on ANY port our rpc server is running, but we keep that a secret.
              (when (or (and (= program +pmapper-program+) (= uport 111))
                        (and (not (= program +pmapper-program+)) (not (= uport 111))))
                (push (make-mapping :program program
                                    :version version
                                    :protocol :udp
                                    :port uport)
                      mappings-to-add)))
            (dolist (tport tcp-ports)
              (when (or (and (= program +pmapper-program+) (= tport 111))
                        (and (not (= program +pmapper-program+)) (not (= tport 111))))
                (push (make-mapping :program program
                                    :version version
                                    :protocol :tcp
                                    :port tport)
                      mappings-to-add)))))))
    mappings-to-add))

(defun add-all-mappings (tcp-ports udp-ports &key rpc)
  (let ((mappings-to-add (generate-mapping-list tcp-ports udp-ports)))
    (dolist (m mappings-to-add)
      ;; add the mapping to our local repository 
      (add-mapping m)
      ;; add to the remote portmap program
      (when rpc 
        (handler-case 
            (call-set m 
                      :host "localhost"
                      :client (make-instance 'unix-client))
          (error (e)
            (frpc-log :info "Failed to map ~A" e))))))
    nil)

(defun remove-all-mappings (tcp-ports udp-ports &key rpc)
  (let ((mappings-to-add (generate-mapping-list tcp-ports udp-ports)))
    (dolist (m mappings-to-add)
      ;; add the mapping to our local repository 
      (rem-mapping m)
      ;; call the remote portmap program
      (when rpc 
        (handler-case 
            (call-unset m 
                        :host "localhost"
                        :client (make-instance 'unix-client))
          (error (e)
            (frpc-log :info "Failed to unmap ~A" e))))))
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

;; only allow it to be called if the host is localhost and the user has been authenticated to some level
(defun auth-or-fail ()
  (when (or (not (equalp *rpc-remote-host* #(127 0 0 1)))
            (eq (opaque-auth-flavour *rpc-remote-auth*)
                :auth-null))
    (frpc-log :info "Rejected ~S ~S" *rpc-remote-host* (opaque-auth-flavour *rpc-remote-auth*))
    (error 'rpc-auth-error :stat :auth-tooweak)))

;; SET -- set a port mapping 

(defun %handle-set (mapping)
  (auth-or-fail)
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
  (auth-or-fail)
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
  (let ((m (find-if (lambda (m)
                      (and (= (mapping-program mapping) (mapping-program m))
                           (eq (mapping-protocol mapping) (mapping-protocol m))))
                    *mappings*)))
    (if m
        (mapping-port m)
        0)))

(defrpc call-get-port 3 mapping :uint32
  (:program port-mapper 2)
  (:arg-transformer (program version &key (query-protocol :udp))
		    (make-mapping :program program
				  :version version
				  :protocol (or query-protocol :udp)))
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
    (frpc-log :trace "CALLIT ~A:~A:~A" program version proc)
    (let ((mapping (find-mapping (make-mapping :program program
                                               :version version
                                               :protocol :udp)))
          (h (find-handler program version proc)))
      (cond
        ((and (null mapping) (null h))
         ;; error: no mapping or no handler. signalling an error here
         ;; causes the server to be silent (not reply)
         (error "proc not found"))
        (h
         ;; found the handler, run it
         (destructuring-bind (reader writer handler) h
           (if handler 
               (let ((res (funcall handler (unpack reader arg-buffer))))
                 (list (mapping-port mapping)
                       (pack writer res)))
               (error "no handler"))))
        (mapping
         ;; we have a mapping, but no handler defined. this means
         ;; the rpc server lives out-of-process i.e. in another Lisp image
         ;; we must therefore contact it via UDP and await a response.
         ;; NOTE: if the handler really is living in our image then
         ;; this will lock for 1 second because we have sent a 
         ;; message to ourselves.
         (let ((port (mapping-port mapping)))
           (let ((res 
                  (call-rpc #'frpc::write-octet-array
                            arg-buffer
                            #'frpc::read-octet-array 
                            :host "localhost"
                            :port port
                            :program program
                            :version version
                            :proc proc)))
             (list port res))))))))

(defrpc call-callit 5 
  (:list :uint32 :uint32 :uint32 (:varray* :octet)) ;;prog version proc args
  (:list :uint32 (:varray* :octet)) ;; port result 
  (:program port-mapper 2)
  (:arg-transformer (program version proc packed-args)
                    (list program version proc packed-args))
  (:transformer (res)
    (destructuring-bind (port b) res
      (values b port)))
  (:documentation 
   "Execute an RPC via the remote port mapper proxy. Returns (values PORT RES) where RES is an opaque array
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

(defxtype binding-list ()
  ((stream)
   (read-xtype-list stream 'binding))
  ((stream mappings)
   (write-xtype-list stream 'binding mappings)))

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
   (read-xtype-list stream 'rpcb-entry))
  ((stream list)
   (write-xtype-list stream 'rpcb-entry list)))


(defconstant +rpcbs-highproc+ 13)
(defconstant +rpcb-vers-stat+ 3)

(defxstruct rpcbs-addr ()
  (program :uint32)
  (version :uint32)
  (success :int32)
  (failure :int32)
  (netid :string))

(defxtype rpcbs-addr-list ()
  ((stream) (read-xtype-list stream 'rpcbs-addr))
  ((stream list) (write-xtype-list stream 'rpbs-addr list)))

(defxstruct rpcbs-rmtcall ()
  (program :uint32)
  (version :uint32)
  (proc :uint32)
  (success :int32)
  (failure :int32)
  (indirect :int32)
  (netid :string))

(defxtype rpcbs-rmtcall-list () 
  ((stream) (read-xtype-list stream 'rpcbs-rmtcall))
  ((stream list) (write-xtype-list stream 'rpcbs-rmtcall list)))

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

