
(defpackage #:frpc.pmap
  (:use #:cl #:frpc)
  (:export #:mapping
	   #:mapping-protocol
	   #:call-null
	   #:call-set
	   #:call-unset
	   #:call-get-port
	   #:call-dump
	   #:call-call))

(in-package #:frpc.pmap)

;; ------- port mapper ----------

(defxenum mapping-protocol
  ((:tcp 6)
   (:udp 17)))

(defxstruct mapping ()
  ((prog :uint32)
   (version :uint32)
   (protocol mapping-protocol :tcp)
   (port :uint32)))


(use-rpc-program 100000 2)

;; ----------------------

(defrpc %portmapper-null 0 :void :void)
(defun call-null (&key (host *rpc-host*) (port *rpc-port*))
  (%portmapper-null host nil :port port))

(defhandler %handle-null (void 0)
  (declare (ignore void))
  nil)

;; ---------------

(defrpc %portmapper-set 1 mapping :boolean)

(defun call-set (mapping &key (host *rpc-host*) (port *rpc-port*))
  (%portmapper-set host mapping :port port))

(defhandler %handle-set (mapping 1)
  (declare (ignore mapping))
  nil)

;; -------------------

(defrpc %portmapper-unset 2 mapping :boolean)

(defun call-unset (mapping &key (host *rpc-host*) (port *rpc-port*))
  (%portmapper-unset host mapping :port port))

(defhandler %handle-unset (mapping 2)
  (declare (ignore mapping))
  nil)

;; ----------------------

(defrpc %portmapper-get-port 3 mapping :uint32)

(defun call-get-port (mapping &key (host *rpc-host*) (port *rpc-port*))
  (%portmapper-get-port host mapping :port port))

(defhandler %handle-get-port (mapping 3)
  (declare (ignore mapping))
  0)

;; ------------------------

(defxstruct mapping-list ()
  ((map mapping)
   (next (:optional mapping-list))))

(defrpc %portmapper-dump 4 :void (:optional mapping-list))
    
(defun call-dump (&key (host *rpc-host*) (port *rpc-port*))
  (do ((mlist (%portmapper-dump host nil :port port) (mapping-list-next mlist))
       (ms nil))
      ((null mlist) ms)
    (push (mapping-list-map mlist) ms)))

(defhandler %handle-dump (void 4)
  (declare (ignore void))
  nil)


;; ---------------------

;;(defxstruct mapping-call-result ()
;;  ((port :uint32)
;;   (res (:varray :octet))))

(defrpc %portmapper-call 5 
  (:list prog version proc args)
  (:list :uint32 (:varray :octet)))

(defun call-call (proc args &key (host *rpc-host*) (port *rpc-port*) (program 0) (version 0))
  (%portmapper-call host (list program version proc args)
		    :port port))

(defhandler %handle-call (args 5)
  (declare (ignore args))
  (list 0 nil))


