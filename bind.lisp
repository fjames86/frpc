;;;; Copyright (c) Frank James 2015 <frank.a.james@gmail.com>
;;;; This code is licensed under the MIT license.

;;; This file implements rpcbind (versions 3 and 4)
;;; This is essentially an upgraded version of port-mapper 

(defpackage #:frpc.bind 
  (:use #:cl #:frpc))

(in-package #:frpc.bind)

(defxstruct rpcb ()
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

(defxtype rpcb-list ()
  ((stream)
   (read-type-list stream 'rpcb))
  ((stream mappings)
   (write-type-list stream 'rpcb mappings)))
       
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

(defrpc call-set3 1 rpcb :boolean
  (:program port-mapper 3))

(defrpc call-unset3 2 rpcb :boolean
  (:program port-mapper 3))

(defrpc call-get-addr3 3
  rpcb
  :string
  (:program port-mapper 3))

(defrpc call-dump3 4
  :void
  (:optional rpcb-list)
  (:program port-mapper 3))

(defrpc call-callit3 5
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

(defrpc call-set4 1 rpcb :boolean
  (:program port-mapper 4))

(defrpc call-unset4 2 rpcb :boolean
  (:program port-mapper 4))

(defrpc call-get-addr4 3
  rpcb
  :string
  (:program port-mapper 4))

(defrpc call-dump4 4
  :void
  (:optional rpcb-list)
  (:program port-mapper 4))

(defrpc call-callit4 5
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
  rpcb :string
  (:program port-mapper 4))

(defrpc call-indirect 10
  rpcb-rmtcall-args
  rpcb-rmtcall-res
  (:program port-mapper 4))

(defrpc call-get-addr-list 11
  rpcb 
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


