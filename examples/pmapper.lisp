
(defpackage #:port-mapper 
  (:use #:cl #:frpc))

(in-package #:port-mapper)

;; ------- port mapper ----------

(defxenum mapping-prot 
  ((:tcp 6)
   (:udp 17)))

(defxstruct mapping ()
  ((prog :uint32)
   (version :uint32)
   (prot mapping-prot :tcp)
   (port :uint32)))



(use-rpc-program 100000 2)

;; ----------------------

(defrpc portmapper-null 0 :void :void)

(defhandler %handle-null (void 0)
  (declare (ignore void))
  nil)

;; ---------------

(defrpc portmapper-set 1 mapping :boolean)

(defhandler %handle-set (mapping 1)
  (declare (ignore mapping))
  nil)

;; -------------------

(defrpc portmapper-unset 2 mapping :boolean)

(defhandler %handle-unset (mapping 2)
  (declare (ignore mapping))
  nil)

;; ----------------------

(defrpc portmapper-get-port 3 mapping :uint32)

(defhandler %handle-get-port (mapping 3)
  (declare (ignore mapping))
  0)

;; ------------------------

(defxstruct mapping-list ()
  ((map mapping)
   (next (:optional mapping-list))))

(defrpc portmapper-dump 4 :void (:optional mapping-list))
    
(defhandler %handle-dump (void 4)
  (declare (ignore void))
  nil)


;; ---------------------

(defxstruct mapping-call-args ()
  ((prog :uint32)
   (version :uint32)
   (proc :uint32)
   (args (:varray :octet))))

(defxstruct mapping-call-result ()
  ((port :uint32)
   (res (:varray :octet))))

(defrpc portmapper-call 5 mapping-call-args mapping-call-result)

(defhandler %handle-call (args 5)
  (declare (ignore args))
  (make-mapping-call-result))



	

