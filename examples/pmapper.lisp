
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

(defxstruct mapping-list ()
  ((map mapping)
   (next (:optional mapping-list))))

(defxstruct mapping-call-args ()
  ((prog :uint32)
   (version :uint32)
   (proc :uint32)
   (args (:varray :octet))))

(defxstruct mapping-call-result ()
  ((port :uint32)
   (res (:varray :octet))))


(with-rpc-program (100000)
  (with-rpc-version (2)
    (defrpc portmapper-null (:void :void) 0)
    (defhandler portmapper-null-handler (void) 0
      (declare (ignore void))
      nil)

    (defrpc portmapper-set (mapping :boolean) 1)
    (defhandler portmapper-set-handler (mapping) 1
      (declare (ignore mapping))
      nil)

    (defrpc portmapper-unset (mapping :boolean) 2)
    (defhandler portmapper-unset-handler (mapping) 2
      (declare (ignore mapping))
      nil)

    (defrpc portmapper-get-port (mapping :uint32) 3)
    (defhandler portmapper-get-port-handler (mapping) 3
      (declare (ignore mapping))
      0)

    (defrpc portmapper-dump (:void mapping-list) 4)
    (defhandler portmapper-dump-handler (void) 4
      (declare (ignore void))
      nil)

    (defrpc portmapper-call (mapping-call-args mapping-call-result) 5)
    (defhandler portmapper-call-handler (args) 5
      (declare (ignore args))
      (make-mapping-call-result))))

	

