
(in-package #:frpc)

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

(defxstruct mapping-call-arg ()
  ((prog :uint32)
   (version :uint32)
   (proc :uint32)
   (args (:varray :octet))))

(defxstruct mapping-call-result ()
  ((port :uint32)
   (res (:varray :octet))))
