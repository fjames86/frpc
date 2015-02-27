

(in-package  #:frpc)

(defxenum gss-proc-t 
  ((:data 0)
   (:init 1)
   (:continue 2)
   (:destroy 3)))

(defxenum gss-service-t 
  ((:none 1)
   (:integrity 2)
   (:privacy 3)))

(defconstant +gss-version+ 1)

(defxstruct gss-cred ()
  ((version :uint32 +gss-version+)
   (proc gss-proc-t)
   (seqno :uint32)
   (service gss-service-t)
   (handle (:varray* :octet))))

(defxstruct gss-init-res ()
  ((handle (:varray* :octet))
   (major :uint32)
   (minor :uint32)
   (window :uint32)
   (token (:varray* :octet))))

(defxstruct gss-integ-data ()
  ((integ (:varray* :octet))
   (checksum (:varray* :octet))))

;;(defxstruct gss-data-t ()
;;  ((seqno :uint32)
;;   (arg proc-req-arg-t)))

(defxenum gss-major-stat
  ((:complete #x00000000)
   (:continue-needed #x00000001)
   (:duplicate-token #x00000002)
   (:old-token #x00000004)
   (:unseq-token #x00000008)
   (:gap-token #x00000010)
   (:bad-mech #x00010000)
   (:bad-name #x00020000)
   (:bad-nametype #x00030000)
   (:bad-bindings #x00040000)
   (:bad-status #x00050000)
   (:bad-mic #x00060000)
   (:bad-sig #x00060000)
   (:no-cred #x00070000)
   (:no-context #x00080000)
   (:defective-token #x00090000)
   (:defective-credential #x000A0000)
   (:credentials-expired #x000B0000)
   (:context-expired #x000C0000)
   (:failure #x000D0000)
   (:bad-qop #x000E0000)
   (:unauthorized #x000F0000)
   (:unavailable #x00100000)
   (:duplicate-element #x00110000)
   (:name-not-mn #x00120000)
   (:call-inaccessible-read #x01000000)
   (:call-inaccessible-write #x02000000)
   (:call-bad-structure #x03000000)))

