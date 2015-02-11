
(in-package #:frpc)

;; ------------------

;; enums 

(defxenum msg-type 
 ((:call 0)
  (:reply 1)))

(defxenum reply-stat
  ((:accepted 0)
   (:denied 1)))

(defxenum accept-stat
  ((:success 0)
   (:prog-unavail 1)
   (:prog-mismatch 2)
   (:proc-unavail 3)
   (:garbage-args 4)))

(defxenum reject-stat
  ((:rpc-mismatch 0)
   (:auth-error 1)))

(defxenum auth-stat
  ((:auth-badcred 1)
   (:auth-rejected 2)
   (:auth-badverf 3)
   (:auth-rejectedverf 4)
   (:auth-tooweak 5)))

(defxenum auth-flavour 
  ((:auth-null 0)
   (:auth-unix 1)
   (:auth-short 2)
   (:auth-des 3)))

;; --------------

;; structs

(defxstruct opaque-auth ()
  ((flavour auth-flavour :auth-null)
   (data (:varray :octet 400))))

(defparameter *default-opaque-auth* (make-opaque-auth :flavour :auth-null))

(defxstruct call-body ()
  ((rpcvers :uint32 2) ;; must always be 2
   (prog :uint32)
   (vers :uint32)
   (proc :uint32)
   (auth opaque-auth *default-opaque-auth*)
   (verf opaque-auth *default-opaque-auth*))) ;; parameters start here

(defxstruct accepted-reply () 
  ((verf opaque-auth *default-opaque-auth*)
   (reply-data 
    (:union accept-stat
      (:success ;; results follow
       :void)
      (:prog-mismatch 
       (:alist (low :uint32) (high :uint32)))
      (otherwise :void)))))

(defxunion rejected-reply (reject-stat)
  ((:rpc-mismatch
    (:alist (low :uint32) (high :uint32)))
   (:auth-error auth-stat)))

(defxunion reply-body (reply-stat)
  ((:msg-accepted accepted-reply)
   (:msg-rejected rejected-reply)))

(defxstruct rpc-msg ()
  ((xid :uint32 0)
   (body 
    (:union msg-type 
      (:call call-body)
      (:reply reply-body)))))


;; ------ todo: implement the authentication stuff ------------

;; 9.1 null authentication

;; 9.2 UNIX authentication

(defxstruct auth-unix ()
  ((stamp :uint32)
   (machine-name :string)
   (uid :uint32)
   (gid :uint32)
   (gids (:varray :uint32 16))))

;; 9.3 DES authentication

(defxenum authdes-namekind 
  ((:adn-fullname 0)
   (:adn-nickname 1)))

(defxtype des-block ()
  ((stream)
   (read-fixed-array #'read-octet stream 8))
  ((stream obj)
   (write-fixed-array #'write-octet stream obj)))
(defun make-des-block ()
  (nibbles:make-octet-vector 8))

(defxstruct authdes-fullname ()
  ((name :string)
   (key des-block)
   (window (:array :octet 4))))

(defxunion authdes-cred (authdes-namekind)
  ((:adn-fullname authdes-fullname (make-des-block))
   (:adn-nickname :int32)))

(defxstruct timestamp ()
  ((seconds :uint32)
   (useconds :uint32)))

(defxstruct authdes-verf-client ()
  ((adv-timestamp des-block (make-des-block))
   (adv-winverf (:array :octet 4) (nibbles:make-octet-vector 4))))

(defxstruct authdes-verf-server ()
  ((adv-timeverf des-block (make-des-block))
   (adv-nickname :int32)))

;; 9.3.5 Diffie-Hellman 


;; ----------------------------------------




     
