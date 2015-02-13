
(in-package #:frpc)

;; ------------------

;; enums 

(defxenum msg-type 
 ((:call 0)
  (:reply 1)))

(defxenum reply-stat
  ((:msg-accepted 0)
   (:msg-denied 1)))

(defxenum accept-stat
  ((:success 0)
   (:prog-unavail 1)
   (:prog-mismatch 2)
   (:proc-unavail 3)
   (:garbage-args 4)
   (:failed 5))) ;; FJ

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
   (:msg-denied rejected-reply)))

(defxstruct rpc-msg ()
  ((xid :uint32 0)
   (body 
    (:union msg-type 
      (:call call-body)
      (:reply reply-body)))))

(defun make-rpc-request (program proc &key (version 0) auth verf (id 0))
  (unless auth (setf auth *default-opaque-auth*))
  (unless verf (setf verf *default-opaque-auth*))

  (make-rpc-msg :xid id
		:body (make-xunion :call
				   (make-call-body :prog program
						   :vers version
						   :proc proc
						   :auth auth
						   :verf verf))))

(defun make-rpc-response (&key accept reject verf (id 0) (high 0) (low 0) auth-stat)
  (unless verf (setf verf *default-opaque-auth*))

  (make-rpc-msg 
   :xid id
   :body 
   (make-xunion 
    :reply 
    (if accept
	(make-xunion 
	 :msg-accepted 
	 (make-accepted-reply 
	  :verf verf
	  :reply-data 
	  (case accept
	    (:success (make-xunion :success nil))
	    (:prog-mismatch (make-xunion :prog-mismatch `((high . ,high) (low . ,low))))
	    (otherwise (make-xunion accept nil)))))
	(make-xunion 
	 :msg-rejected
	 (ecase reject
	   (:rpc-mismatch (make-xunion :rpc-mismatch `((high . ,high) (low . ,low))))
	   (:auth-error (make-xunion :auth-error auth-stat))))))))
	    
      
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


(defparameter *rpc-program* 0)
(defparameter *rpc-version* 0)

(defmacro with-rpc-program ((program) &body body)
  `(let ((*rpc-program* ,program))
     ,@body))

(defmacro with-rpc-version ((version) &body body)
  `(let ((*rpc-version* ,version))
     ,@body))
	 
;; stores an assoc list for each program id
;; each program id stores an alist of version ids
;; each version id stores an aslist of handlers
(defparameter *handlers* nil)

(defun %defhandler (program version proc arg-type res-type handler)
  (let ((p (assoc program *handlers*)))
    (if p
	(let ((v (assoc version (cdr p))))
	  (if v
	      (let ((c (assoc proc (cdr v))))
		(if c
		    (progn
		      (setf (cdr c) (list arg-type res-type handler))
		      (return-from %defhandler))
		    (push (cons proc (list arg-type res-type handler)) (cdr v))))
	      (push (cons version (list (cons proc (list arg-type res-type handler))))
		    (cdr p))))
	(push (cons program
		    (list (cons version
				(list (cons proc (list arg-type res-type handler))))))
	      *handlers*)))
  nil)

(defun find-handler (program &optional version proc)
  (let ((p (assoc program *handlers*)))
    (if (and p version)
	(let ((v (assoc version (cdr p))))
	  (if (and v proc)
	      (cdr (assoc proc (cdr v)))
	      (cdr v)))
	(cdr p))))


