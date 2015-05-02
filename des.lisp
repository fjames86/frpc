;;;; Copyright (c) Frank James 2015 <frank.a.james@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package #:frpc)


;; 9.3 DES authentication

;; For DES-based authentication, the first transaction is treated as follows:
;; 1. client sends a verifier (authdes-verf-client). The timestamp slot is and encrypted timestamp 
;; and the windows is the same timestamp -1, i.e. and encrypted (1- timestamp). 
;; 2. The server will respond to the client with the same encrypted timestmap - 1. 
;;
;; All other transactions are authenticated by validating that an encrypted 
;; timestamp is "close" to the current time

(defxenum authdes-namekind 
  (:adn-fullname 0)
  (:adn-nickname 1))

(defxstruct authdes-fullname ()
  (name :string)
  (key (:varray* :octet)) ;; encrypted conversion key
  (window (:array :octet 4)))

(defxunion authdes-cred (authdes-namekind)
  (:adn-fullname authdes-fullname)
  (:adn-nickname :int32))

;; for verifiers
(defxtype* authdes-timestamp ()
  (:plist :seconds :uint32
	  :useconds :uint32))

;; the verifier sent by the client
(defxstruct authdes-verf-client ()
  (adv-timestamp (:varray* :octet)) ;; encrypted timestamp
  (adv-winverf (:array :octet 4))) ;; encrypted window

;; the verifier sent by the server 
(defxstruct authdes-verf-server ()
  (adv-timeverf (:varray* :octet)) ;; encrypted timestamp -1 
  (adv-nickname :int32)) ;; nickname to be used for the conversation

(defun make-auth-des-client-verifier (timestamp window)
  (let ((v (nibbles:make-octet-vector 16)))
    ;; pack the structure
    (setf (nibbles:ub32ref/be v 0) (getf timestamp :seconds)
	  (nibbles:ub32ref/be v 4) (getf timestamp :useconds)
	  (nibbles:ub32ref/be v 8) window
	  (nibbles:ub32ref/be v 12) (1- timestamp))
    ;; FIXME: encrypt v
    (make-authdes-verf-client :adv-timestamp (subseq v 0 8)
			      :adv-winverf (subseq v 12 16))))

;; Diffie-Hellman encryption

;; these constants are specified in the rfc
(defconstant +dh-base+ 3)
(defconstant +dh-modulus+ (parse-integer "d4a0ba0250b6fd2ec626e7efd637df76c716e22d0944b88b" 
					 :radix 16))
(defun dh-key (number)
  "Convert a number into a 192-bit DH key."
  (let ((key (nibbles:make-octet-vector 24)))
    (dotimes (i 24)
      (setf (aref key (- 24 i)) (logand number #xff)
	    number (ash number -8)))
    key))

(defun dh-secret-key ()
  (do ((secret 0)
       (i 0 (1+ i)))
      ((= i 8) secret)
    (setf secret (+ (* secret 256) (random 256)))))

(defun discrete-expt-modulo (base exponent modulo)
  "Compute the value base**exponent % modulo. Uses the right-to-left binary method, as described on wikipedia."
  (declare (type integer base exponent modulo))
  (do ((result 1)
       (base (mod base modulo)))
      ((zerop exponent) result)
    (when (= (mod exponent 2) 1)
      (setf result (mod (* result base) modulo)))
    (setf exponent (ash exponent -1)
	  base (mod (* base base) modulo))))

(defun dh-public-key (secret)
  "Generate the public key from the secret key."
  (discrete-expt-modulo +dh-base+ secret +dh-modulus+))

(defun dh-common-key (secret public)
  "Generate the common key from the local private key and remote public key."
  (discrete-expt-modulo public secret +dh-modulus+))

(defun dh-conversation-key (common)
  "Generate the 56-bit conversation key from the 192-bit common key."
  (let ((bytes (nibbles:make-octet-vector 8)))
    (setf (nibbles:ub64ref/be bytes 0) (ash common -64))
    (dotimes (i 8)
      (let ((parity (logcount (aref bytes i))))
	(unless (zerop parity)
	  (setf (aref bytes i)
		(logior (aref bytes i) 1)))))
    bytes))

(defun make-dh-cipher (key &optional verifier)
  "Make a cipher to use for encryption. If used to enrypt the initial client verifier,
VERIFIER should be T. Otherwise VERIFIER should be nil."
  (ironclad:make-cipher :des
			:mode (if verifier :cbc :ebc)
			:key key
			:initialization-vector (nibbles:make-octet-vector 8)))

(defun dh-encrypt (cipher data)
  (let ((result (nibbles:make-octet-vector (length data))))
    (ironclad:encrypt cipher data result)
    result))

(defun dh-decrypt (cipher data)
  (let ((result (nibbles:make-octet-vector (length data))))
    (ironclad:decrypt cipher data result)
    result))

(defun des-timestamp ()
  "Time since midnight March 1st 1970"
  (list :seconds (- (get-universal-time)
		    (encode-universal-time 0 0 0 1 3 1970 0))  ;; note: march 1st, not Jan 1st!
	:useconds 0))

(defun make-auth-des (&key fullname public-key conversation-key nickname window)
  "Make an AUTH-DES opaque-auth structure. If NICKNAME is provided then is should be used, otherwise 
FULLNAME and KEY must be provided."
  (make-opaque-auth :auth-des 
		    (if nickname
			(make-xunion :adn-nickname nickname)
			(let ((c (make-dh-cipher public-key)))
			  (make-xunion 
			   :adn-fullname 
			   (make-authdes-fullname 
			    :name fullname
			    :key (dh-encrypt c conversation-key)
			    :window (dh-encrypt c 
						(let ((v (nibbles:make-octet-vector 4)))
						  (setf (nibbles:ub32ref/be v 0) 
							(or window 0))
						  v))))))))



(defvar *des-private-key* nil
  "The server's private key.")

(defun des-init (secret)
  (setf *des-private-key* secret))

;; need a database of public keys. Only clients which have an entry in this list may be authenticated
;; because we need to know their public key 
(defvar *des-public-keys* nil
  "List of public keys for each client that may talk to the server.")

(defstruct des-key
  fullname key)

(defun add-des-public-key (fullname key)
  (push (make-des-key :fullname fullname
		      :key key)
	*des-public-keys*))

(defun find-des-public-key (fullname)
  (let ((d (find-if (lambda (d)
		      (string-equal (des-key-fullname d) fullname))
		    *des-public-keys*)))
    (when d 
      (des-key-key d))))



;; client contexts -- basically information about which clients have been authenticated
(defstruct des-context 
  fullname nickname timestamp key)

(defvar *des-contexts* (make-cyclic-buffer 10))

(defun add-des-context (fullname timestamp key)
  (let ((c (make-des-context :fullname fullname
			     :nickname (random (expt 2 32))
			     :timestamp timestamp
			     :key key)))
    (cyclic-push *des-contexts* c)
    c))

(defun find-des-context (nickname)
  (cyclic-find-if (lambda (c)
		    (= (des-context-nickname c) nickname))
		  *des-contexts*))
