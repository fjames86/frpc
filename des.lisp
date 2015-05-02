;;;; Copyright (c) Frank James 2015 <frank.a.james@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package #:frpc)


;; 9.3 DES authentication
;; Procress is as follows:
;; 1. Client sends an authdes-cred (fullname) structure to the server. This contains the conversion key
;; and a time window, both encrypted in the server's public key. The window is the amount of time the 
;; credential should be valid for. 
;; In this initial request, the client ALSO sends a verifier which contains the current timestamp,
;; the same window as in the authenticator, and the window -1. This structure is encrypted 
;; in DES CBC mode using the conversation key and null IV.
;; 2. The server decrypts the authenticator to extract the conversation key. This is then used 
;; to decrypt the verifier. The server validates that the timestamp, window and window-1 are 
;; all consistent. It then assigns this client a nickname (a random number) and returns the same timestamp-1
;; (encrypted) and the nickname. 
;; 3. The client validates that the verifier the server responded with is valid by decrypting and checking the 
;; timestamp.
;; 4. All subsequent client requests now send an authenticator consisting just of the nickname. The 
;; verifier should now be the encryted timestamp and encrypted window verifier.

(defxenum authdes-namekind 
  (:fullname 0)
  (:nickname 1))

(defxstruct authdes-fullname ()
  (name :string)
  (key (:varray* :octet)) ;; encrypted conversation key
  (window (:array :octet 4))) ;; encrypted window

(defxunion authdes-cred (authdes-namekind)
  (:fullname authdes-fullname)
  (:nickname :int32))

;; for verifiers
(defxtype* authdes-timestamp ()
  (:plist :seconds :uint32
	  :useconds :uint32))

;; the verifier sent by the client
(defxstruct authdes-verf-client ()
  (adv-timestamp (:varray* :octet)) ;; encrypted timestamp
  (adv-winverf (:array :octet 4))) ;; encrypted (window - 1)

;; the verifier sent by the server 
(defxstruct authdes-verf-server ()
  (adv-timeverf (:varray* :octet)) ;; encrypted (timestamp - 1)
  (adv-nickname :int32)) ;; nickname to be used for the conversation

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
  "Generate a random secret key"
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
			:mode (if verifier :cbc :ecb)
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

(defun des-timestamp (&optional seconds useconds)
  "Time since midnight March 1st 1970"
  (list :seconds (or seconds 
		     (- (get-universal-time)
			(encode-universal-time 0 0 0 1 3 1970 0)))  ;; note: march 1st, not Jan 1st!
	:useconds (or useconds 0)))

;; I don't think this works
(defun make-auth-des (&key fullname public-key conversation-key nickname window)
  "Make an AUTH-DES opaque-auth structure. If NICKNAME is provided then is should be used, otherwise 
FULLNAME and KEY must be provided."
  (make-opaque-auth :auth-des 
		    (if nickname
			(make-xunion :nickname nickname)
			(let ((c (make-dh-cipher public-key)))
			  (make-xunion 
			   :fullname 
			   (make-authdes-fullname 
			    :name fullname
			    :key (dh-encrypt c conversation-key)
			    :window (subseq 
				     (dh-encrypt c 
						 (let ((v (nibbles:make-octet-vector 8))) ;; what to put here?
						   (setf (nibbles:ub32ref/be v 0) 
							 (or window 0))
						   v))
				     0 4)))))))

(defxtype* des-enc-block ((:reader read-des-enc-block) (:writer write-des-enc-block))
  (:list authdes-timestamp
	 :uint32 
	 :uint32))

(defun pack-des-enc-block (key timestamp window)
  (dh-encrypt (make-dh-cipher key t)
	      (pack #'write-des-enc-block (list timestamp window (1- window)))))

(defun unpack-des-enc-block (key block)
  (unpack #'read-des-enc-block 
	  (dh-decrypt (make-dh-cipher key t) block)))

(defun make-des-client-verifier (key timestamp window)
  (let ((v (pack-des-enc-block key timestamp window)))
    (make-authdes-verf-client :adv-timestamp (subseq v 0 8)
			      :adv-winverf (subseq v 12 16))))

(defun make-des-server-verifier (key timestamp nickname)
  (let ((v (dh-encrypt (make-dh-cipher key) 
		       (pack #'%write-authdes-timestamp (des-timestamp (1- (getf timestamp :seconds))
								       (getf timestamp :useconds))))))
    (make-authdes-verf-server :adv-timeverf v
			      :adv-nickname nickname)))

;; allow 60 seconds difference
(defconstant +des-timestamp-skew+ 60)

(defun verify-des-client (key verf window)
  (let ((v (dh-decrypt (make-dh-cipher key t)
		       (flexi-streams:with-output-to-sequence (s)
			 (write-sequence (authdes-verf-client-adv-timestamp verf) s)
			 (nibbles:write-ub32/be window s)
			 (write-sequence (authdes-verf-client-adv-winverf verf) s)))))
    ;; check the timestamp
    (let ((ts (unpack #'%read-authdes-timestamp (subseq v 0 8)))
	  (wf (nibbles:ub32ref/be v 12)))
      (and (= wf (1- window))
	   (< (- (getf ts :seconds) (getf (des-timestamp) :seconds))
	      +des-timestamp-skew+)))))

(defun verify-des-server (key verf)
  (let ((v (unpack #'%read-authdes-timestamp 
		   (dh-decrypt (make-dh-cipher key)
			       (authdes-verf-server-adv-timeverf verf)))))
    (< (- (getf v :seconds) (getf (des-timestamp) :seconds))
       +des-timestamp-skew+)))











(defvar *des-private-key* nil
  "The server's private key.")

(defun des-init (secret)
  "Initialize the server with its secret key so it can accept DES authentication."
  (setf *des-private-key* secret))

;; need a database of public keys. Only clients which have an entry in this list may be authenticated
;; because we need to know their public key 
(defvar *des-public-keys* nil
  "List of public keys for each client that may talk to the server.")

(defstruct des-key fullname key)

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
  fullname nickname timestamp key window)

(defvar *des-contexts* (make-cyclic-buffer 10))

(defun add-des-context (fullname timestamp key window)
  (let ((c (make-des-context :fullname fullname
			     :nickname (random (expt 2 32))
			     :timestamp timestamp
			     :key key
			     :window window)))
    (cyclic-push *des-contexts* c)
    c))

(defun find-des-context (nickname)
  (cyclic-find-if (lambda (c)
		    (= (des-context-nickname c) nickname))
		  *des-contexts*))

