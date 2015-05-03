;;;; Copyright (c) Frank James 2015 <frank.a.james@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package #:frpc)


;; 9.3 DES authentication
;; Process is:
;; 1. Client generates a random DES key to be used for the conversation. 
;; 2. Client gets its local secret key and the public key of the server (by an unspecified mechanism)
;; 3. Client computes the common key by combining its secret and the server's public keys.
;; 4. Client encrypts the conversation key using the common key.
;; 5. Client forms a 2-block array and packs it with the current timestamp (1 block), the "window" (1/2 block)
;; and the "window - 1" (1/2 block). This is encrypted in DES CBC mode using the conversation key.
;; 6. Client sends the authdes-fullname and authdes-verf-client authenticator and verifier to the server.
;; 7. The server gets its secret key and the public key for the client and combines them to getthe common key.
;; 8. The server decrypts to get the conversation key.
;; 9. The server forms a 2-block array and decrypts using the conversation key. 
;; 10. The server validates that the timestamp is within a "reasonable" skew of its own clock and that the 
;; winverf = window - 1.
;; 11. The server stores the conversation key, window, name etc in some local storage and assigns the client an
;; integer "nickname". 
;; 12. The server returns the nickname and the same timestamp - 1 second (encrypted) back to the client.
;; 13. All subsequence client requests only send and encrypted timestamp (ECB mode) in the verifier, 
;; and the nickname in the authenticator.
;; 14. The server validates the timestamp is "within reasonable skew" and responds with the timestamp-1. 
;; The server is free to flush the contexts whenever it wishes.



;; ------------- the authenticator -------------

(defxenum authdes-namekind 
  (:fullname 0)
  (:nickname 1))

;; the client sends this to the server in the first request
(defxstruct authdes-fullname ()
  (name :string)
  (key (:varray* :octet)) ;; encrypted conversation key
  (window (:array :octet 4))) ;; encrypted window

(defxunion authdes-cred (authdes-namekind)
  (:fullname authdes-fullname) ;; only used in the first request
  (:nickname :int32)) ;; all other requests use the nickname 

;; ------------ the verifiers -------------------

(defxtype* authdes-timestamp ()
  (:plist :seconds :uint32
	  :useconds :uint32))

;; the verifier sent by the client
(defxstruct authdes-verf-client ()
  (adv-timestamp (:varray* :octet)) ;; encrypted timestamp
  (adv-winverf (:array :octet 4))) ;; encrypted (window - 1) only used in the first request

;; the verifier sent by the server 
(defxstruct authdes-verf-server ()
  (adv-timeverf (:varray* :octet)) ;; encrypted (timestamp - 1)
  (adv-nickname :int32)) ;; nickname to be used for the conversation

;; -----------------------------------------------

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

(defun dh-conversation-key ()
  "Generate a random 56-bit (8-octet) DES key to be used as the conversation key"
  (let ((key (nibbles:make-octet-vector 8)))
    (dotimes (i 8)
      (let ((n (random 256)))
	(when (zerop (logcount n))
	  (setf n (1+ n)))
	(setf (aref key i) n)))
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
  (let ((common (discrete-expt-modulo public secret +dh-modulus+)))
    (let ((bytes (nibbles:make-octet-vector 8)))
      (setf (nibbles:ub64ref/be bytes 0) (ash common -64))
      (dotimes (i 8)
	(let ((parity (logcount (aref bytes i))))
	  (unless (zerop parity)
	    (setf (aref bytes i)
		  (logior (aref bytes i) 1)))))
    bytes)))
    
(defun make-dh-cipher (key &optional initial)			     
  "Make a cipher to use for encryption. If used to enrypt the initial client verifier,
VERIFIER should be T. Otherwise VERIFIER should be nil."
  (ironclad:make-cipher :des
			:mode (if initial :cbc :ecb)
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

(defun dh-encrypt-conversation-key (common-key conv-key)
  (let ((c (make-dh-cipher common-key)))
    (dh-encrypt c conv-key)))

(defun dh-decrypt-conversation-key (common-key data)
  (let ((c (make-dh-cipher common-key)))
    (dh-decrypt c data)))

(defun des-timestamp (&optional seconds useconds)
  "Time since midnight March 1st 1970"
  (list :seconds (or seconds 
		     (- (get-universal-time)
			(encode-universal-time 0 0 0 1 3 1970 0)))  ;; note: march 1st, not Jan 1st!
	:useconds (or useconds 0)))

(defxtype* des-enc-block ((:reader read-des-enc-block) (:writer write-des-enc-block))
  (:list authdes-timestamp
	 :uint32 ;; window
	 :uint32)) ;; window-1

;; make the initial client request 
(defun des-initial-client-request (name secret public window)
  "Returns (values auth verf). the authenticator and verifier to send to the server."
  (let ((conversation (dh-conversation-key))
	(common (dh-common-key secret public)))
    ;; form a 2-block array and encrypt using the conversation key in CBC mode
    (let ((v (dh-encrypt (make-dh-cipher conversation
					 t)
			 (pack #'write-des-enc-block (list (des-timestamp) window (1- window))))))
      (values 
       (make-authdes-fullname :name name
			      :key (dh-encrypt-conversation-key common conversation)
			      :window (subseq v 8 12))
       (make-authdes-verf-client :adv-timestamp (subseq v 0 8)
				 :adv-winverf (subseq v 12 16))))))

  
;; allow 60 seconds difference
(defconstant +des-timestamp-skew+ 60)

;; server validates the client request
(defun des-valid-client-request (secret public auth verf)
  "This runs on the server and validates the initial client request."
  (let ((common (dh-common-key secret public)))
    ;; start by getting the converation key from the authenticator
    (let ((conversation (dh-decrypt-conversation-key common (authdes-fullname-key auth))))
      ;; now form the block and decrypt it
      (let ((v (dh-decrypt (make-dh-cipher conversation t)
			   (concatenate '(vector (unsigned-byte 8))
					(authdes-verf-client-adv-timestamp verf)
					(authdes-fullname-window auth)
					(authdes-verf-client-adv-winverf verf)))))
	;; unpack it 
	(destructuring-bind (timestamp window winverf) (unpack #'read-des-enc-block v)
	  ;; compare the timestamp and window
	  ;; FIXME: if it is valid then return allocate and return context
	  (and (< (abs (- (getf timestamp :seconds) (getf (des-timestamp) :seconds)))
		  +des-timestamp-skew+)
	       (= winverf (1- window))))))))

(defun des-client-verifier (conversation)
  "Generates a DES verifier for normal transactions."
  (let ((v (pack #'%write-authdes-timestamp (des-timestamp))))
    (make-authdes-verf-client :adv-timestamp (dh-encrypt (make-dh-cipher conversation) v)
			      :adv-winverf (nibbles:make-octet-vector 4))))

(defun des-server-verifier (conversation timestamp nickname)
  "Generate a DES verifier that the server responds with."
  (let ((v (pack #'%write-authdes-timestamp 
		 (des-timestamp (1- (getf timestamp :seconds))
				(getf timestamp :useconds)))))
    (make-authdes-verf-server :adv-timeverf (dh-encrypt (make-dh-cipher conversation) v)
			      :adv-nickname nickname)))

(defun des-valid-server-verifier (conversation timestamp verf)
  "Check the timestamp is 1- the timestamp we sent"
  (let ((v (authdes-verf-server-adv-timeverf verf)))
    (let ((ts (unpack #'%read-authdes-timestamp (dh-decrypt (make-dh-cipher conversation) v))))
      (= (1+ (getf ts :seconds) (getf timestamp :seconds))))))


;; --------------------------------------------



(defvar *des-private-key* nil
  "The server's private key.")

(defun des-init (secret)
  "Initialize the server with its secret key so it can accept DES authentication."
  (setf *des-private-key* secret))

;; need a database of public keys. Only clients which have an entry in this list may be authenticated
;; because we need to know their public key 
(defvar *des-public-keys* nil
  "List of public keys for each client that may talk to the server.")

(defstruct des-key fullname key nickname)

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

