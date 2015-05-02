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

(defxtype des-block ()
  ((stream)
   (read-fixed-array #'read-octet stream 8))
  ((stream obj)
   (write-fixed-array #'write-octet stream obj)))

(defun make-des-block ()
  (nibbles:make-octet-vector 8))

(defxstruct authdes-fullname ()
  (name :string)
  (key des-block) ;; encrypted conversion key
  (window (:array :octet 4)))

(defxunion authdes-cred (authdes-namekind)
  (:adn-fullname authdes-fullname)
  (:adn-nickname :int32))

(defun make-auth-des (&key fullname key nickname)
  "Make an AUTH-DES opaque-auth structure. If NICKNAME is provided then is should be used, otherwise 
FULLNAME and KEY must be provided."
  (make-opaque-auth :auth-des 
		    (if nickname
			(make-xunion :adn-nickname nickname)
			(make-xunion :adn-fullname 
				     (make-authdes-fullname :name fullname
							    :key key
							    :window key)))))

;; for verifiers
(defxtype* authdes-timestamp ()
  (:plist :seconds :uint32
	  :useconds :uint32))

;; the verifier sent by the client
(defxstruct authdes-verf-client ()
  (adv-timestamp des-block) ;; encrypted timestamp
  (adv-winverf (:array :octet 4))) ;; encrypted window

;; the verifier sent by the server 
(defxstruct authdes-verf-server ()
  (adv-timeverf des-block) ;; encrypted timestamp -1 
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


