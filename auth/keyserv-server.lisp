;;;; Copyright (c) Frank James 2015 <frank.a.james@gmail.com>
;;;; This code is licensed under the MIT license.


(in-package #:keyserv)


(defun auth-or-fail ()
  "The caller MUST be authenticated using AUTH-UNIX (or AUTH-SHORT)."
  (unless (equalp *rpc-remote-host* #(127 0 0 1)) (error 'rpc-auth-error))
  (let ((creds (get-unix-creds)))
    (if creds
        creds
        (error 'rpc-auth-error))))

(defvar *keylist* nil
  "List of (unix-creds netname secret-key public-key) for each known host.")

(defvar *keyfile-path* (merge-pathnames "keyserv.dat")
  "Pathname of where the key database file is located.")

(defun write-keylist ()
  "Write the keylist to this file whenever we add new credentials"
  (with-open-file (f *keyfile-path* 
                     :direction :output
                     :if-exists :supersede)
    (pprint *keylist* f)))

(defun read-keylist ()
  "Read the initial keylist from the file on first access to an empty *keylist*."
  (with-open-file (f *keyfile-path* 
                     :direction :input
                     :if-does-not-exist :create)
    (setf *keylist* (read f nil nil))))

(defun add-key-entry (creds netname secret-key)
  (declare (type frpc::auth-unix creds)
           (type string netname)
           (type vector secret-key))
  (unless *keylist* (read-keylist))
  (push 
   ;; convert the array version of the secret key into a bignum
   (let ((secret (keybuf-integer secret-key))) 
     (list creds 
           netname 
           secret 
           (frpc-des:des-public secret)))
   *keylist*)
  (write-keylist))

(defun find-key-entry (netname)
  "Find the entry for this netname. Reads from the local database if the entry list if empty."
  (unless *keylist* (read-keylist))
  (find-if (lambda (entry)
             (let ((name (second entry)))
               (string-equal name netname)))
           *keylist*))


(defhandler handle-null (void keyserv 1 0)
  (declare (ignore void))
  nil)

(defhandler handle-set (key keyserv 1 1)
  (let ((creds (auth-or-fail)))
    (add-key-entry creds (rpc-auth-principal) key)
    :success))

(defhandler handle-encrypt (arg keyserv 1 2)
  (auth-or-fail)
  (destructuring-bind (netname key) arg
    ;; find the entries for the client (caller) and server (named by netname)
    (let ((server-entry (find-key-entry netname))
          (client-entry (find-key-entry (rpc-auth-principal))))
      (if (and client-entry server-entry)
          (destructuring-bind (c n server-secret server-public) server-entry 
            (declare (ignore c n server-secret))
            (destructuring-bind (c n client-secret client-public) client-entry
              (declare (ignore c n client-public))
              ;; compute the common key of the client and the entry
              (let ((common (frpc-des::dh-common-key client-secret server-public)))
                (make-xunion :success 
                             (frpc-des::dh-encrypt-conversation-key common 
                                                                (concatenate '(vector (unsigned-byte 8))
                                                                             key))))))
          (make-xunion :unknown nil)))))

(defhandler handle-decrypt (arg keyserv 1 3)
  (auth-or-fail)
  (destructuring-bind (netname key) arg
    ;; find the entries for the server (caller) and client (named by netname)
    (let ((client-entry (find-key-entry netname))
          (server-entry (find-key-entry (rpc-auth-principal))))
      (if (and client-entry server-entry)
          (destructuring-bind (c n server-secret server-public) server-entry 
            (declare (ignore c n server-secret))
            (destructuring-bind (c n client-secret client-public) client-entry
              (declare (ignore c n client-public))
              ;; compute the common key of the client and the entry
              (let ((common (frpc-des::dh-common-key client-secret server-public)))
                (make-xunion :success (frpc-des::dh-decrypt-conversation-key common 
                                                                         (concatenate '(vector (unsigned-byte 8))
                                                                                      key))))))
          (make-xunion :unknown nil)))))

(defhandler handle-gen (void keyserv 1 4)
  (declare (ignore void))
  (auth-or-fail)
  (frpc-des:des-conversation))


