;;;; Copyright (c) Frank James 2015 <frank.a.james@gmail.com>
;;;; This code is licensed under the MIT license.

;; this file defines the database codes for persisting the user public keys.
;; We mmap a file and carve it up into blocks of size +key-entry-size+ each (default 128 bytes).
;; Each entry MUST fit inside this.
;; The first block is reserved for the header, currently this is just an unsigned integer indicating the number
;; of entries in the database.
;;

(in-package #:frpc-des)

;; block size
(defconstant +key-entry-size+ 128)

;; path to the file 
(defvar *keylist-path* (merge-pathnames "frpc-des-keylist.dat" (user-homedir-pathname)))

;; the file mapping and its stream wrapper 
(defstruct db 
  mapping stream count)
(defvar *db* nil)
(defmacro with-locked-db (&body body)
  `(pounds:with-locked-mapping ((db-stream *db*)) ,@body))

;;(defvar *key-db* nil)
;;(defvar *key-db-stream* nil)

(defun key-db-count ()
  "Read the number of entries in the database."
  (file-position (db-stream *db*) 0)
  (nibbles:read-ub32/be (db-stream *db*)))

(defun close-key-file ()
  "Close the key database."
  (when *db* 
    (pounds:close-mapping (db-mapping *db*))
    (setf *db* nil)))

(defun open-key-file (&optional (count 32))
  "Open the key database, with at least COUNT available entries."
  (cond
    (*db*
     ;; the mapping is already open, check the real count matches the count we think it is
     (let ((real-count (with-locked-db (key-db-count))))
       (unless (= real-count (db-count *db*))
	 (close-key-file)
	 (open-key-file real-count))))
    (t
     (let ((mapping (pounds:open-mapping *keylist-path* (* +key-entry-size+ count))))
       (setf *db* (make-db :mapping mapping
			   :stream (pounds:make-mapping-stream mapping)
			   :count count))
       ;; read the header, if the count is less than the size then remap
       (let ((real-count (key-db-count)))
	 (cond
	   ((zerop real-count)
	    ;; the count is zero ,this means it is a freshly created file
	    ;; write the initial count
	    (file-position (db-stream *db*)  0)
	    (nibbles:write-ub32/be count (db-stream *db*)))
	   ((> real-count count)
	    ;; the real count is larger than the requested count, means we need to remap to a larger size
	    (close-key-file)
	    (open-key-file real-count))
	   ((< real-count count)
	    ;; the requested count is greater than the current count so write the new count to the header
	    (file-position (db-stream *db*) 0)
	    (nibbles:write-ub32/be count (db-stream *db*)))))))))

(defxstruct key-list-entry ((:reader read-key-list-entry) (:writer write-key-list-entry))
  (active :boolean)
  (timestamp :uint64)
  (name :string)
  (key (:varray* :octet 48)))

(defun integer-keybuf (integer)
  "Convert a bignum to a keybuffer"
  (do ((nums nil)
       (n integer (ash n -8)))
      ((zerop n) (apply #'vector (nreverse nums)))
    (push (mod n 256) nums)))

(defun keybuf-integer (keybuf)
  "Convert a keybuffer back to a bignum."
  (do ((i (1- (length keybuf)) (1- i))
       (n 0))
      ((< i 0) n)
    (setf n (+ (ash n 8) (aref keybuf i)))))


(defun add-public-key (name public &optional (modify t))
  "Create a new entry in the database. If this entry already exists then it is modified."
  (declare (type string name)
	   (type integer public))
  (open-key-file)
  ;; walk the mapping to find an empty entry, otherwise expand the mapping 
  (let ((count 0))
    (with-locked-db 
      (setf count (key-db-count))
      (do ((i 1 (1+ i)))
	  ((>= i count))
	(file-position (db-stream *db*) (* +key-entry-size+ i))
	(let ((entry (read-key-list-entry (db-stream *db*))))
	  (cond
	    ((and (key-list-entry-active entry)
		  (string-equal (key-list-entry-name entry) name)
		  modify)
	     ;; this is the same entry, update its key and timestamp
	     (setf (key-list-entry-key entry) (integer-keybuf public)
		   (key-list-entry-timestamp entry) (get-universal-time))
	     (file-position (db-stream *db*) (* +key-entry-size+ i))
	     (write-key-list-entry (db-stream *db*) entry)
	     (force-output (db-stream *db*)) ;; ensure the buffers are flushed to disk
	     (return-from add-public-key))
	    ((not (key-list-entry-active entry))
	     ;; this entry is free, use it
	     (setf (key-list-entry-active entry) t
		   (key-list-entry-timestamp entry) (get-universal-time)
		   (key-list-entry-name entry) name
		   (key-list-entry-key entry) (integer-keybuf public))
	     (file-position (db-stream *db*) (* +key-entry-size+ i))
	     (write-key-list-entry (db-stream *db*) entry)
	     (force-output (db-stream *db*)) ;; ensure the buffers are flushed
	     (return-from add-public-key))))))
    ;; no free entries, expand the mapping 
    (close-key-file)
    (open-key-file (* count 2))
    (file-position (db-stream *db*) (* +key-entry-size+ count))
    (let ((entry (make-key-list-entry :name name
				      :timestamp (get-universal-time)
				      :key (integer-keybuf public)
				      :active t)))
      (with-locked-db 
	(write-key-list-entry (db-stream *db*) entry)
	(force-output (db-stream *db*)))))
  nil)

(defun remove-public-key (name)
  "Delete the entry for this name from the database."
  (declare (type string name))
  (open-key-file)
  ;; walk the mapping to find the entry
  (with-locked-db 
    (let ((count (key-db-count)))
      (dotimes (i (1- count))
	(file-position (db-stream *db*) (* +key-entry-size+ (1+ i)))
	(let ((entry (read-key-list-entry (db-stream *db*))))
	  (when (and (key-list-entry-active entry)
		     (string-equal (key-list-entry-name entry) name))
	    ;; this entry is in use, compare the name
	    (setf (key-list-entry-active entry) nil)
	    (file-position (db-stream *db*) (* +key-entry-size+ (1+ i)))
	    (write-key-list-entry (db-stream *db*) entry)
	     (force-output (db-stream *db*)) ;; ensure the buffers are flushed
	    (return-from remove-public-key nil))))))
  nil)

(defun find-public-key (name)
  "Look up the public key for this name. Returns the public key or nil if not found."
  (declare (type string name))
  ;; walk the file to find the entry, or fail
  (with-locked-db 
    (let ((count (key-db-count)))
      (do ((i 1 (1+ i)))
	  ((= i count))
	(file-position (db-stream *db*) (* +key-entry-size+ i))
	(let ((entry (read-key-list-entry (db-stream *db*))))
	  (when (and (key-list-entry-active entry)
		     (string-equal (key-list-entry-name entry) name))
	    (return-from find-public-key
	      (keybuf-integer (key-list-entry-key entry)))))))))
  
(defun list-public-keys ()
  "Enumerate all available public key entries."
  (open-key-file)
  (with-locked-db 
    (let ((count (key-db-count)))
      (do ((i 1 (1+ i))
	   (entries nil))
	  ((= i count) entries)
	(file-position (db-stream *db*) (* +key-entry-size+ i))
	(let ((entry (read-key-list-entry (db-stream *db*))))
	  (when (key-list-entry-active entry)
	    (push (list :name (key-list-entry-name entry)
			:public (keybuf-integer (key-list-entry-key entry))
			:timestamp (key-list-entry-timestamp entry))
		  entries)))))))
  
