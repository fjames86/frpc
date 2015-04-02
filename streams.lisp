;;;; Copyright (c) Frank James 2015 <frank.a.james@gmail.com>
;;;; This code is licensed under the MIT license.

(defpackage #:frpc.streams
  (:use #:cl #:trivial-gray-streams)
  (:export #:make-buffer-stream
	   #:allocate-buffer
	   #:with-buffer-stream
	   #:buffer-stream-buffer
	   #:buffer-stream-position))

(in-package #:frpc.streams)

(defclass buffer-stream (trivial-gray-stream-mixin 
			 fundamental-binary-input-stream 
			 fundamental-binary-output-stream)
  (#+cmu
   (open-p :initform t
           :accessor buffer-stream-open-p
           :documentation "For CMUCL we have to keep track of this manually.")
   (buffer :initarg :buffer :reader buffer-stream-buffer)
   (position :initform 0 :initarg :start :accessor buffer-stream-position)
   (end :initarg :end :reader buffer-stream-end)))

#+:cmu
(defmethod open-stream-p ((stream buffer-stream))
  "Returns a true value if STREAM is open.  See ANSI standard."
  (buffer-stream-open-p stream))

#+:cmu
(defmethod close ((stream buffer-stream) &key abort)
  "Closes the stream STREAM.  See ANSI standard."
  (declare (ignore abort))
  (prog1
      (buffer-stream-open-p stream)
    (setf (buffer-stream-open-p stream) nil)))

(defun check-if-open (stream)
  "Checks if STREAM is open and signals an error otherwise."
  (unless (open-stream-p stream)
    (error "stream closed")))

(defmethod stream-element-type ((stream buffer-stream))
  "The element type is always OCTET by definition."
  '(unsigned-byte 8))

;; use this to check if there are more bytes to read
(defmethod stream-listen ((stream buffer-stream))
  "checks whether there are bytes left to read"
  (check-if-open stream)
  (< (buffer-stream-position stream) 
     (buffer-stream-end stream)))

(defmethod stream-file-position ((stream buffer-stream))
  "Simply returns the index into the underlying vector."
  (buffer-stream-position stream))

(defmethod (setf stream-file-position) (position-spec (stream buffer-stream))
  "Sets the index into the underlying vector if POSITION-SPEC is acceptable."
  (setf (buffer-stream-position stream)
	(case position-spec
	  (:start 0)
	  (:end (buffer-stream-end stream))
	  (otherwise 
	   (unless (integerp position-spec) (error "Must be integer"))
	   position-spec)))
  (buffer-stream-position stream))

(defmethod stream-read-sequence ((stream buffer-stream) sequence start end &key)
  "Returns the index of last byte read."
  (declare (type fixnum start end))
  (let ((count (- end start))
	(buffer (buffer-stream-buffer stream)))
    (do ((i 0 (1+ i)))
	((= i count) (buffer-stream-position stream))
      (setf (elt sequence (+ start i))
	    (aref buffer (buffer-stream-position stream)))
      (incf (buffer-stream-position stream)))))
        
(defmethod stream-write-sequence ((stream buffer-stream) sequence start end &key)
  "Returns the index of last byte written."
  (declare (type fixnum start end))
  (let ((count (- end start))
	(buffer (buffer-stream-buffer stream)))
    (do ((i 0 (1+ i)))
	((= i count) sequence)
      (setf (aref buffer (buffer-stream-position stream))
	    (elt sequence (+ start i)))
      (incf (buffer-stream-position stream)))))

(defmethod stream-read-byte ((stream buffer-stream))
  "Returns the byte or :EOF"
  (cond
    ((< (buffer-stream-position stream)
	(buffer-stream-end stream))
     (prog1 (aref (buffer-stream-buffer stream)
		  (buffer-stream-position stream))
       (incf (buffer-stream-position stream))))
    (t :eof)))
  
(defmethod stream-write-byte ((stream buffer-stream) byte)
  (cond
    ((< (buffer-stream-position stream)
	(buffer-stream-end stream))
     (setf (aref (buffer-stream-buffer stream)
		 (buffer-stream-position stream))
	   byte)
     (incf (buffer-stream-position stream)))
    (t (error 'end-of-file))))


(defun make-buffer-stream (buffer &key (start 0) end)
  (make-instance 'buffer-stream
		 :buffer buffer
		 :start start
		 :end (or end (length buffer))))

(defmacro with-buffer-stream ((var buffer &key (start 0) end) &body body)
  "Execute the body in the context of a buffer stream. Returns the number of bytes written to the buffer."
  `(let ((,var (make-buffer-stream ,buffer :start ,start :end ,end)))
     ,@body
     (buffer-stream-position ,var)))

(defparameter *buffers* 
  (make-array 8 
	      :initial-contents 
	      (loop :for i :below 8 
		 :collect (nibbles:make-octet-vector 65536))))
(defparameter *index* 0)

(defun allocate-buffer ()
  "Returns one of a set of statically allocated buffers. Cycles round after a few calls."
  (setf *index* (mod (1+ *index*) 8))
  (aref *buffers* *index*))

