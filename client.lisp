
(in-package #:frpc)

(defun call-rpc (host arg-type arg result-type 
		 &key (port 111) (program 0) (version 0) auth verf (request-id 0) (proc 0))
				     
  (let ((socket (usocket:socket-connect host port 
					:element-type '(unsigned-byte 8))))
    (unwind-protect 
	 (let ((stream (usocket:socket-stream socket)))
	   ;; write the request content
	   (%write-rpc-msg 
	    stream
	    (make-rpc-msg 
	     :xid request-id
	     :body 
	     (make-xunion :call
			  (make-call-body :prog program
					  :vers version
					  :proc proc
					  :auth (if auth 
						    auth
						    *default-opaque-auth*)
					  :verf (if verf
						    verf
						    *default-opaque-auth*)))))
	   (funcall (xtype-writer arg-type) stream arg)
	   ;; read the response 
	   (funcall (xtype-reader result-type) stream))
      (usocket:socket-close socket))))


(defparameter *rpc-program* 0)
(defparameter *rpc-version* 0)

(defmacro defrpc (name (arg-type result-type) proc &key (program '*rpc-program*) (version '*rpc-version*))
  `(defun ,name (host arg &key (port 111) auth verf (request-id 0))
     (call-rpc host ',arg-type arg ',result-type
	       :port port
	       :program ,program
	       :version ,version
	       :auth auth
	       :verf verf
	       :request-id request-id
	       :proc ,proc)))

(defmacro with-rpc-program ((program) &body body)
  `(let ((*rpc-program* ,program))
     ,@body))
(defmacro with-rpc-version ((version) &body body)
  `(let ((*rpc-version* ,version))
     ,@body))
	 

(with-rpc-program (1)
  (with-rpc-version (2)
    (defrpc ping-pong-null (:void :void) 0)
    (defrpc ping-poing-ping-back (:void :int) 1))
  (with-rpc-version (1)
    (defrpc ping-pong-null (:void :void) 0)))



