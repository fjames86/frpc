;;;; Copyright (c) Frank James 2015 <frank.a.james@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package #:frpc)

(defun write-request (stream msg arg-type arg)
  (%write-rpc-msg stream msg)
  (write-xtype arg-type stream arg)
  (force-output stream)
  nil)

(defun read-response (stream res-type)
  (let ((msg (%read-rpc-msg stream)))
    (unless (eq (xunion-tag (rpc-msg-body msg)) :reply)
      (error "Expected REPLY recieved ~S" (xunion-tag (rpc-msg-body msg))))
    ;; validate the message. if there was rejected or a prog mismatch then raise
    ;; the condition
    (let ((body (xunion-val (rpc-msg-body msg))))
      (cond
	((eq (xunion-tag body) :rpc-rejected)
	 (error 'rpc-error :description "Message rejected"))
	(t 
	 (case (xunion-tag (accepted-reply-reply-data (xunion-val body)))
	   (:prog-mismatch
	    (let ((a (xunion-val(accepted-reply-reply-data (xunion-val body)))))
	      (error 'rpc-mismatch-error 
		     :low (cdr (assoc 'low a))
		     :high (cdr (assoc 'high a))
		     :description "Program mismatch")))
	   (:success ;; do nothing 
	    nil)
	   (otherwise
	    (error 'rpc-accept-error 
		   :stat (xunion-tag (accepted-reply-reply-data (xunion-val body)))
		   :description "RPC Failed"))))))
    ;; only got to here (to read the response) if the message was successful
    ;; discard the response msg since we don't need it any more 
    (values msg
	    (read-xtype res-type stream))))

;; ---------------------------------------

(defparameter *rpc-host* "localhost")
(defparameter *rpc-port* 111)

(defun rpc-connect (host &optional port)
  "Establish a connection to the rpc server."
  (usocket:socket-connect host (or port *rpc-port*)
			  :element-type '(unsigned-byte 8)))

(defun rpc-close (conn)
  "Close the connection to the server."
  (usocket:socket-close conn))
	    
(defmacro with-rpc-connection ((var host &optional port) &body body)
  "Execute the body in the context of a connection."
  `(let ((,var (rpc-connect ,host ,port)))
     (unwind-protect (progn ,@body)
       (rpc-close ,var))))

(defun call-rpc-server (connection arg-type arg result-type
			&key (program 0) (version 0) (proc 0)
			  auth verf (request-id 0))
  "Send a request to the RPC server, await a response."
  (let ((stream (usocket:socket-stream connection)))
    ;; write the request message
    (write-request stream 
		   (make-rpc-request program proc 
				     :version version
				     :auth auth
				     :verf verf
				     :id request-id)
		   arg-type
		   arg)
    ;; read the response (throws error if failed)
    (nth-value 1 (read-response stream result-type))))

(defun call-rpc (host arg-type arg result-type 
		 &key (port *rpc-port*) (program 0) (version 0) (proc 0) 
		   auth verf (request-id 0))
  "Establish a connection and execute an RPC to a remote machine."
  (with-rpc-connection (conn host port)
    (call-rpc-server conn arg-type arg result-type 
		     :request-id request-id
		     :program program
		     :version version
		     :proc proc
		     :auth auth
		     :verf verf)))

(defmacro defrpc (name proc arg-type result-type)
  "Declare an RPC interface and define a calling function."
  (alexandria:with-gensyms (gprogram gversion gproc greader gwriter)
    `(let ((,gprogram ,*rpc-program*)
	   (,gversion ,*rpc-version*)
	   (,gproc ,proc))
       
       ;; define a function to call it
       (defun ,name (host arg &key (port ,*rpc-port*) auth verf (request-id 0))
	 (with-writer (,gwriter ,arg-type)
	   (with-reader (,greader ,result-type)
	     (call-rpc host #',gwriter arg #',greader
		       :port port
		       :program ,gprogram
		       :version ,gversion
		       :proc ,gproc
		       :auth auth
		       :verf verf
		       :request-id request-id))))

       ;; define a server handler
       (with-reader (,greader ,arg-type)
	 (with-writer (,gwriter ,result-type)
	   (%defhandler ,gprogram ,gversion ,gproc 
			(function ,greader) 
			(function ,gwriter) 
			nil))))))



;; -------- udp -----------------


(defun send-rpc-request-udp (send-socket arg-type arg
			     &key (program 0) (version 0) (proc 0)
			       auth verf (request-id 0))
  (let ((buffer (flexi-streams:with-output-to-sequence (v)
		  (write-request v 
				 (make-rpc-request program proc
						   :version version
						   :auth auth
						   :verf verf
						   :id request-id)
				 arg-type
				 arg))))
    (usocket:socket-send send-socket buffer (length buffer))))

