;;;; Copyright (c) Frank James 2015 <frank.a.james@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package #:frpc)

(defun write-request (stream msg arg-type arg)
  "Write an RPC request to the stream."
  (%write-rpc-msg stream msg)
  (write-xtype arg-type stream arg)
  (force-output stream)
  nil)

(defun read-response (stream res-type)
  "Read an RPC response from the stream. This is for TCP response parsing only."
  (let ((msg (%read-rpc-msg stream)))
    (unless (eq (xunion-tag (rpc-msg-body msg)) :reply)
      (error "Expected REPLY recieved ~S" (xunion-tag (rpc-msg-body msg))))
    ;; validate the message. if there was rejected or a prog mismatch then raise
    ;; the condition
    (let ((body (xunion-val (rpc-msg-body msg))))
      (cond
	((eq (xunion-tag body) :msg-denied)
	 (let ((denied (xunion-val body)))
	   (ecase (xunion-tag denied)
	     (:auth-error (error 'rpc-error :description (format nil "Auth error: ~A" (xunion-val denied))))
	     (:rpc-mismatch (error 'rpc-error :description "RPC mismatch")))))
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
			  auth verf request-id)
  "Send a request to the RPC server and await a response."
  (let ((stream (usocket:socket-stream connection)))
    ;; write the request message
    ;; When using TCP (as we are here) you must prepend the message with 
    ;; the length of the message as a Big-endian uint32 (see section 10 of the rfc)
    ;; therefore we need to write the request through to a local stream first 
    ;; so we know the total length. the final fragment needs to set the high bit.
    ;; We only ever send a single fragment.
    (let ((buffer (flexi-streams:with-output-to-sequence (output)
		    (write-request output 
				   (make-rpc-request program proc 
						     :version version
						     :auth auth
						     :verf verf
						     :id request-id)
				   arg-type
				   arg))))
      ;; write as a single fragment to the socket
      (write-uint32 stream (logior #x80000000 (length buffer)))
      (write-sequence buffer stream)
      (force-output stream))
    ;; need to read the response back to a local stream, to account for the fragmenting
    (flexi-streams:with-input-from-sequence (input (read-fragmented-message stream))
      ;; read the response (throws error if failed)
      (nth-value 1 (read-response input result-type)))))

(defun call-rpc (host arg-type arg result-type 
		 &key (port *rpc-port*) (program 0) (version 0) (proc 0) 
		   auth verf request-id protocol)
  "Establish a connection and execute an RPC to a remote machine. By default a TCP connection
is established and this function will block until a reply is received.

If PROTOCOL is :UDP, the RPC will be sent via UDP and this function returns immediately.
So it will not wait for a response. See the WAIT-FOR-REPLY and GET-REPLY functions to get reply messages."
  (ecase protocol
    ((:tcp nil)
     (with-rpc-connection (conn host port)
       (call-rpc-server conn arg-type arg result-type 
			:request-id request-id
			:program program
			:version version
			:proc proc
			:auth auth
			:verf verf)))
    (:udp
     ;; we have a little problem here... 
     ;; we can send the udp request, but we don't know whether to 
     ;; wait for the reply or not. in any case, we'd need access
     ;; to the local udp rpc-server instance in order to get the reply.
     ;; therefore any code which wishes to get the reply 
     ;; needs to be external to this function. that's a little bit annoying 
     ;; for users though, because it means the result-type is ignored here
     ;; and they need to provide it elsewhere. I don't see a simple alternative
     ;; yet.
     (let ((socket (usocket:socket-connect host port
					   :protocol :datagram
					   :element-type '(unsigned-byte 8))))
       (let ((buffer 
	      (flexi-streams:with-output-to-sequence (stream)
		(write-request stream 
			       (make-rpc-request program proc 
						 :version version
						 :auth auth
						 :verf verf
						 :id request-id)
			       arg-type
			       arg))))
	 (usocket:socket-send socket buffer (length buffer)))
       (usocket:socket-close socket)))))
			    
(defmacro defrpc (name proc arg-type result-type)
  "Declare an RPC interface and define a calling function. This MUST be defined before a partner DEFHANDLER form.

NAME should be a symbol naming the client function to be defined.

PROC should be an integer or constant form.

ARG-TYPE and RESULT-TYPE should be XDR type specification forms."
  (alexandria:with-gensyms (gprogram gversion gproc greader gwriter)
    `(let ((,gprogram ,*rpc-program*)
	   (,gversion ,*rpc-version*)
	   (,gproc ,proc))
       
       ;; define a function to call it
       (defun ,name (host arg &key (port ,*rpc-port*) auth verf request-id protocol)
	 (with-writer (,gwriter ,arg-type)
	   (with-reader (,greader ,result-type)
	     (call-rpc host #',gwriter arg #',greader
		       :port port
		       :program ,gprogram
		       :version ,gversion
		       :proc ,gproc
		       :auth auth
		       :verf verf
		       :request-id request-id
		       :protocol protocol))))

       ;; define a server handler
       (with-reader (,greader ,arg-type)
	 (with-writer (,gwriter ,result-type)
	   (%defhandler ,gprogram ,gversion ,gproc 
			(function ,greader) 
			(function ,gwriter) 
			nil))))))

