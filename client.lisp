
(in-package #:frpc)

(defun write-request (stream msg arg-type arg)
  (write-xtype 'rpc-msg stream msg)
  (write-xtype arg-type stream arg)
  nil)

(defun read-response (stream res-type)
  (let ((msg (read-xtype res-type stream)))
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
    (values msg
	    (read-xtype res-type stream))))
  

(defun call-rpc (host arg-type arg result-type 
		 &key (port 111) (program 0) (version 0) 
		   auth verf (request-id 0) (proc 0))
				     
  (let ((socket 
	 (usocket:socket-connect 
	  host port 
	  :element-type '(unsigned-byte 8))))
    (unwind-protect 
	 (let ((stream (usocket:socket-stream socket)))
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
	   (read-response stream result-type))
      (usocket:socket-close socket))))


(defmacro defrpc (name (arg-type result-type &key (program '*rpc-program*) (version '*rpc-version*)) proc)
  (alexandria:with-gensyms (gprogram gversion gproc)
    `(let ((,gprogram ,program)
	   (,gversion ,version)
	   (,gproc ,proc))
       (defun ,name (host arg &key (port 111) auth verf (request-id 0))
	 (call-rpc host ',arg-type arg ',result-type
		   :port port
		   :program ,gprogram
		   :version ,gversion
		   :auth auth
		   :verf verf
		   :request-id request-id
		   :proc ,gproc))
       (%defhandler ,gprogram ,gversion ,gproc ',arg-type ',result-type nil))))





