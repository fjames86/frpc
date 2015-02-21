;;;; Copyright (c) Frank James 2015 <frank.a.james@gmail.com>
;;;; This code is licensed under the MIT license.


(in-package #:frpc)

;; ------------- server handlers ---------------------

;; stores an assoc list for each program id
;; each program id stores an alist of version ids
;; each version id stores an aslist of handlers
(defparameter *handlers* nil)

(defun %defhandler (program version proc arg-type res-type handler)
  (let ((p (assoc program *handlers*)))
    (if p
	(let ((v (assoc version (cdr p))))
	  (if v
	      (let ((c (assoc proc (cdr v))))
		(if c
		    (progn
		      (setf (cdr c) (list arg-type res-type handler))
		      (return-from %defhandler))
		    (push (cons proc (list arg-type res-type handler)) (cdr v))))
	      (push (cons version (list (cons proc (list arg-type res-type handler))))
		    (cdr p))))
	(push (cons program
		    (list (cons version
				(list (cons proc (list arg-type res-type handler))))))
	      *handlers*)))
  nil)

(defun find-handler (program &optional version proc)
  (let ((p (assoc program *handlers*)))
    (if (and p version)
	(let ((v (assoc version (cdr p))))
	  (if (and v proc)
	      (cdr (assoc proc (cdr v)))
	      (cdr v)))
	(cdr p))))
				     
(defmacro defhandler (name (var proc) &body body)
  "Define a server handler for the program specified. You MUST have defined the RPC signature with a
previous call to DEFRPC. This is needed so the system knows the argument/result types." 
  (alexandria:with-gensyms (gprogram gversion gproc gh gha garg-type gres-type)
    `(let* ((,gprogram ,*rpc-program*)
	    (,gversion ,*rpc-version*)
	    (,gproc ,proc)
	    (,gh (find-handler ,gprogram ,gversion ,gproc)))
       (unless ,gh
	 (error "RPC ~A:~A:~A not yet declared. DEFRPC first!" 
		,gprogram ,gversion ,gproc))
       (destructuring-bind (,garg-type ,gres-type ,gha) ,gh
	 (declare (ignore ,gha))
	 (defun ,name (,var)
	   ,@body)
	 (%defhandler ,gprogram ,gversion ,gproc 
		      ,garg-type ,gres-type 
		      (function ,name))))))

;; ------------------------- handle a request from the stream --------------

(defun read-fragmented-message (stream)
  "Read a sequence of message fragements until the terminal bit is set. Returns a sequence containing 
the resulting bytes."
  (flexi-streams:with-output-to-sequence (output)
    (do ((done nil))
	(done)
      (let ((length (read-uint32 stream)))
	;; when the terminating bit is set we are done
	(unless (zerop (logand length #x80000000))
	  (setf done t
		length (logand length (lognot #x80000000))))
	(let ((buff (nibbles:make-octet-vector length)))
	  (read-sequence buff stream)
	  (write-sequence buff output))))))

(defun handle-request (stream &optional output allowed-programs)
  "Read an RPC request from the STREAM and write the output to the OUTPUT stream (or STREAM if not provided)."
;;  (info "Handling request")
  (handler-case 
      (let ((msg (%read-rpc-msg stream)))
	(info "Recieved message ID ~A" (rpc-msg-xid msg))
	;; validate the message
	(cond
	  ((not (eq (xunion-tag (rpc-msg-body msg)) :call))
	   ;; not a call
	   (info "Bad request: not a call")
	   (%write-rpc-msg (or output stream)
			   (make-rpc-response :accept :garbage-args
					      :id (rpc-msg-xid msg))))
	  ((not (= (call-body-rpcvers (xunion-val (rpc-msg-body msg))) 2))
	   ;; rpc version != 2
	   (info "RPC version not = 2")
	   (%write-rpc-msg (or output stream)
			   (make-rpc-response :reject :rpc-mismatch
					      :id (rpc-msg-xid msg))))
	  (t 
	   (let* ((call (xunion-val (rpc-msg-body msg)))
		  (h (find-handler (call-body-prog call)
				   (call-body-vers call)
				   (call-body-proc call))))
	     (cond
	       ((and allowed-programs 
		     (not (member (call-body-prog call) 
				  allowed-programs)))
		;; program not in the list of permissible programs
		(info "Program ~A not in program list" (call-body-prog call))
		(%write-rpc-msg (or output stream)
				(make-rpc-response :accept :prog-mismatch
						   :id (rpc-msg-xid msg))))
	       ((not h)
		;; no handler registered
		(info "No handler registered")
		(%write-rpc-msg (or output stream)
				(make-rpc-response :accept :prog-mismatch
						   :id (rpc-msg-xid msg))))
	       (t 
		(destructuring-bind (arg-type res-type handler) h
		  (handler-case 
		      (let ((arg (read-xtype arg-type stream)))
			(info "Passing arg to handler")
			(let ((res (funcall handler arg)))
			  (info "Call successful")
			  (%write-rpc-msg (or output stream)
					  (make-rpc-response :accept :success
							     :id (rpc-msg-xid msg)))
			  (write-xtype res-type (or output stream) res)))
		    (undefined-function (e)
		      ;; no such function -- probably means we didn't register a handler
		      (info "No handler: ~S" e)
		      (%write-rpc-msg (or output stream)
				      (make-rpc-response :accept :proc-unavail
							 :id (rpc-msg-xid msg))))
		    (error (e)
		      ;; FIXME: should we just terminate the connection at this point?
		      (info "Error handling: ~S" e) 
		      (%write-rpc-msg (or output stream)
				      (make-rpc-response :accept :garbage-args
							 :id (rpc-msg-xid msg))))))))))))
    (end-of-file (e)
      ;; unexpected end of file -- probably means the connection was closed 
      ;; rethrow the error so the connection handler knows the connection has been closed
      (error e))
    (error (e)
      (info "Error reading msg: ~A" e)
      (%write-rpc-msg (or output stream)
		      (make-rpc-response :accept :garbage-args))))
  (info "Flushing output")
  (force-output (or output stream))
  (info "Finished request"))

;; ------------- rpc server ----------

(defstruct (rpc-server (:constructor %make-rpc-server))
  thread programs exiting)

(defun run-rpc-server (server port)
  (let ((socket (usocket:socket-listen usocket:*wildcard-host* port 
				       :element-type '(unsigned-byte 8))))
    (unwind-protect 
	 (flet ((maybe-accept ()
		  ;; wait for a connection and accept, or timeout
		  (when (usocket:wait-for-input socket :timeout 1 :ready-only t)
		    (usocket:socket-accept socket))))
	   (do ((conn (maybe-accept) (maybe-accept)))
	       ((rpc-server-exiting server))	   
	     (when conn
	       ;; a connection has been accepted -- process it 
	       (info "Accepted connection from ~A:~A" (usocket:get-peer-address conn) (usocket:get-peer-port conn))
	       (handler-case 
		   (loop 
		      (let ((stream (usocket:socket-stream conn)))
			(info "reading request")
			(flexi-streams:with-input-from-sequence (input (read-fragmented-message stream))
			  (info "successfully read request")
			  (let ((buff (flexi-streams:with-output-to-sequence (output)
					(handle-request input
							output
							(rpc-server-programs server)))))
			    (info "writing response")
			    ;; write the fragment header (with terminating bit set)
			    (write-uint32 stream (logior #x80000000 (length buff)))
			    ;; write the buffer itself
			    (write-sequence buff stream)
			    ;; flush output
			    (force-output stream)))))
		 (end-of-file (e)
		   (declare (ignore e))
		   (info "Connection closed"))
		 (error (e)
		   (info "Error: ~A" e)
		   nil))
	       (usocket:socket-close conn))))
      (usocket:socket-close socket))))
    
(defun make-rpc-server (&optional programs)
  (%make-rpc-server :programs programs))



(defgeneric start-rpc-server (server &key port))

(defmethod start-rpc-server ((server rpc-server) &key (port *rpc-port*))
  (setf (rpc-server-thread server)
	(bt:make-thread (lambda ()
			  (run-rpc-server server port))
			:name (format nil "rpc-server-thread port ~A" port)))
  server)

(defgeneric stop-rpc-server (server))

(defmethod stop-rpc-server ((server rpc-server))
  (setf (rpc-server-exiting server) t)
  (bt:join-thread (rpc-server-thread server))
  nil)

;; ---------------------

;; for testing/experimenting purposes

(defmacro with-local-stream (&rest type-forms)
  "Write (ARG-TYPE RES-TYPE) forms to a local stream read the results back. This 
can be useful to test handler functions in isolation."
  `(flexi-streams:with-input-from-sequence 
       (input (flexi-streams:with-output-to-sequence (output)
		,@(mapcar (lambda (type-form)
			    `(write-xtype ',(first type-form) output ,(second type-form)))
			  type-forms)))
     (list ,@(mapcar (lambda (type-form)
		       `(read-xtype ',(first type-form) input))
		     type-forms))))

(defmacro with-local-server ((arg-type res-type &key (program 0) (version 0) (proc 0)) &body body)
  "Emulates a socket server using local streams."
  `(flexi-streams:with-input-from-sequence 
       (input (flexi-streams:with-output-to-sequence (output)
		(write-request output
			       (make-rpc-request ,program ,proc :version ,version) 
			       ',arg-type 
			       (progn ,@body))))
     (flexi-streams:with-input-from-sequence 
	 (v (flexi-streams:with-output-to-sequence (output)
	      (handle-request input output)))
       (read-response v ',res-type))))


;; e.g. 
;; (with-local-server (:string :string :program 1 :proc 3) "frank")

