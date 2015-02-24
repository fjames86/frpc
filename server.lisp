;;;; Copyright (c) Frank James 2015 <frank.a.james@gmail.com>
;;;; This code is licensed under the MIT license.


(in-package #:frpc)

;; ------------- server handlers ---------------------

;; stores an assoc list for each program id
;; each program id stores an alist of version ids
;; each version id stores an aslist of handler lists
;; a handler list is (ARG-READER RES-WRITER HANDLER)
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
  "Look up the handler(s) for the given PROGRAM, VERSION and PROC IDs."
  (let ((p (assoc program *handlers*)))
    (if (and p version)
	(let ((v (assoc version (cdr p))))
	  (if (and v proc)
	      (cdr (assoc proc (cdr v)))
	      (cdr v)))
	(cdr p))))
				     
(defmacro defhandler (name (var proc) &body body)
  "Define a server handler for the procedure specified. You MUST have defined the RPC signature with a
previous call to DEFRPC. This is needed so the system knows the argument/result types.

A function named NAME will be defined and will accept a single argument named VAR, which will be bound
to a value of type specified by the ARG-TYPE in the partner DEFRPC form. The function should return 
a value of type specified in the RESULT-TYPE of the DEFRPC.

The BODY should NEVER signal any errors, any incorrect behaviour (such as bad arg values etc) 
should be returned to the sender in a way specified by the RPC program itself, typically 
with enum status return values." 
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
the bytes read."
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
  "Read an RPC request from the STREAM and write the output to the OUTPUT stream (or STREAM if not provided). This is for
TCP requests only."
  (handler-case 
      (let ((msg (%read-rpc-msg stream)))
	(log:debug "Recieved message ID ~A" (rpc-msg-xid msg))
	;; validate the message
	(cond
	  ((not (eq (xunion-tag (rpc-msg-body msg)) :call))
	   ;; not a call
	   (log:info "Bad request: not a call")
	   (%write-rpc-msg (or output stream)
			   (make-rpc-response :accept :garbage-args
					      :id (rpc-msg-xid msg))))
	  ((not (= (call-body-rpcvers (xunion-val (rpc-msg-body msg))) 2))
	   ;; rpc version != 2
	   (log:info "RPC version not = 2")
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
		(log:info "Program ~A not in program list" (call-body-prog call))
		(%write-rpc-msg (or output stream)
				(make-rpc-response :accept :prog-mismatch
						   :id (rpc-msg-xid msg))))
	       ((or (not h) (null (third h)))
		;; no handler registered
		(log:warn "No handler registered")
		(%write-rpc-msg (or output stream)
				(make-rpc-response :accept :proc-unavail
						   :id (rpc-msg-xid msg))))
	       (t 
		(destructuring-bind (arg-type res-type handler) h
		  (handler-case 
		      (let ((arg (read-xtype arg-type stream)))
			(log:debug "Passing arg to handler")
			(let ((res (funcall handler arg)))
			  (log:debug "Call successful")
			  (%write-rpc-msg (or output stream)
					  (make-rpc-response :accept :success
							     :id (rpc-msg-xid msg)))
			  (write-xtype res-type (or output stream) res)))
		    (undefined-function (e)
		      ;; no such function -- probably means we didn't register a handler
		      (log:warn "No handler: ~S" e)
		      (%write-rpc-msg (or output stream)
				      (make-rpc-response :accept :proc-unavail
							 :id (rpc-msg-xid msg))))
		    (error (e)
		      ;; FIXME: should we just terminate the connection at this point?
		      (log:error "Error handling: ~S" e) 
		      (%write-rpc-msg (or output stream)
				      (make-rpc-response :accept :garbage-args
							 :id (rpc-msg-xid msg))))))))))))
    (end-of-file (e)
      ;; unexpected end of file -- probably means the connection was closed 
      ;; rethrow the error so the connection handler knows the connection has been closed
      (error e))
    (error (e)
      (log:error "Error reading msg: ~A" e)
      (%write-rpc-msg (or output stream)
		      (make-rpc-response :accept :garbage-args))))
  (log:debug "Flushing output")
  (force-output (or output stream))
  (log:debug "Finished request"))

;; ------------- rpc server ----------


(defun accept-rpc-connection (server socket)
  "Accept a TCP connection and process the request(s). Will keep processing requests from the connection
until the client terminates or some other error occurs."
  ;; a connection has been accepted -- process it 
  (let ((conn (usocket:socket-accept socket)))
    (log:info "Accepted connection from ~A:~A" (usocket:get-peer-address conn) (usocket:get-peer-port conn))
    (handler-case 
	(loop 
	   (let ((stream (usocket:socket-stream conn)))
	     (log:debug "reading request")
	     (flexi-streams:with-input-from-sequence (input (read-fragmented-message stream))
	       (log:debug "successfully read request")
	       (let ((buff (flexi-streams:with-output-to-sequence (output)
			     (handle-request input
					     output
					     (rpc-server-programs server)))))
		 (log:debug "writing response")
		 ;; write the fragment header (with terminating bit set)
		 (write-uint32 stream (logior #x80000000 (length buff)))
		 ;; write the buffer itself
		 (write-sequence buff stream)
		 ;; flush output
		 (force-output stream)))))
      (end-of-file (e)
	(declare (ignore e))
	(log:debug "Connection closed"))
      (error (e)
	(log:error "Error processing: ~A" e)
	nil))
    (usocket:socket-close conn)))

;; -------------------- udp rpc server -------------

(defun enqueue-reply (server id reply)
  "Enqueue a reply and signal to any waiting threads that there is a new 
message to recieve. The reply should be an array of octets."
  (bt:with-lock-held ((rpc-server-lock server))
    (push (cons id reply) 
	  (rpc-server-replies server))
    (bt:condition-notify (rpc-server-condv server))))

(defun get-reply (server id dispose)
  "Finds the message with ID, but leaves it in the queue. Removes from the queue if DISPOSE is T.
You MUST hold the server lock when calling this function." 
  (cond
    (id 
     (let ((reply (assoc id (rpc-server-replies server) :test #'=)))
       (cond
	 ((not reply) (values nil nil))
	 (dispose (setf (rpc-server-replies server)
			(remove reply (rpc-server-replies server)))
		  (values (cdr reply) t))
	 (t (values (cdr reply) t)))))
    (dispose
     (let ((reply (pop (rpc-server-replies server))))
       (if reply
	   (values (cdr reply) t)
	   (values nil nil))))
    (t 
     (let ((reply (car (rpc-server-replies server))))
       (if reply
	   (values (cdr reply) t)
	   (values nil nil))))))

(defun wait-for-reply (server &optional id)
  "Blocks until a reply is recieved. If ID is supplied, will block until a message for a 
matching ID is resived, otherwise returns the first reply."
  (do ((res nil)
       (done nil))
      (done res)
    (bt:with-lock-held ((rpc-server-lock server))
      (multiple-value-bind (reply found) (get-reply server id t)
	(if found 
	    (setf res reply
		  done t)
	    (bt:condition-wait (rpc-server-condv server) 
			       (rpc-server-lock server)))))))

;; FIXME: there must be a better way of doing this
(defun %read-until-eof (stream)
  "Read the rest of the stream."
  (flexi-streams:with-output-to-sequence (output)
    (do ((done nil))
	(done)
      (let ((byte (read-byte stream nil nil)))
	(if byte
	    (write-byte byte output)
	    (setf done t))))))

(defun process-udp-request (server input-stream &key program version proc id)
  "Returns a packed buffer containing the response to send back to the caller."
  (let ((h (find-handler program version proc)))
    (cond
      ((and (rpc-server-programs server)
	    (not (member program (rpc-server-programs server))))
       ;; not in allowed programs list
       (log:warn "Program ~A not in program list" program)
       (pack #'%write-rpc-msg 
	     (make-rpc-response :accept :prog-mismatch
			      :id id)))
      ((or (not h) (null (third h)))
       ;; no handler
       (log:warn "No handler registered for ~A:~A:~A" program version proc)
       (pack #'%write-rpc-msg
	     (make-rpc-response :accept :proc-unavail
				:id id)))
      (t
       (destructuring-bind (reader writer handler) h
	 ;; read the argument
	 (let ((arg (read-xtype reader input-stream)))
	   ;; run the handler
	   (let ((res (funcall handler arg)))
	     ;; package the reply and send
	     (flexi-streams:with-output-to-sequence (output)
	       (%write-rpc-msg output
			       (make-rpc-response :accept :success
						  :id id))
	       (write-xtype writer output res)))))))))

(defun handle-udp-request (server buffer &key remote-host reply-port)
  (log:debug "Buffer ~S" buffer)
  (flexi-streams:with-input-from-sequence (input buffer)
    (let ((msg (%read-rpc-msg input)))
      (ecase (xunion-tag (rpc-msg-body msg))
	(:reply
	 ;; is a reply -- read the rest of the stream and enqueue 
	 ;; to the replies list
	 (log:info "Recieved reply from ~A:~A" remote-host reply-port)
	 (enqueue-reply server (rpc-msg-xid msg) (%read-until-eof input)))
	(:call 
	 ;; is a call -- lookup the proc handler and send a reply 
	 (log:info "Recived call from ~A:~A" remote-host reply-port)
	 (let ((call (xunion-val (rpc-msg-body msg))))
	   (let ((return-buffer (process-udp-request server input
						 :program (call-body-prog call) 
						 :version (call-body-vers call) 
						 :proc (call-body-proc call)
						 :id (rpc-msg-xid msg))))
	     (let ((socket (usocket:socket-connect remote-host reply-port
						   :protocol :datagram
						   :element-type '(unsigned-byte 8))))
	       (log:debug "Sending reply")
	       (usocket:socket-send socket return-buffer (length return-buffer))
	       (usocket:socket-close socket)))))))))

(defun accept-udp-rpc-request (server socket reply-port)
  (multiple-value-bind (buffer length remote-host remote-port) (usocket:socket-receive socket nil 65507)	    
    (when buffer 
      (log:info "Recieved msg from ~A:~A" remote-host remote-port)
      (handler-case 
	  (handle-udp-request server (subseq buffer 0 length)
			      :remote-host remote-host
			      :reply-port reply-port)
	(error (e)
	  (log:error "Error handling: ~S" e))))))



;; --------------------------------------

(defstruct (rpc-server (:constructor %make-rpc-server))
  thread 
  programs 
  exiting
  (lock (bt:make-lock))
  (condv (bt:make-condition-variable))
  replies)

(defun make-rpc-server (&optional programs)
  "Make an RPC server instance. PROGRAMS should be a list of program numbers 
to be accepted by the server, all RPC requests for programs not in this list will
be rejected. If not supplied all program requests are accepted."
  (%make-rpc-server :programs programs))

(defun run-rpc-server (server tcp-ports udp-ports)
  "Run the RPC server until the SERVER-EXITING flag is set. Will open TCP sockets listening
on the TCP-PORTS list and UDP sockets listening on the UDP-PORTS list."
  (let ((tcp-sockets (mapcar (lambda (port)
			       (usocket:socket-listen usocket:*wildcard-host* port
						      :reuse-address t
						      :element-type '(unsigned-byte 8)))
			     tcp-ports))
	(udp-sockets (mapcar (lambda (port)
			       (usocket:socket-connect nil nil 
						       :protocol :datagram
						       :element-type '(unsigned-byte 8)
						       :local-port port))
			     udp-ports)))
    (flet ((find-udp-port (udp-sock)
	     "Determine the port we should send UDP replies to."
	     (do ((uports udp-ports (cdr uports))
		  (usocks udp-sockets (cdr usocks)))
		 ((null uports))
	       (when (eq (car usocks) udp-sock)
		 (return-from find-udp-port (car uports))))))
      (unwind-protect 
	   (do ()
	       ((rpc-server-exiting server))
	     (let ((ready (usocket:wait-for-input (append tcp-sockets udp-sockets) :ready-only t :timeout 1)))
	       (dolist (socket ready)
		 (etypecase socket
		   (usocket:stream-server-usocket
		    ;; a tcp socket to accept
		    (accept-rpc-connection server socket))
		   (usocket:datagram-usocket
		    ;; a udp socket is ready to read from
		    ;; FIXME: we always reply back on the same port we're listening on
		    ;; this probably isn't the right thing to do
		    ;; but replying back to the port we recieved the request from 
		    ;; doesn't seem right either.
		    (accept-udp-rpc-request server socket (find-udp-port socket)))))))
	;; close the server sockets
	(dolist (tcp tcp-sockets)
	  (usocket:socket-close tcp))
	(dolist (udp udp-sockets)
	  (usocket:socket-close udp))))))

(defun start-rpc-server (server &key tcp-ports udp-ports (add-port-mappings t))
  "Start the RPC server in a new thread. The server will listen for requests
on the TCP and UDP ports specified."
  ;; add all the port mappings
  (when add-port-mappings
    (port-mapper:add-all-mappings tcp-ports udp-ports))
  (setf (rpc-server-thread server)
	(bt:make-thread (lambda ()
			  (run-rpc-server server tcp-ports udp-ports))
			:name "rpc-server-thread"))
  server)

(defun stop-rpc-server (server)
  "Stop the RPC server and wait until its thread exits."
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

;; --------------------------------------------
