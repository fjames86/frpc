;;;; Copyright (c) Frank James 2015 <frank.a.james@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package #:frpc)

(defun write-rpc-response (stream &key accept reject verf (id 0) (high 0) (low 0) auth-stat)
  (%write-rpc-msg stream
		  (make-rpc-response :accept accept
				     :reject reject
				     :verf verf
				     :id id
				     :high high
				     :low low
				     :auth-stat auth-stat)))

;; server handlers might need to know who called them
;; bind these special variables to the remote host, port and protocol
(defvar *rpc-remote-host* nil)
(defvar *rpc-remote-port* nil)
(defvar *rpc-remote-protocol* nil)
(defvar *rpc-remote-auth* nil)

(defmacro with-caller-binded ((host port protocol auth) &body body)
  `(let ((*rpc-remote-host* ,host)
	 (*rpc-remote-port* ,port)
	 (*rpc-remote-protocol* ,protocol)
	 (*rpc-remote-auth* ,auth))
     ,@body))


;; stores an assoc list for each program id
;; each program id stores an alist of version ids
;; each version id stores an alist of handler lists
;; a handler list is (ARG-READER RES-WRITER HANDLER)
(defvar *handlers* nil)

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

(defun find-handler (&optional program version proc)
  "Look up the handler(s) for the given PROGRAM, VERSION and PROC IDs."
  ;; if no program supplied return a list of all programs currently defined
  (unless program
    (return-from find-handler (mapcar #'car *handlers*)))
  ;; otherwise find the specified program/version/proc 
  (let ((p (assoc program *handlers*)))
    (if (and p version)
	(let ((v (assoc version (cdr p))))
	  (if (and v proc)
	      (cdr (assoc proc (cdr v)))
	      (cdr v)))
	(cdr p))))

;; ---------------------------------------------------------

;; stores the information about the rpc server, its thread, exit flag etc
(defstruct (rpc-server (:constructor %make-rpc-server))
  thread 
  udp-ports
  tcp-ports
  (timeout 60)
  programs 
  exiting)

(defun make-rpc-server (&key udp-ports tcp-ports programs (timeout 60))
  "Make an RPC server instance. 

PROGRAMS should be a list of program numbers to be accepted by the server, 
all RPC requests for programs not in this list will be rejected. 
If not supplied all program requests are accepted. 

UDP-PORTS and TCP-PORTS should each be a list of integers, specifying the ports
to listen on. If USOCKET:*WILDCARD-PORT* is supplied, a random unused port will be selected.
These lists will be replaced with the ports actually used once the server starts.

TIMEOUT specifies the duration (in seconds) that a TCP connection should remain open.
"
  (%make-rpc-server :programs programs
		    :udp-ports udp-ports
		    :tcp-ports tcp-ports
		    :timeout timeout))

;; describes a TCP connection
(defstruct rpc-connection 
  conn time)

(defun purge-connection-list (connections now timeout)
  "Returns a list of connections with old ones closed and removed."
  (mapcan (lambda (c)
	    (cond
	      ((> (- now (rpc-connection-time c)) timeout)
	       ;; the connection is old, close it 
	       (frpc-log :info "Purging connection to ~A" 
			 (usocket:get-peer-address (rpc-connection-conn c)))
	       (handler-case (usocket:socket-close (rpc-connection-conn c))
		 (error (e)
		   ;; this can happen if it's already been closed by the remote peer
		   (frpc-log :info "Couldn't close connection: ~A" e)))
	       nil)
	      (t 
	       (list c))))
	  connections))

;; ---------------------------------------------------------

(defun process-rpc-auth (output-stream auth verf id)
  "Process authentication, returns T is authenticated, nil is no further processing required."
  (let ((flavour (opaque-auth-flavour auth))
	(data (opaque-auth-data auth)))
    (handler-case (authenticate flavour data verf)
      (error (e)
	(frpc-log :trace "authentication failed: ~A" e)
	(write-rpc-response output-stream
			    :reject :auth-error
			    :auth-stat :auth-rejected
			    :id id)
	nil))))

(defun process-rpc-call (input-stream output-stream 
			 &key host port protocol id auth verf program version proc)
  "Process the actual call. read the argument, handle it and write the response."
  (let ((rverf (process-rpc-auth output-stream auth verf id)))
    ;; check the verifier
    (unless rverf 
      (write-rpc-response output-stream
			  :reject :auth-error
			  :auth-stat :auth-rejected)
      (return-from process-rpc-call))
    ;; go ahead and try to invoke the handler
    (let ((h (find-handler program version proc)))
      (unless h
	(write-rpc-response output-stream :accept :proc-unavail :id id)
	(return-from process-rpc-call))
      (destructuring-bind (reader writer handler) h
	(let ((gss-service (when (eq (opaque-auth-flavour auth) :auth-gss)
			     (gss-cred-service (opaque-auth-data auth))))
	      (arg-reader reader)
	      (res-writer writer))

	  ;; when doing GSS integrity we need to modify the reader
	  (case gss-service
	    (:integrity 
	     (let* ((cred (opaque-auth-data auth))
		    (context (find-gss-context (gss-cred-handle cred))))
	       (setf arg-reader (lambda (stream)
				  (read-gss-integ-arg stream 
						      reader 
						      (gss-context-context context)
						      (gss-cred-seqno cred)))
		     res-writer (lambda (stream obj)
				  (write-gss-integ-res stream writer obj 
						       (gss-context-context context)
						       (gss-cred-seqno cred))))))
	    (:privacy 
	     (let* ((cred (opaque-auth-data auth))
		    (context (find-gss-context (gss-cred-handle cred))))
	       (setf arg-reader (lambda (stream)
				  (read-gss-priv-arg stream reader 
						     (gss-context-context context)
						     (gss-cred-seqno cred)))
		     res-writer (lambda (stream obj)
				  (write-gss-priv-res stream writer obj
						      (gss-context-context context)
						      (gss-cred-seqno cred)))))))

	  ;; funcall the handler to get the result 
	  (let ((arg (handler-case (read-xtype reader input-stream)
		       (error (e)
			 (frpc-log :trace "Failed to read argument: ~A" e)
			 (write-rpc-response output-stream 
					     :accept :garbage-args :id id)
			 (return-from process-rpc-call)))))

	    (let ((res (handler-case (with-caller-binded (host port protocol auth) (funcall handler arg))
			 (error (e)
			   (frpc-log :trace "Failed to invoke handler: ~A" e)
			   ;; be silent if the handler errors, this allows us to 
			   ;; provide the "silent" semantics that some APIs require
;;			   (write-rpc-response output-stream :accept :garbage-args :id id)
			   (return-from process-rpc-call)))))

	      (write-rpc-response output-stream :accept :success :id id :verf rverf)
	      (write-xtype writer output-stream res))))))))

(defun process-gss-init-command (input-stream output-stream id)
  "GSS requires special treatment, it can send arguments in place of the nullproc void parameter."
  (let ((token (handler-case (coerce (read-xtype 'gss-init-arg input-stream)
				     '(vector (unsigned-byte 8)))
		 (error (e)
		   (frpc-log :trace "Error parsing GSS token: ~A" e)
		   (write-rpc-response output-stream 
				       :accept :garbge-args
				       :id id)
		   (return-from process-gss-init-command)))))
    (let ((cxt (handler-case (gss-authenticate token)
		 (error () nil))))
      (cond
	(cxt
	 (write-rpc-response output-stream :accept :success :id id)
	 (%write-gss-init-res output-stream
			      (make-gss-init-res :handle (gss-context-handle cxt)
						 :major 0
						 :minor 0
						 :window 0
						 :token nil)))
	(t
	 ;; no context granted... means was invalid token
	 (write-rpc-response output-stream 
			     :reject :auth-error
			     :id id
			     :auth-stat :gss-cred-problem))))))

(defun process-rpc-request (input-stream output-stream &key host port protocol)
  "Process a request from the input stream, writing the response to the output stream."
  (let* ((msg (handler-case (%read-rpc-msg input-stream)
		(error (e)
		  (frpc-log :trace "Failed to read msg: ~A" e)
		  (%write-rpc-msg output-stream
				  (make-rpc-response :accept :garbage-args))
		  (return-from process-rpc-request))))
	 (id (rpc-msg-xid msg)))
    ;; if it's a reply then this is not intended for us
    (when (eq (xunion-tag (rpc-msg-body msg)) :reply)
      (%write-rpc-msg output-stream
		      (make-rpc-response :accept :garbage-args :id id))
      (return-from process-rpc-request))

    (let* ((call (xunion-val (rpc-msg-body msg)))
	   (auth (call-body-auth call)))
      
      ;; if the authenticator is a GSS init (FIXME: or continue) command then we need to do special things
      (when (and (eq (opaque-auth-flavour auth) :auth-gss)
		 (eq (gss-cred-proc (opaque-auth-data auth)) :init))
	(process-gss-init-command input-stream output-stream id)
	(return-from process-rpc-request))

      (handler-case 
	  (process-rpc-call input-stream output-stream
			    :host host :port port :protocol protocol :id id
			    :auth auth :verf (call-body-verf call)
			    :program (call-body-prog call) 
			    :version (call-body-vers call)
			    :proc (call-body-proc call))
	(error (e)
	  (frpc-log :trace "Failed to process: ~A" e)
	  nil)))))

;; -----------------------------------------------------------------

					
;; FIXME: should we put a limit on the maximum number of simultaneous connections?
(defun run-rpc-server (server)
  "Run the RPC server until the SERVER-EXITING flag is set."
  (declare (type rpc-server server))
  (let (tcp-sockets udp-sockets connections)
    (unwind-protect 
	 (progn 
	   ;; collect the sockets this way so that if there is an error thrown (such as can't listen on port)
	   ;; then we can gracefully fail, and close the sockets we have opened
	   ;; if we mapcar to collect them then we leak the sockets we opened before the failure
	   (dolist (port (rpc-server-tcp-ports server))
	     (push (usocket:socket-listen usocket:*wildcard-host* port
					  :reuse-address t
					  :element-type '(unsigned-byte 8))
		   tcp-sockets))
	   (setf (rpc-server-tcp-ports server)
		 (mapcar #'usocket:get-local-port tcp-sockets))
	   (dolist (port (rpc-server-udp-ports server))
	     (push (usocket:socket-connect nil nil 
					   :protocol :datagram
					   :element-type '(unsigned-byte 8)
					   :local-port port)
		   udp-sockets))
	   (setf (rpc-server-udp-ports server)
		 (mapcar #'usocket:get-local-port udp-sockets))

	   ;; the polling-loop
	   (do ((udp-buffer (nibbles:make-octet-vector 65507))
		(prev-time (get-universal-time)))
	       ((rpc-server-exiting server))

	     ;; if any connections are getting old then close them
	     ;; only do the check at most once per second
	     (let ((now (get-universal-time)))
	       (when (> now prev-time)
		 (setf connections (purge-connection-list connections now (rpc-server-timeout server))
		       prev-time now)))
	     
	     ;; poll and timeout each second so that we can check the exiting flag and purge connections
	     (let ((ready (usocket:wait-for-input (append tcp-sockets 
							  udp-sockets
							  (mapcar #'rpc-connection-conn connections))
						  :ready-only t :timeout 1)))
	       (dolist (socket ready)
		 (etypecase socket
		   (usocket:stream-server-usocket
		    ;; a tcp socket to accept
		    (let ((conn (make-rpc-connection :conn
						     (usocket:socket-accept socket)
						     :time 
						     (get-universal-time))))
		      (frpc-log :info "Accepting TCP connection from ~A:~A" 
				 (usocket:get-peer-address (rpc-connection-conn conn))
				 (usocket:get-peer-port (rpc-connection-conn conn)))
		      (push conn connections)))
		   (usocket:datagram-usocket
		    ;; a udp socket is ready to read from
		    (handler-case 
			(multiple-value-bind (%buffer count remote-host remote-port) (usocket:socket-receive socket udp-buffer 65507)
			  (declare (ignore %buffer))
			  (cond
			    ((or (< count 0) (= count #xffffffff))
			     ;; some implementations (e.g. up to SBCL 1.2.10 x64, LispWorks) return error codes 
			     ;; this way rather than signalling an error
			     (frpc-log :info "recvfrom returned -1"))
			    (t 
			     (flexi-streams:with-input-from-sequence (input-stream udp-buffer :end count)
			       (let ((response-buffer 
				      (flexi-streams:with-output-to-sequence (output-stream)
					(process-rpc-request input-stream output-stream
							     :host remote-host 
							     :port remote-port
							     :protocol :udp))))
				 (unless (zerop (length response-buffer))
				   (usocket:socket-send socket 
							response-buffer
							(length response-buffer)
							:host remote-host
							:port remote-port)))))))
		      (error (e)
			;; windows is known to throw an error on receive if there was no-one 
			;; listening on the port the previous UDP packet was sent to.
			(frpc-log :info "error handling udp: ~A" e))))
		   (usocket:stream-usocket 
		    ;; a TCP connection is ready to read 
		    (let ((c (find socket connections :key #'rpc-connection-conn)))
		      (setf (rpc-connection-time c) (get-universal-time)))
		    (handler-case 
			;; FIXME: replace this with a gray stream that wraps the underlying socket stream
			(let ((buffer (read-fragmented-message (usocket:socket-stream socket))))
			  (flexi-streams:with-input-from-sequence (input-stream buffer)
			    (let ((response-buffer 
				   (flexi-streams:with-output-to-sequence (output-stream)
				     (process-rpc-request input-stream output-stream
							  :host (usocket:get-peer-address socket)
							  :port (usocket:get-peer-port socket)
							  :protocol :tcp))))
			      (unless (zerop (length response-buffer))
				(write-uint32 (usocket:socket-stream socket)
					      (logior #x80000000 (length response-buffer)))
				(write-sequence response-buffer (usocket:socket-stream socket))))))
		      (end-of-file ()
			(frpc-log :info "Connection closed by remote host")
			(setf connections (remove socket connections :key #'rpc-connection-conn)))
		      (error (e)
			(frpc-log :info "Error: ~A" e)
			(ignore-errors (usocket:socket-close socket))
			(setf connections (remove socket connections :key #'rpc-connection-conn))))))))))

      ;; close outstanding connections
      (dolist (conn connections)
	(ignore-errors 
	  (usocket:socket-close (rpc-connection-conn conn))))
      ;; close the server sockets
      (dolist (tcp tcp-sockets)
	(usocket:socket-close tcp))
      (dolist (udp udp-sockets)
	(usocket:socket-close udp)))))

(defun start-rpc-server (server)
  "Start the RPC server in a new thread."
  (setf (rpc-server-thread server)
	(bt:make-thread (lambda ()
			  (run-rpc-server server))
			:name "rpc-server-thread"))
  server)

(defun stop-rpc-server (server)
  "Stop the RPC server and wait until its thread to exit."
  (setf (rpc-server-exiting server) t)
  (bt:join-thread (rpc-server-thread server))
  nil)
