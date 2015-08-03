;;;; Copyright (c) Frank James 2015 <frank.a.james@gmail.com>
;;;; This code is licensed under the MIT license.

;;; This file implements the codes to accept and process RPC requests.

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

(defun rpc-auth-principal (&optional (auth *rpc-remote-auth*))
  "Returns a string representing the authenticated caller, or nil if none available."
  (auth-principal-name (opaque-auth-flavour auth)
		       (opaque-auth-data auth)))

(defun get-unix-creds (&optional (auth *rpc-remote-auth*))
  "Get the UNIX credentials from this authenticator. Returns the AUTH-UNIX associated with this authenticator
if it is of type :AUTH-UNIX or :AUTH-SHORT. Returns nil if could not be found."
  (case (opaque-auth-flavour auth)
    (:auth-unix (opaque-auth-data auth))
    (:auth-short 
     (let ((c (find-unix-context (opaque-auth-data auth))))
       (when c (unix-context-unix c))))))

;; ---------------------------------------------------------

;; stores the information about the rpc server, its thread, exit flag etc
(defstruct (rpc-server (:constructor %make-rpc-server))
  thread                   ;; the server thread
  udp-ports                ;; list of UDP listening ports
  tcp-ports                ;; list of TCP listening ports
  (timeout 60)             ;; TCP connection timeout 
  programs                 ;; List of accepting programs 
  exiting                  ;; server thread exiting flag 
;; new slots for refactored server codes
 tcp-sockets               ;; list of TCP listening sockets 
 udp-sockets               ;; list of UDP listening sockets 
 connections)              ;; list of accepted TCP connections 


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
  (%make-rpc-server :programs (mapcar (lambda (p)
					(if (integerp p)
					    p
					    (program-id p)))
				      programs)
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
    (frpc-log :info "~A:~A:~A ~A:~A:~A ~A ~S"
              host port protocol
              program version proc 
              (rpc-auth-principal auth) (opaque-auth-flavour auth))
    ;; go ahead and try to invoke the handler
    (let ((h (find-handler program version proc)))
      (unless h
	(write-rpc-response output-stream :accept :proc-unavail :id id)
	(return-from process-rpc-call))
      (destructuring-bind (reader writer handler) h
	(let ((gss-service (when (eq (opaque-auth-flavour auth) :auth-gss)
			     (gss-cred-service (opaque-auth-data auth)))))
	  (flet ((arg-reader (stream)
		   (frpc-log :trace "Reading arg")
		   (case gss-service
		     (:integrity
		      (let* ((cred (opaque-auth-data auth))
			     (context (find-gss-context (gss-cred-handle cred))))
			(read-gss-integ stream 
					reader 
					(gss-context-context context)
					(gss-cred-seqno cred))))
		     (:privacy
		      (let* ((cred (opaque-auth-data auth))
			     (context (find-gss-context (gss-cred-handle cred))))
			(read-gss-priv stream reader 
				       (gss-context-context context)
				       (gss-cred-seqno cred))))		      
		     (otherwise (read-xtype reader stream))))
		 (res-writer (stream obj)
		   (frpc-log :trace "Writing result")
		   (case gss-service
		     (:integrity
		      (let* ((cred (opaque-auth-data auth))
			     (context (find-gss-context (gss-cred-handle cred))))
			(frpc-log :trace "writing integ response")
			(write-gss-integ stream writer obj 
					 (gss-context-context context)
					 (gss-cred-seqno cred))))
		     (:privacy
		      (let* ((cred (opaque-auth-data auth))
			     (context (find-gss-context (gss-cred-handle cred))))
			(write-gss-priv stream writer obj
					(gss-context-context context)
					(gss-cred-seqno cred))))
		     (otherwise (write-xtype writer stream obj)))))
	    
	    ;; funcall the handler to get the result 
	    (let ((arg (handler-case (read-xtype #'arg-reader input-stream)
			 (error (e)
			   (frpc-log :trace "Failed to read argument: ~A" e)
			   (write-rpc-response output-stream 
					       :accept :garbage-args :id id)
			   (return-from process-rpc-call)))))
	      
	      (let ((res (handler-case 
			     (with-caller-binded (host port protocol auth)
			       (funcall handler arg))
                       (rpc-auth-error (e)
                         (frpc-log :trace "Handler signalled an auth error")
                         (write-rpc-response output-stream
                                             :reject :auth-error
                                             :id id
                                             :auth-stat (or (auth-error-stat e) :auth-tooweak))
                         (return-from process-rpc-call))
			   (error (e)
			     (frpc-log :trace "Failed to invoke handler: ~A" e)
			     ;; be silent if the handler errors, this allows us to 
			     ;; provide the "silent" semantics that some APIs require
			     ;;	(write-rpc-response output-stream :accept :garbage-args :id id) ;; This is what I used to do
			     (return-from process-rpc-call)))))
		
		(write-rpc-response output-stream :accept :success :id id :verf rverf)
		(write-xtype #'res-writer output-stream res)))))))))

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
    (let ((res (handler-case (gss-authenticate token)
		 (error () nil))))
      (cond
	(res
	 ;; FIXME: it might be that we need to authenticate ourselves to the client
	 ;; we need to send the response buffer as the TOKEN parameter of the res struct
	 (frpc-log :info "GSS context granted")
	 (write-rpc-response output-stream :accept :success :id id)
	 (%write-gss-init-res output-stream res))
	(t
	 ;; no context granted... means was invalid token
	 (frpc-log :info "No GSS context granted")
	 (write-rpc-response output-stream 
			     :reject :auth-error
			     :id id
			     :auth-stat :gss-cred-problem))))))

(defun process-rpc-request (input-stream output-stream &key host port protocol programs)
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
      
      ;; check here that the call program is in the list of programs which we are willing to handle 
      (unless (or (null programs) (member (call-body-prog call) programs))
      	(write-rpc-response output-stream
      			    :accept :prog-unavail
      			    :id (rpc-msg-xid msg))
      	(return-from process-rpc-request))
			    
		  
      ;; if the authenticator is a GSS init (FIXME: or continue) command then we need to do special things
      (when (and (eq (opaque-auth-flavour auth) :auth-gss)
                 (eq (gss-cred-proc (opaque-auth-data auth)) :init))
        (frpc-log :trace "Process GSS init: ~A ~A" 
                  (opaque-auth-flavour auth) (gss-cred-proc (opaque-auth-data auth)))
        (process-gss-init-command input-stream output-stream id)
        (return-from process-rpc-request))

      ;; if the host is an integer then convert it to an octet vector.
      ;; LispWorks and CCL are both known to provide host IPs as the htol integer
      ;; but an octet vector is more useful I think, plus it means handlers can consistently 
      ;; assume the *rpc-remote-host* is an octet vector.
      (when (integerp host)
        (setf host 
              (let ((v (nibbles:make-octet-vector 4)))
                (setf (nibbles:ub32ref/be v 0) host)
                v)))

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

;; new versions of the single-threaded server using the new API
(defun run-rpc-server (server)
  "Keep processing RPC requests until the EXITING flag is set."
  (declare (type rpc-server server))
  (do ()
      ((rpc-server-exiting server))
    (accept-rpc-request server)))

(defun start-rpc-server (server)
  "Startup the RPC server, then spawn a new thread to process requests.
Call STOP-RPC-SERVER to shut the server down.

SERVER ::= an instance of RPC-SERVER, as returned from MAKE-RPC-SERVER."
  (declare (type rpc-server server))
  (startup-rpc-server server)
  (setf (rpc-server-thread server)
	(bt:make-thread (lambda () (run-rpc-server server))
			:name "rpc-server-thread"))
  server)

(defun stop-rpc-server (server)
  "Stop the RPC server processing thread and wait for it to exit.
Then shuts the server down to free all resources.

SERVER ::= an instance of RPC-SERVER which was used to spawn an accepting 
thread by START-RPC-SERVER.
"
  (declare (type rpc-server server))
  (setf (rpc-server-exiting server) t)
  (bt:join-thread (rpc-server-thread server))
  (shutdown-rpc-server server)
  nil)


;; -------------------------------------------------

;; a default handler for the null procedure
(defun default-null-handler (void)
  (declare (ignore void))
  nil)

;; -------------------------------------------------

(defun startup-rpc-server (server) 
  "Initialize the RPC server and allocate listening ports.

SERVER ::= an instance of RPC-SERVER, as returned from MAKE-RPC-SERVER.

If no ports are specified, a wildcard port is added to both UDP and TCP ports.

If no portmapper is detected to be running on localhost, then port 111 is 
added to both UDP and TCP port lists, so that the port mapper will be run by this server.

Can signal RPC errors if unable to communicate with local port mapper or unable
to bind the listening sockets.

This server should then be used to accept and process RPC requests by calling
ACCEPT-RPC-REQUEST. 

Call SHUTDOWN-RPC-SERVER to free all resources and close the sockets.
"
  (declare (type rpc-server server))
  (let ((programs (rpc-server-programs server)))

  (setf (rpc-server-exiting server) nil
	(rpc-server-thread server) nil)

  (unless *handlers* (warn "No programs registered."))

  (unless (and (rpc-server-udp-ports server)
	       (rpc-server-tcp-ports server))
    (warn "No ports selected, listening on wildcard port.")
    (push 0 (rpc-server-udp-ports server))
    (push 0 (rpc-server-tcp-ports server)))

  ;; try talking to the local port mapper
  (let ((pmap-p (handler-case (progn (frpc.bind:call-null :host "localhost") t)
                  (error () nil))))
    (unless pmap-p
      (warn "No port mapper detected, running portmap locally.")
      (pushnew 111 (rpc-server-udp-ports server))
      (pushnew 111 (rpc-server-tcp-ports server))))

  (let (tcp-sockets udp-sockets)
    (handler-bind ((error (lambda (e)
			    (declare (ignore e))
			    (dolist (s tcp-sockets)
			      (usocket:socket-close s))
			    (dolist (s udp-sockets)
			      (usocket:socket-close s)))))
      (dolist (port (rpc-server-tcp-ports server))
	(push (usocket:socket-listen usocket:*wildcard-host* port
				     :reuse-address t
				     :element-type '(unsigned-byte 8))
	      tcp-sockets))
      (dolist (port (rpc-server-udp-ports server))
	(push (usocket:socket-connect nil nil 
				      :protocol :datagram
				      :element-type '(unsigned-byte 8)
				      :local-port port)
	      udp-sockets))
      
      ;; add all port mappings 
      (let ((client (make-instance 'unix-client :host "localhost"))
	    (rpcp (member 111 (union (rpc-server-tcp-ports server) (rpc-server-udp-ports server)))))
	(dolist (program (or programs (mapcar #'car *handlers*)))
	  (dolist (version (mapcar #'car (find-handler program)))
	    (dolist (s tcp-sockets)
	      (let* ((port (usocket:get-local-port s))
		     (mapping (frpc.bind:make-mapping :program program
						      :version version
						      :protocol :tcp
						      :port port)))
		(when (if (= program frpc.bind::+pmapper-program+) 
			  (= port 111)
			  t)
		  (if rpcp
		      ;; we are running the port mapper from within Lisp, we can just add the mapping directly 
		      (frpc.bind:add-mapping mapping)
		      (frpc.bind:call-set mapping :client client)))))
	    (dolist (s udp-sockets)
	      (let* ((port (usocket:get-local-port s))
		     (mapping (frpc.bind:make-mapping :program program
						      :version version
						      :protocol :udp
						      :port port)))
		(when (if (= program frpc.bind::+pmapper-program+)
			  (= port 111)
			  t)
		  (if rpcp
		      (frpc.bind:add-mapping mapping)
		      (frpc.bind:call-set mapping :client client)))))))))

    (setf (rpc-server-tcp-sockets server) tcp-sockets
	  (rpc-server-udp-sockets server) udp-sockets)

    nil)))
  
  

(defun shutdown-rpc-server (server)
  "Close all opened sockets and shut the server down.

SERVER ::= an instance of RPC-SERVER.
"
  (declare (type rpc-server server))
  (let ((tcp-sockets (rpc-server-tcp-sockets server))
	(udp-sockets (rpc-server-udp-sockets server))
	(programs (rpc-server-programs server)))
    ;; remove all port mappings 
    (let ((tcp-ports (mapcar #'usocket:get-local-port tcp-sockets))
	  (udp-ports (mapcar #'usocket:get-local-port udp-sockets))
	  (client (make-instance 'unix-client :host "localhost")))
      (dolist (program (or programs (mapcar #'car *handlers*)))
	(dolist (version (mapcar #'car (find-handler program)))
	  (dolist (s tcp-sockets)
	    (let* ((port (usocket:get-local-port s))
		   (mapping (frpc.bind:make-mapping :program program
						    :version version
						    :protocol :tcp
						    :port port)))		 
	      (if (member 111 (union tcp-ports udp-ports))
		;; we are running the port mapper from within Lisp, we can just remove the mapping directly 
		(frpc.bind:rem-mapping mapping)
		(frpc.bind:call-unset mapping :client client))))
	  (dolist (s udp-sockets)
	    (let* ((port (usocket:get-local-port s))
		   (mapping (frpc.bind:make-mapping :program program
						    :version version
						    :protocol :udp
						    :port port)))
		(if (member 111 (union tcp-ports udp-ports))
		    (frpc.bind:rem-mapping mapping)
		    (frpc.bind:call-unset mapping :client client)))))))

    (dolist (s tcp-sockets)
      (usocket:socket-close s))
    (dolist (s udp-sockets)
      (usocket:socket-close s)))
  nil)

(defun accept-rpc-request (server &key (timeout 1))
  "Accept and process a single RPC request.

SERVER ::= an RPC-SERVER instance that has been started, by first calling STARTUP-RPC-SERVER.
TIMEOUT ::= timeout in seconds to wait for the request. NIL implies waiting indefinitely.

No meaningful return value.
"
  (declare (type rpc-server server))
  ;; start by purging connections which may have timed out 
  (setf (rpc-server-connections server)
	(purge-connection-list (rpc-server-connections server) 
			       (get-universal-time)
			       (rpc-server-timeout server)))

  (let ((tcp-sockets (rpc-server-tcp-sockets server))
	(udp-sockets (rpc-server-udp-sockets server))
	(programs (rpc-server-programs server)))
    ;; poll and timeout, process any sockets which are ready 
    (let* ((udp-buffer (frpc.streams:allocate-buffer))
	   (ready (usocket:wait-for-input (append tcp-sockets 
						  udp-sockets
						  (mapcar #'rpc-connection-conn (rpc-server-connections server)))
					  :ready-only t :timeout timeout)))
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
	     (push conn (rpc-server-connections server))))
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
						    :protocol :udp
						    :programs programs))))
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
	   (let ((c (find socket (rpc-server-connections server) :key #'rpc-connection-conn)))
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
						 :protocol :tcp
						 :programs programs))))
		     (unless (zerop (length response-buffer))
		       (write-uint32 (usocket:socket-stream socket)
				     (logior #x80000000 (length response-buffer)))
		       (write-sequence response-buffer (usocket:socket-stream socket)))
		     (force-output (usocket:socket-stream socket)))))
	     (end-of-file ()
	       (frpc-log :info "Connection closed by remote host")
	       (setf (rpc-server-connections server)
		     (remove socket (rpc-server-connections server) :key #'rpc-connection-conn)))
	     (error (e)
	       (frpc-log :info "Error: ~A" e)
	       (ignore-errors (usocket:socket-close socket))
	       (setf (rpc-server-connections server)
		     (remove socket (rpc-server-connections server) :key #'rpc-connection-conn))))))))))

