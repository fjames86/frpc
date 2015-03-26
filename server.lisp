;;;; Copyright (c) Frank James 2015 <frank.a.james@gmail.com>
;;;; This code is licensed under the MIT license.


(in-package #:frpc)

;; ------------- server handlers ---------------------

;; server handlers might need to know who called them
;; bind these special variables to the remote host, port and protocol
(defvar *rpc-remote-host* nil)
(defvar *rpc-remote-port* nil)
(defvar *rpc-remote-protocol* nil)
(defvar *rpc-remote-auth* nil)

(defmacro with-caller-binded ((host port protocol) &body body)
  `(let ((*rpc-remote-host* ,host)
	 (*rpc-remote-port* ,port)
	 (*rpc-remote-protocol* ,protocol))
     ,@body))


;; stores an assoc list for each program id
;; each program id stores an alist of version ids
;; each version id stores an aslist of handler lists
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

(defun handle-request (server input-stream output-stream)
  "Read an RPC request from the STREAM and write the output to the OUTPUT stream (or STREAM if not provided). This is for
TCP requests only."
  (handler-case 
      (let ((msg (%read-rpc-msg input-stream)))
	(log:debug "Recieved message ID ~A" (rpc-msg-xid msg))
	;; validate the message
	(cond
	  ((not (eq (xunion-tag (rpc-msg-body msg)) :call))
	   ;; not a call
	   (log:info "Bad request: not a call")
	   (%write-rpc-msg output-stream
			   (make-rpc-response :accept :garbage-args
					      :id (rpc-msg-xid msg))))
	  ((not (= (call-body-rpcvers (xunion-val (rpc-msg-body msg))) 2))
	   ;; rpc version != 2
	   (log:info "RPC version not = 2")
	   (%write-rpc-msg output-stream
			   (make-rpc-response :reject :rpc-mismatch
					      :id (rpc-msg-xid msg))))
	  (t 
	   (let* ((call (xunion-val (rpc-msg-body msg)))
		  (h (find-handler (call-body-prog call)
				   (call-body-vers call)
				   (call-body-proc call))))
	     (log:debug "Calling ~A:~A:~A" (call-body-prog call) (call-body-vers call) (call-body-proc call))
	     (cond
	       ((and (rpc-server-programs server)
		     (not (member (call-body-prog call) 
				  (rpc-server-programs server))))
		;; program not in the list of permissible programs
		(log:info "Program ~A not in program list" (call-body-prog call))
		(%write-rpc-msg output-stream
				(make-rpc-response :accept :prog-mismatch
						   :id (rpc-msg-xid msg))))
	       ((rpc-server-auth-handler server)
		(unless (funcall (rpc-server-auth-handler server) (call-body-auth call) (call-body-verf call))
		  (log:info "Authentication failure")
		  (%write-rpc-msg output-stream
				  (make-rpc-response :reject :auth-error 
						     :auth-stat :auth-rejected
						     :id (rpc-msg-xid msg)))))
	       ((or (not h) (null (third h)))
		;; no handler registered
		(log:debug "No handler registered")
		(%write-rpc-msg output-stream
				(make-rpc-response :accept :proc-unavail
						   :id (rpc-msg-xid msg))))
	       (t 
		;; log the authentication
;;		(unless (eq (opaque-auth-flavour (call-body-auth call)) :auth-null)
;;		  (log:debug "Authentication: ~S" (call-body-auth call)))
		;; run the handler 
		(destructuring-bind (arg-type res-type handler) h
		  (handler-case 
		      (let ((arg (read-xtype arg-type input-stream)))
			(log:debug "Passing arg to handler")
			(let ((res (let ((*rpc-remote-auth* (call-body-auth call)))
				     (funcall handler arg))))
			  (log:debug "Call successful")
			  (%write-rpc-msg output-stream
					  (make-rpc-response :accept :success
							     :id (rpc-msg-xid msg)))
			  (write-xtype res-type output-stream res)))
            (rpc-accept-error (e)
              (log:error "Accept error: ~A" e)
              (%write-rpc-msg output-stream
                              (make-rpc-response :accept (rpc-accept-error-stat e)
                                                 :id (rpc-msg-xid msg))))
            (rpc-auth-error (e)
              (log:error "Auth error: ~A" e)
              (%write-rpc-msg output-stream
                              (make-rpc-response :reject :auth-error
                                                 :auth-stat (auth-error-stat e)
                                                 :id (rpc-msg-xid msg))))

		    (undefined-function (e)
		      ;; no such function -- probably means we didn't register a handler
		      (log:warn "No handler: ~S" e)
		      (%write-rpc-msg output-stream
				      (make-rpc-response :accept :proc-unavail
							 :id (rpc-msg-xid msg))))
		    (error (e)
		      ;; FIXME: should we just terminate the connection at this point?
		      (log:error "Error handling: ~A" e) 
		      (%write-rpc-msg output-stream
				      (make-rpc-response :accept :garbage-args
							 :id (rpc-msg-xid msg))))))))))))
    (end-of-file (e)
      ;; unexpected end of file -- probably means the connection was closed 
      ;; rethrow the error so the connection handler knows the connection has been closed
      (error e))
    (error (e)
      (log:error "Error reading msg: ~A" e)
      (%write-rpc-msg output-stream
		      (make-rpc-response :accept :garbage-args))))
  (log:debug "Flushing output")
  (force-output output-stream)
  (log:debug "Finished request"))

;; ------------- rpc server ----------

(defun process-rpc-connection (server conn)
  (let ((stream (usocket:socket-stream conn)))
    (log:debug "Reading request")
    (flexi-streams:with-input-from-sequence (input (read-fragmented-message stream))
      (log:debug "Read request")
      (let ((buff (flexi-streams:with-output-to-sequence (output)
		    (with-caller-binded ((usocket:get-peer-address conn)
					 (usocket:get-peer-port conn)
					 :tcp)
		      (handle-request server input output)))))
	(log:debug "Writing response")
	;; write the fragment header (with terminating bit set)
	(write-uint32 stream (logior #x80000000 (length buff)))
	;; write the buffer itself
	(write-sequence buff stream)
	;; flush output
	(force-output stream)))))

;; -------------------- udp rpc server -------------

(defun process-udp-request (server msg input-stream)
  "Returns a packed buffer containing the response to send back to the caller."
  (let ((id (rpc-msg-xid msg))
	(call (xunion-val (rpc-msg-body msg))))
    (let ((program (call-body-prog call))
	  (version (call-body-vers call))
	  (proc (call-body-proc call)))
      (log:debug "Calling ~A:~A:~A" program version proc)
      (let ((h (find-handler program version proc)))
	(cond
	  ((and (rpc-server-programs server)
		(not (member program (rpc-server-programs server))))
	   ;; not in allowed programs list
	   (log:warn "Program ~A not in program list" program)
	   (pack #'%write-rpc-msg 
		 (make-rpc-response :accept :prog-mismatch
				    :id id)))
	  ((and (rpc-server-auth-handler server)
		(not (funcall (rpc-server-auth-handler server)
			      (call-body-auth call)
			      (call-body-verf call))))
	   (log:debug "Authentication failure")
	   (pack #'%write-rpc-msg 
		 (make-rpc-response :reject :auth-error 
				    :id id
				    :auth-stat :auth-rejected)))
	  ((or (not h) (null (third h)))
	   ;; no handler
	   (log:warn "No handler registered for ~A:~A:~A" program version proc)
	   (pack #'%write-rpc-msg
		 (make-rpc-response :accept :proc-unavail
				    :id id)))
	  (t
	   ;; log the authentication
;;	   (unless (eq (opaque-auth-flavour (call-body-auth call)) :auth-null)
;;	     (log:debug "Authentication: ~S" (call-body-auth call)))
	   ;; handle the request
	   (destructuring-bind (reader writer handler) h
	     ;; read the argument
	     (let ((arg (read-xtype reader input-stream)))
           ;; package the reply and send
           (flexi-streams:with-output-to-sequence (output)             
             (handler-case 
                 ;; run the handler
                 (let ((res (let ((*rpc-remote-auth* (call-body-auth call)))
                              (funcall handler arg))))
                   (%write-rpc-msg output
                                   (make-rpc-response :accept :success
                                                      :id id))
                   (write-xtype writer output res))
               (rpc-accept-error (e)
                 (log:error "Accept error: ~A" e)
                 (%write-rpc-msg output
                                 (make-rpc-response :accept (rpc-accept-error-stat e)
                                                    :id (rpc-msg-xid msg))))
               (rpc-auth-error (e)
                 (log:error "Auth error: ~A" e)
                 (%write-rpc-msg output
                                 (make-rpc-response :reject :auth-error
                                                    :auth-stat (auth-error-stat e)
                                                    :id (rpc-msg-xid msg))))               
               (error (e)
                 (log:error "error handling: ~A" e)
                 (%write-rpc-msg output
                                 (make-rpc-response :accept :garbage-args
                                                    :id (rpc-msg-xid msg))))))))))))))


(defun handle-udp-request (server socket buffer length remote-host remote-port)
  "Given a buffer containing an RPC message and argument, read it and take appropriate action."
;;  (log:debug "Buffer ~S" buffer)
  (flexi-streams:with-input-from-sequence (input buffer :start 0 :end length)
    (let ((msg (%read-rpc-msg input)))
      (ecase (xunion-tag (rpc-msg-body msg))
	(:reply
	 ;; received a reply to the server port --- is a bit odd since we never 
	 ;; initiate rpcs from the server so don't expect any replies. let's just discard it 
	 (log:warn "Recieved reply from ~A:~A" remote-host remote-port))
	(:call 
	 ;; is a call -- lookup the proc handler and send a reply 
	 (log:debug "Recived call from ~A:~A" remote-host remote-port)
	 (let ((return-buffer (process-udp-request server msg input)))
	   (log:debug "Sending reply to ~A:~A" remote-host remote-port)
	   (usocket:socket-send socket return-buffer (length return-buffer) 
				:host remote-host :port remote-port)))))))

(defun accept-udp-rpc-request (server socket buffer)
  "Wait for and read a datagram from the UDP socket. If successfully read the message then process it."
  (multiple-value-bind (%buffer length remote-host remote-port) (usocket:socket-receive socket buffer (length buffer))
    (declare (ignore %buffer))
    (log:debug "UDP msg from ~A:~A (length ~A)" remote-host remote-port length)
    (cond
      ;; there seems to be a bug in SBCL which causes it not to detect error return value (-1)
      ;; from recvfrom -- this means socket-receive seemingly returns successfully but with a length
      ;; of 4294967295. we shouldn't really need to be checking for it here, but this is a workaround
      ((= length #xffffffff)
       (log:error "recvfrom returned -1"))
      (t 
       (handler-case 
	   (with-caller-binded (remote-host remote-port :udp)
	     (handle-udp-request server socket
				 buffer length
				 remote-host remote-port))
	 (error (e)
	   (log:error "Error handling: ~S" e)))))))



;; --------------------------------------

(defstruct (rpc-server (:constructor %make-rpc-server))
  thread 
  programs 
  exiting
  auth-handler)

(defun make-rpc-server (&key programs auth-handler)
  "Make an RPC server instance. 

PROGRAMS should be a list of program numbers to be accepted by the server, 
all RPC requests for programs not in this list will be rejected. 
If not supplied all program requests are accepted. 

AUTH-HANDLER, if supplied, should be a function of signature (AUTH VERF) that returns a boolean
indicating whether the authentication succeeded. Both AUTH and VERF are OPAQUE-AUTH structures
as sent in the RPC request.
"
  (%make-rpc-server :programs programs
		    :auth-handler auth-handler))


(defstruct rpc-connection 
  conn time)

;; FIXME: should we put a limit on the maximum number of simultaneous connections?
(defun run-rpc-server (server &key tcp-ports udp-ports (timeout 60))
  "Run the RPC server until the SERVER-EXITING flag is set. Will open TCP sockets listening on the TCP-PORTS list and UDP sockets listening on the UDP-PORTS list.

TIMEOUT should be an integer specifying the maximum time TCP connections should be held open."
  (declare (type rpc-server server)
	   (type list tcp-ports udp-ports)
	   (type fixnum timeout))
  (let (tcp-sockets udp-sockets connections)
    (unwind-protect 
	 (progn 
	   ;; collect the sockets this way so that if there is an error thrown (such as can't listen on port)
	   ;; then we can gracefully fail, and close the sockets we have opened
	   ;; if we mapcar to collect them then we lose the sockets we opened before the failure
	   (dolist (port tcp-ports)
	     (push (usocket:socket-listen usocket:*wildcard-host* port
					  :reuse-address t
					  :element-type '(unsigned-byte 8))
		   tcp-sockets))
	   (dolist (port udp-ports)
	     (push (usocket:socket-connect nil nil 
					   :protocol :datagram
					   :element-type '(unsigned-byte 8)
					   :local-port port)
		   udp-sockets))

	   ;; the polling-loop
	   (do ((udp-buffer (nibbles:make-octet-vector 65507))
		(prev-time (get-universal-time)))
	       ((rpc-server-exiting server))

	     ;; if any connections are getting old then close them
	     ;; only do the check at most once per second
	     (let ((now (get-universal-time)))
	       (when (> now prev-time)
		 (setf connections
		       (mapcan (lambda (c)
				 (cond
				   ((> (- now (rpc-connection-time c)) timeout)
				    ;; the connection is old, close it 
				    (log:debug "Purging connection to ~A" (usocket:get-peer-address (rpc-connection-conn c)))
				    (handler-case (usocket:socket-close (rpc-connection-conn c))
				      (error (e)
					(log:debug "Couldn't close connection: ~S" e)))
				    nil)
				   (t 
				    (list c))))
			       connections)
		       prev-time now)))
	     
	     ;; poll and timeout each second so that we can check the exiting flag
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
		      (log:debug "Accepting TCP connection from ~A:~A" 
				 (usocket:get-peer-address (rpc-connection-conn conn))
				 (usocket:get-peer-port (rpc-connection-conn conn)))
		      (push conn connections)))
		   (usocket:datagram-usocket
		    ;; a udp socket is ready to read from
		    (handler-case (accept-udp-rpc-request server socket udp-buffer)
		      (error (e)
			;; windows is known to throw an error on receive if there was no-one 
			;; listening on the port the previous UDP packet was sent to.
			(log:debug "~A" e))))
		   (usocket:stream-usocket 
		    ;; a connection is ready to read 
		    (let ((c (find socket connections :key #'rpc-connection-conn)))
		      (setf (rpc-connection-time c) (get-universal-time)))
		    (handler-case (process-rpc-connection server socket)
		      (end-of-file ()
			(log:debug "Connection closed by remote host")
			(setf connections (remove socket connections :key #'rpc-connection-conn)))
		      (error (e)
			(log:debug "Error: ~S" e)
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

(defun start-rpc-server (server &key tcp-ports udp-ports (timeout 60))
  "Start the RPC server in a new thread. 

TCP-PORTS and UDP-PORTS should be lists of integers specifying the ports to listen on, for each 
transport protocol respectively.

If ADD-PORT-MAPPINGS is non-nil then all possible port mappings will be added to the port mapper. If 
you only want to advertise each service on particular ports then you should set this to nil and 
add each mapping manually (using ADD-MAPPING).

When specified, TIMEOUT will be set as the TCP receive timeout (read timeout). Accepted TCP connections which are inactive for longer than TIMEOUT will be closed.
"
  ;; add all the port mappings  
  ;;  (when add-port-mappings (port-mapper:add-all-mappings tcp-ports udp-ports))
  (setf (rpc-server-thread server)
	(bt:make-thread (lambda ()
			  (run-rpc-server server 
					  :tcp-ports tcp-ports 
					  :udp-ports udp-ports 
					  :timeout timeout))
			:name "rpc-server-thread"))
  server)

(defun stop-rpc-server (server)
  "Stop the RPC server and wait until its thread to exit."
  (setf (rpc-server-exiting server) t)
  (bt:join-thread (rpc-server-thread server))
  nil)
