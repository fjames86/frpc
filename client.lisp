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
  "Read an RPC response from the stream."
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
	     (:auth-error (error 'rpc-auth-error :stat (xunion-val denied)))
	     (:rpc-mismatch (error 'rpc-error :description "RPC mismatch")))))
	(t 
	 (case (xunion-tag (accepted-reply-reply-data (xunion-val body)))
	   (:prog-mismatch
	    (let ((a (xunion-val (accepted-reply-reply-data (xunion-val body)))))
	      (error 'rpc-mismatch-error 
		     :low (cdr (assoc 'low a))
		     :high (cdr (assoc 'high a))
		     :description "Program mismatch")))
	   (:success ;; do nothing -- the result value will be read at the end 
	    nil)
	   (otherwise
	    (error 'rpc-accept-error 
		   :stat (xunion-tag (accepted-reply-reply-data (xunion-val body)))
		   :description "RPC Failed"))))))
    ;; only got to here (to read the response) if the message was successful
    ;; discard the response msg since we don't need it any more 
    (values msg
	    (read-xtype res-type stream))))

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

;; ---------------------------------------

(defparameter *rpc-host* "localhost")
(defparameter *rpc-port* 111)

(defparameter *default-rpc-host* '*rpc-host*)
(defparameter *default-rpc-port* '*rpc-port*)

(defmacro use-rpc-host (host port)
  `(eval-when (:load-toplevel :compile-toplevel :execute)
     (setf *default-rpc-host* ,host
	   *default-rpc-port* ,port)))

;; ------------- default/null authentication ---------------

(defclass rpc-client ()
  ((host :initarg :host :initform nil :accessor rpc-client-host)
   (port :initarg :port :initform nil :accessor rpc-client-port)
   (program :initarg :program :initform nil :accessor rpc-client-program)
   (version :initarg :version :initform nil :accessor rpc-client-version)
   (initial :initform t :accessor rpc-client-initial)
   (connection :initarg :connection :initform nil :accessor rpc-client-connection)
   (protocol :initarg :protocol :initform nil :accessor rpc-client-protocol)
   (timeout :initarg :timeout :initform nil :accessor rpc-client-timeout)))

;;(defmethod reinitialize-instance :after ((instance rpc-client) &key)
;;  (setf (rpc-client-initial instance) nil))

(defgeneric rpc-client-auth (client)
  (:documentation "The authenticator to use for the client request."))

(defgeneric rpc-client-verf (client)
  (:documentation "The verifier to use for the client request."))

(defgeneric verify (client verf)
  (:documentation "Verify the response received from the server. Signals an error on failure."))


;; default methods for auth-null flavour authentication 
(defmethod rpc-client-auth ((client rpc-client))
  (make-opaque-auth :auth-null nil))

(defmethod rpc-client-verf ((client rpc-client))
  (make-opaque-auth :auth-null nil))

(defmethod verify ((client rpc-client) verf) t)

;; ---------------- unix ----------------

(defclass unix-client (rpc-client)
  ((uid :initarg :uid :initform 0 :accessor unix-client-uid)
   (gid :initarg :gid :initform 0 :accessor unix-client-gid)
   (gids :initarg :gids :initform nil :accessor unix-client-gids)
   (nickname :initform nil :accessor unix-client-nickname)))

(defmethod print-object ((client unix-client) stream)
  (print-unreadable-object (client stream :type t)
    (format stream ":NICKNAME ~A" (unix-client-nickname client))))

(defmethod rpc-client-auth ((client unix-client))
  (if (rpc-client-initial client)
      (make-opaque-auth :auth-unix
			(make-auth-unix :stamp (- (get-universal-time) 
						  (encode-universal-time 0 0 0 1 1 1970 0))
					:machine-name (machine-instance)
					:uid (unix-client-uid client)
					:gid (unix-client-gid client)
					:gids (unix-client-gids client)))
      (make-opaque-auth :auth-short 
			(unix-client-nickname client))))

(defmethod verify ((client unix-client) verf)
  (when (eq (opaque-auth-flavour verf) :auth-short)
    (setf (unix-client-nickname client) (opaque-auth-data verf)
	  (rpc-client-initial client) nil))
  t)

;; ----------------- des -----------------

(defclass des-client (rpc-client)
  ((name :initform nil :initarg :name :accessor des-client-name)
   (secret :initform nil :initarg :secret :accessor des-client-secret)
   (public :initform nil :initarg :public :accessor des-client-public)
   (key :initarg :key :initform (des-conversation) :accessor des-client-key)
   (window :initform 300 :initarg :window :accessor des-client-window)
   (nickname :initform nil :accessor des-client-nickname)
   (timestamp :initform nil :accessor des-client-timestamp))) ;; timestamp used in request

(defmethod initialize-instance :after ((inst des-client) &key)
  ;; just check that the name, secret and public have been provided
  (unless (des-client-name inst) (error "Must provide a client NAME"))
  (unless (des-client-secret inst) (error "Must provide a client secret key"))
  (unless (des-client-public inst) (error "Must provide a server public key"))
  inst)

(defmethod print-object ((client des-client) stream)
  (print-unreadable-object (client stream :type t)
    (format stream ":NICKNAME ~A" (des-client-nickname client))))

(defmethod rpc-client-auth ((client des-client))
  ;; if initial request then send a fullname cred, otherwise send a nickname 
  (if (rpc-client-initial client)
      (let ((timestamp (des-timestamp)))
	;; store the timestamp so we can compare with the response timestamp
	(setf (des-client-timestamp client) timestamp)
	(des-initial-auth (des-client-key client)
			  (des-client-name client)
			  (des-client-secret client)
			  (des-client-public client)
			  (des-client-window client)
			  timestamp))
      (let ((timestamp (des-timestamp)))
	;; store the timestamp so we can compare with the response timestamp
	(setf (des-client-timestamp client) timestamp)
	(des-auth (des-client-nickname client)))))

(defmethod rpc-client-verf ((client des-client))
  (if (rpc-client-initial client)
      (des-initial-verf (des-client-key client) 
			(des-client-window client)
			(des-client-timestamp client))
      (des-verf (des-client-key client))))

(defmethod verify ((client des-client) verf)
  (let ((v (unpack #'%read-authdes-verf-server (opaque-auth-data verf))))
    (unless (des-valid-server-verifier (des-client-key client)
				       (des-client-timestamp client)
				       v)
      (error 'rpc-error :description "Invalid DES verifier"))
    ;; store the nickname 
    (setf (des-client-nickname client) (authdes-verf-server-adv-nickname v)
	  (rpc-client-initial client) nil)))

;; ----------------- gss ---------------

(defclass gss-client (rpc-client)
  ((context :initarg :context :accessor gss-client-context) 
   (handle :initform nil :accessor gss-client-handle)
   (seqno :initform 0 :accessor gss-client-seqno)
   (service :initarg :service :initform :none :accessor gss-client-service)))

(defmethod print-object ((client gss-client) stream)
  (print-unreadable-object (client stream :type t)
    (format stream ":HANDLE ~S" (gss-client-handle client))))

(defmethod rpc-client-auth ((client gss-client))
  (when (rpc-client-initial client)
    (let ((res 
	   (call-rpc #'%write-gss-init-arg
		     (glass:initialize-security-context (gss-client-context client))
		     #'%read-gss-init-res
		     :host (rpc-client-host client)
		     :port (rpc-client-port client)
		     :program (rpc-client-program client)
		     :version (rpc-client-version client)
		     :auth (make-opaque-auth :auth-gss
					     (make-gss-cred :proc :init
							    :seqno (gss-client-seqno client)
							    :service :none))
		     :protocol (rpc-client-protocol client)
		     :timeout (rpc-client-timeout client)
		     :connection (rpc-client-connection client))))
      ;; store the handle for future requests
      (setf (gss-client-handle client) (gss-init-res-handle res)
	    (rpc-client-initial client) nil)))

  ;; increment the seqno
  (incf (gss-client-seqno client))

  ;; return authenticator
  (make-opaque-auth :auth-gss
		    (make-gss-cred :proc :data
				   :seqno (gss-client-seqno client)
				   :service (gss-client-service client) 
				   :handle (gss-client-handle client))))

;; the client does not send a verifier 

;; if mutual authentication was requested, there may be a verifier returned


;; ---------------------------------------------------------

(defun rpc-connect (host port &optional (protocol :udp))
  "Establish a connection to the rpc server. MUST be closed with a call to RPC-CLOSE."
  (ecase protocol
    (:udp 
     (usocket:socket-connect host port
			     :protocol :datagram
			     :element-type '(unsigned-byte 8)))
    (:tcp
     (usocket:socket-connect host (or port *rpc-port*)
			     :protocol :stream
			     :element-type '(unsigned-byte 8)))))

(defun rpc-close (conn)
  "Close the connection to the server."
  (usocket:socket-close conn))
	    
(defmacro with-rpc-connection ((var host port &optional protocol) &body body)
  "Execute the body in the context of a connection."
  `(let ((,var (rpc-connect ,host ,port ,@(when protocol `(,protocol)))))
     (unwind-protect (progn ,@body)
       (rpc-close ,var))))

(defun call-rpc-server (connection arg-type arg result-type
			&key (program 0) (version 0) (proc 0)
			  auth verf request-id)
  "Send a TCP request to the RPC server and await a response."
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
;;      (nth-value 1 (read-response input result-type)))))
      (multiple-value-bind (msg res) (read-response input result-type)
	(values res (rpc-msg-verifier msg))))))

(defun collect-udp-replies (socket timeout result-type)
  "Wait TIMEOUT seconds, collecting as many UDP replies as arrive in that time."
  (do ((replies nil)
       (buffer (frpc.streams:allocate-buffer)) ;;(nibbles:make-octet-vector 65507))
       (start (get-universal-time))
       (now (get-universal-time) (get-universal-time)))
      ((> (- now start) timeout) replies)
    (when (usocket:wait-for-input socket :timeout (- timeout (- now start)) :ready-only t)
      (multiple-value-bind (%buffer count remote-host remote-port) (usocket:socket-receive socket buffer 65507)
	(declare (ignore %buffer))
	(cond
	  ;; workaround for sbcl bug. seems like there is a similar bug in the usocket Lispworks codes too
	  ((or (= count #xffffffff) (= count -1))
	   (frpc-log :error "recvfrom returned -1"))
	  (t
	   (frpc-log :trace "Received response from ~A:~A (length ~A)" remote-host remote-port count)
	   (frpc.streams:with-buffer-stream (input buffer :start 0 :end count)
;;	   (flexi-streams:with-input-from-sequence (input buffer :start 0 :end count)
         (handler-case
             (let ((msg (%read-rpc-msg input)))
               (frpc-log :trace "MSG ID ~A" (rpc-msg-xid msg))
               (let ((body (rpc-msg-body msg)))
                 (cond
                   ((eq (xunion-tag (xunion-val body)) :msg-denied)
                    ;; rpc denied e.g. auth-error
                    (list remote-host remote-port nil))
                   ((not (eq (xunion-tag (accepted-reply-reply-data (xunion-val (xunion-val body))))
                             :success))
                    ;; rpc unsuccessful e.g. prog-unavail
                    (list remote-host remote-port nil))
                   (t 
                    ;; FIXME: should really check the reply's id
                    (push (list remote-host remote-port (read-xtype result-type input))
                          replies)))))
           (error (e)
             (frpc-log :error "~A" e)
             (frpc-log :error "~S" (subseq buffer 0 count))
             (list remote-host remote-port nil))))))))))
    
(defun send-rpc-udp (socket arg-type arg &key program version proc auth verf request-id host port)
  (let ((buffer (frpc.streams:allocate-buffer)))
    (let ((count (frpc.streams:with-buffer-stream (stream buffer)
		   ;;	 (flexi-streams:with-output-to-sequence (stream)
		   (write-request stream 
				  (make-rpc-request program proc 
						    :version version
						    :auth auth
						    :verf verf
						    :id request-id)
				  arg-type
				  arg))))
      (usocket:socket-send socket buffer count 
			   :host host :port port))))

(defun broadcast-rpc (arg-type arg result-type 
		     &key host port program version proc auth verf request-id timeout)
  (let ((socket (usocket:socket-connect nil nil
					:protocol :datagram
					:element-type '(unsigned-byte 8))))
    ;; we need to do this to prevent certain errors getting raised
    (setf (usocket:socket-option socket :broadcast) t)
    (unwind-protect 
	 (progn 
	   (frpc-log :trace "Sending to ~A:~A" host port)
	   (send-rpc-udp socket arg-type arg 
			 :program program
			 :version version
			 :proc proc
			 :auth auth
			 :verf verf
			 :request-id request-id
             :host host :port port)
	   ;; now wait for the replies to come in
	   (frpc-log :trace "Collecting replies")
	   (collect-udp-replies socket timeout result-type))
      (usocket:socket-close socket))))
	
(defun call-rpc-udp (host arg-type arg result-type 
		     &key port program version proc auth verf request-id timeout connection)
  "Send a request to the server via UDP and await a response. Will timeout after TIMEOUT seconds."
  (let ((socket (if connection
		    connection
		    (usocket:socket-connect host port
					    :protocol :datagram
					    :element-type '(unsigned-byte 8)))))
    (unwind-protect 
	 (progn 
	   (frpc-log :trace "Sending to ~A:~A" host port)
	   (send-rpc-udp socket arg-type arg 
			 :program program
			 :version version
			 :proc proc
			 :auth auth
			 :verf verf
			 :request-id request-id)
	   ;; now wait for a reply, but only if a timeout has been supplied otherwise just exit
	   (when timeout
	     (frpc-log :trace "Waiting for reply")
	     (if (usocket:wait-for-input socket :timeout timeout :ready-only t)
		 (let ((buffer (frpc.streams:allocate-buffer)))
		   (multiple-value-bind (%buffer count remote-host remote-port) (progn (usocket:socket-receive socket buffer 65507))
		     (declare (ignore %buffer))
		     (frpc-log :trace "Received response from ~A:~A (count ~A)" remote-host remote-port count)
		     ;; sbcl bug 1426667: socket-receive on x64 windows doesn't correctly check for errors 
		     ;; workaround is to check for -1 as an unsigned int.
		     ;; Seems like there is a similar bug in the usocket Lispworks codes too
		     (when (or (= count #xffffffff) (= count -1))
		       (error "Error: recvfrom returned -1"))
		     (let ((input (frpc.streams:make-buffer-stream buffer :end count)))
;;		       (nth-value 1 (read-response input result-type))
		       (multiple-value-bind (msg res) (read-response input result-type)
			 (values res (rpc-msg-verifier msg))))))
		   (error 'rpc-timeout-error))))
      (unless connection
	(usocket:socket-close socket)))))

(defun call-rpc (arg-type arg result-type 
		 &key (host *rpc-host*) (port *rpc-port*) (program 0) (version 0) (proc 0) 
		   auth verf request-id (protocol :udp) (timeout 1) connection client)
  "Establish a connection and execute an RPC to a remote machine. Returns the value decoded by the RESULT-TYPE.
This function will block until a reply is received or the request times out.

ARG-TYPE should be either a writer function or a symbol naming a valid XTYPE. 

ARG should be a value which can be passed to the ARG-TYPE function.

RESULT-TYPE should be either a reader function or a symbol naming a valid XTYPE.

HOST and PORT name the server to send the message to.

PROGRAM, VERSION and PROC define the RPC procedure to call.

If provided, AUTH and VERF should be OPAQUE-AUTH structures, as returned from MAKE-OPAQUE-AUTH. These are used to authenticate
the request. Note: do not use these unless you understand what you are doing. 
The easy way to authenticate requests is to provide a CLIENT parameter (see below).

If provided, REQUEST-ID should be an integer specifying the message ID to use. If not provided an incrementing seqno will be used.
Generally there is no reason for users to provide this parameter.

If TIMEOUT is specified, it will be set as the RECEIVE-TIMEOUT (is using TCP) or to time to wait for UDP responses.

PROTOCOL should be :TCP, :UDP or :BROADCAST. :UDP is the default, and will block until TIMEOUT seconds for a reply 
and will raise an RPC-TIMEOUT-ERROR if it doesn't receive one. Specify TIMEOUT to NIL to return immediately and not await 
a response.
:TCP will block until a response it received.
:BROADCAST should be used for UDP broadcasts. The client will wait for up to TIMEOUT seconds and collect all the repsonses
received in that time. Note that it will return a list of (host port result) instead of just the result.

CONNECTION should be a TCP or UDP connection, as returned by RPC-CONNECT.

CLIENT should be an instance of RPC-CLIENT or its subclasses. This is the ONLY way to authenticate calls.
"
  (when client
    ;; if the client is a gss client it needs special treatment...
    ;; because it needs all the call-rpc keyword args in its client so it can do 
    ;; the initial context creation rpc, we need to fill in the missing parts
    ;; from the args provided here
    (when (typep client 'gss-client)
      (setf (rpc-client-host client) (or (rpc-client-host client) host)
	    (rpc-client-port client) (or (rpc-client-port client) port)
	    (rpc-client-protocol client) (or (rpc-client-protocol client) protocol)
	    (rpc-client-timeout client) (or (rpc-client-timeout client) timeout)
	    (rpc-client-program client) (or (rpc-client-program client) program)
	    (rpc-client-version client) (or (rpc-client-version client) version)))

    ;; when a client is provided use it to fill in authenticator and verifier
    ;; fill out the other parameters with values from the client 
    (setf host (or host (rpc-client-host client))
	  port (or port (rpc-client-port client))
	  protocol (or protocol (rpc-client-protocol client))
	  timeout (or timeout (rpc-client-timeout client))
	  program (or program (rpc-client-program client))
	  version (or version (rpc-client-version client))
	  connection (or connection (rpc-client-connection client))

	  auth (rpc-client-auth client) ;; generate an authenticator
	  verf (rpc-client-verf client))) ;; generate a verifier 

  ;; when we're doing gss security levels :integrity or :privacy we need to modify the call args
  (when (typep client 'gss-client)
    (case (gss-client-service client)
      (:integrity 
       ;; pack and checksum the argument
       (setf arg-type (lambda (stream obj)
			(write-gss-integ-res stream arg-type obj
					     (gss-client-context client)
					     (gss-client-seqno client)))
	     result-type (lambda (stream)
			   (read-gss-integ-arg stream result-type 
					       (gss-client-context client)
					       (gss-client-seqno client)))))
      (:privacy 
       ;; pack, checksum and encrypt 
       (setf arg-type (lambda (stream obj)
			(write-gss-priv-res stream arg-type obj
					    (gss-client-context client)
					    (gss-client-seqno client)))
	     result-type (lambda (stream)
			   (read-gss-priv-arg stream result-type 
					      (gss-client-context client)
					      (gss-client-seqno client)))))))
  
  (multiple-value-bind (res verf) 
      (ecase protocol
	(:tcp
	 (let ((conn (if connection 
			 connection
			 (rpc-connect host port :tcp))))
	   ;; if timeout specified then set the socket option
	   (when timeout
	     (setf (usocket:socket-option conn :receive-timeout) timeout))
	   
	   (unwind-protect 
		(call-rpc-server conn arg-type arg result-type 
				 :request-id request-id
				 :program program
				 :version version
				 :proc proc
				 :auth auth
				 :verf verf)
	     (unless connection 
	       (rpc-close conn)))))
	(:udp
	 (call-rpc-udp host arg-type arg result-type
		       :port port
		       :request-id request-id
		       :program program
		       :version version
		       :proc proc
		       :auth auth
		       :verf verf
		       :timeout timeout
		       :connection connection))
	(:broadcast
	 (broadcast-rpc arg-type arg result-type
			:host host
			:port port
			:program program
			:version version
			:proc proc
			:auth auth
			:verf verf
			:request-id request-id
			:timeout timeout)))

    ;; verify the response -- this will error if verification fails 
    (when client 
      (verify client verf))

    ;; return the RPC result 
    res))

;; FIXME: it would be nice to have some simple way of providing default values for
;; the arguments. in many cases you typically want to be using the same host, port, protocol 
;; etc. and it would be nice to have defaults used rathe rthan specifying them each every time.
;; maybe we should just bind specials?
(defmacro defrpc (name proc arg-type result-type &rest options)
  "Declare an RPC interface and define a client function that invokes CALL-RPC. This must be defined before a partner DEFHANDLER form.

NAME should be a symbol naming the client function to be defined.

PROC should be an integer or constant form.

ARG-TYPE and RESULT-TYPE should be XDR type specification forms.

By default the generated function will accept a single argument which must match the type specifed by the ARG-TYPE. 

OPTIONS allow customization of the generated client function:

\(:arg-transformer lambda-list &body body\) makes it possible to augment the default function parameters before passing them to CALL-RPC. The body should return a value matching the type specified by ARG-TYPE.

\(:transformer (var) &body body\) runs after CALL-RPC has returned with VAR bound to the result. This makes it possible to destructure the result object.

\(:documentation doc-string\) specifies the docu-string for the client function.

\(:handler function-designator\) specifies a server handler for the rpc. It should designate a function of a single parameter.
"
  (alexandria:with-gensyms (gprogram gversion gproc)
    (let ((arg-reader (alexandria:symbolicate '%read- name '-arg))
	  (arg-writer (alexandria:symbolicate '%write- name '-arg))
	  (res-reader (alexandria:symbolicate '%read- name '-res))
	  (res-writer (alexandria:symbolicate '%write- name '-res)))
    `(let ((,gprogram ,*rpc-program*)
	   (,gversion ,*rpc-version*)
	   (,gproc ,proc))

       ;; define the serializers for the client 
       (defwriter ,arg-writer ,arg-type)
       (defreader ,res-reader ,result-type)
       ;; for the server
       (defreader ,arg-reader ,arg-type)
       (defwriter ,res-writer ,result-type)

       ;; define a function to call it
       (defun ,name (,@(cond
			((eq arg-type :void)
			 '(&key))
			((assoc :arg-transformer options)
			 (destructuring-bind (params &body body) (cdr (assoc :arg-transformer options))
			   (declare (ignore body))
			   (if (member '&key params)
			       params
			       `(,@params &key))))
			(t 
			 '(arg &key)))
		     (host ,*default-rpc-host*) (port ,*default-rpc-port*) auth verf request-id (protocol :udp) (timeout 1) connection client)
	 ,@(when (assoc :documentation options) (cdr (assoc :documentation options)))
	 ,(let ((the-form `(call-rpc (function ,arg-writer)
                                     ,(cond
                                       ((eq arg-type :void) 
                                        'nil)
                                       ((assoc :arg-transformer options)
                                        (destructuring-bind (params &body body) (cdr (assoc :arg-transformer options))
                                          (declare (ignore params))
                                          `(progn ,@body)))
                                       (t 'arg))
                                     (function ,res-reader)
                                     :host host
                                     :port port
                                     :program ,gprogram
                                     :version ,gversion
                                     :proc ,gproc
                                     :auth auth
                                     :verf verf
                                     :request-id request-id
                                     :protocol protocol
                                     :timeout timeout
				     :connection connection
				     :client client))
		(transformer (assoc :transformer options))
		(gtrafo (gensym "TRAFO"))
		(gcall (gensym "CALL")))
	       (if transformer	       
		   (destructuring-bind ((var) &body body) (cdr transformer)			 
		     `(flet ((,gcall () ,the-form)
			     (,gtrafo (,var) ,@body))
			(case protocol
			  (:broadcast (mapcar (lambda (b)
						(destructuring-bind (host port val) b
						  (list host port (,gtrafo val))))
					      (,gcall)))
			  (otherwise (,gtrafo (,gcall))))))
		   the-form)))

       ;; define a server handler if required
       ,@(let ((handler(cadr (assoc :handler options))))
	   (when handler
	     `((%defhandler ,gprogram ,gversion ,gproc 
			    (function ,arg-reader) 
			    (function ,res-writer) 
			    ,handler))))))))

