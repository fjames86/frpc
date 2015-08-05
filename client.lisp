;;;; Copyright (c) Frank James 2015 <frank.a.james@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package #:frpc)

(defvar *rpc-remote-host* nil)
(defvar *rpc-remote-port* nil)
(defvar *rpc-remote-protocol* nil)
(defvar *rpc-remote-auth* nil)


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
	      (error 'rpc-prog-mismatch-error 
		     :low (getf a :low)
		     :high (getf a :high)
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

(defparameter *default-rpc-host* '*rpc-host*)
(defparameter *default-rpc-port* '*rpc-port*)

(defmacro use-rpc-host (host port)
  `(eval-when (:load-toplevel :compile-toplevel :execute)
     (setf *default-rpc-host* ,host
	   *default-rpc-port* ,port)))

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
       ;; some implementations return host addresses as htol integers, but octet vectors are more useful
       (when (integerp remote-host)
         (setf remote-host 
               (let ((v (nibbles:make-octet-vector 4)))
                 (setf (nibbles:ub32ref/be v 0) remote-host)
                 v)))

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
                    (list remote-host nil))
                   ((not (eq (xunion-tag (accepted-reply-reply-data (xunion-val (xunion-val body))))
                             :success))
                    ;; rpc unsuccessful e.g. prog-unavail
                    (list remote-host nil))
                   (t 
                    ;; FIXME: should really check the reply's id
                    (push (list remote-host (read-xtype result-type input))
                          replies)))))
           (error (e)
             (frpc-log :error "~A" e)
             (frpc-log :error "~S" (subseq buffer 0 count))
             (list remote-host nil))))))))))
    
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
    (setf host (rpc-client-host client)
	  port (rpc-client-port client)
	  protocol (rpc-client-protocol client)
	  timeout (rpc-client-timeout client)
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
       (setf arg-type (let ((w arg-type))
                        (lambda (stream obj)
                          (write-gss-integ stream w obj
                                           (gss-client-context client)
                                           (gss-client-seqno client))))
             result-type (let ((r result-type))
                           (lambda (stream)
                             (read-gss-integ stream r
                                             (gss-client-context client)
                                             (gss-client-seqno client))))))
      (:privacy 
       ;; pack, checksum and encrypt 
       (setf arg-type (let ((a arg-type))
                        (lambda (stream obj)
                          (write-gss-priv stream a obj
                                          (gss-client-context client)
                                          (gss-client-seqno client))))
             result-type (let ((r result-type))
                           (lambda (stream)
                             (read-gss-priv stream r
                                            (gss-client-context client)
                                            (gss-client-seqno client))))))))
  
  ;; touch up the protocol if a connection is provided 
  (when connection
    (setf protocol
          (etypecase connection
            (usocket:datagram-usocket :udp)
            (usocket:stream-usocket :tcp))))

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

\(:program program version\) PROGRAM and VERSION name the program name/number and the version number.

\(:arg-transformer lambda-list &body body\) makes it possible to augment the default function parameters before passing them to CALL-RPC. The body should return a value matching the type specified by ARG-TYPE.

\(:transformer (var) &body body\) runs after CALL-RPC has returned with VAR bound to the result. This makes it possible to destructure the result object.

\(:documentation doc-string\) specifies the docu-string for the client function.

\(:handler function-designator\) specifies a server handler for the rpc. It should designate a function of a single parameter.
"
  ;; check a program has been provided, otherwise signal a style warning
  (unless (assoc :program options)
    (alexandria:simple-style-warning "Implicit program identifier is DEPRECATED. Use a (:program progname version) option."))

  (alexandria:with-gensyms (gprogram gversion gproc)
    (let ((arg-reader (alexandria:symbolicate '%read- name '-arg))
	  (arg-writer (alexandria:symbolicate '%write- name '-arg))
	  (res-reader (alexandria:symbolicate '%read- name '-res))
	  (res-writer (alexandria:symbolicate '%write- name '-res)))
    `(let ((,gprogram ,(or (cadr (find-program (cadr (assoc :program options)))) 
			   *rpc-program*))
	   (,gversion ,(or (caddr (assoc :program options))
			   *rpc-version*))
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
		     (host ,*default-rpc-host*) (port ,*default-rpc-port*) 
;;                      auth verf request-id  ;; does anyone ever use these? authentication should only be done using a client
                      (protocol :udp) (timeout 1) connection client)
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
;;                                     :auth auth
;;                                     :verf verf
;;                                     :request-id request-id
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
       ,@(let ((handler (cadr (assoc :handler options))))
	   (when handler
	     `((%defhandler ,gprogram ,gversion ,gproc 
			    (function ,arg-reader) 
			    (function ,res-writer) 
			    ,handler))))))))


;; -------------- handlers -----------------

;; stores an assoc list for each program id
;; each program id stores an alist of version ids
;; each version id stores an alist of handler lists
;; a handler list is (ARG-READER RES-WRITER HANDLER)
(defvar *handlers* nil)

(defun %defhandler (program version proc arg-type res-type handler)
  ;; lookup the program. allow it to be specified by symbol as well as by number.
  (let ((pg (find-program program)))
    (if pg (setf program (cadr pg))))

  (let ((p (assoc program *handlers*)))
    (if p
        (let ((v (assoc version (cdr p))))
          (if v
              (let ((c (assoc proc (cdr v))))
                (if c
                    (destructuring-bind (old-arg-type old-res-type old-handler) (cdr c)
                      (setf (cdr c) (list (or arg-type old-arg-type)
                                          (or res-type old-res-type)
                                          (or handler old-handler)))
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

;; WARNING: this macro seems nice to have but it's generally not a good idea to use it.
;; I've got it here because there is a chance that you might want to write server handlers
;; which implicitly depend on the client calls provided by defrpc. That means you can't 
;; specify the real handler in the defrpc form. This allows you to define it at some stage after.
(defmacro defhandler (name (var program version proc) &body body)
  "Define a server handler for the procedure named by PROGRAM,VERSION,PROC."
  `(progn
     (defun ,name (,var) ,@body)
     (%defhandler ',program ,version ,proc
                  nil nil #',name)))

;;------------------------------------------------------------------------

(defun send-rpc (client arg-type arg proc)
  "Send an RPC request and return immediately. Does not wait for a reply.
Responses should be collected with calls to RECEIVE-RPC.

CLIENT ::= an RPC-CLIENT instance. 
The client MUST have its PROGRAM, VERSION and CONNECTION slots set.

ARG-TYPE ::= an XDR writer function or a symbol naming an XDR type.
ARG ::= object to feed to the XDR writer function.
PROC ::= integer naming the procedure to call.

Returns the XID for the request sent."
  (declare (type rpc-client client)
	   (type integer proc)
	   (type (or function symbol) arg-type))
  (let ((auth (rpc-client-auth client))
	(verf (rpc-client-verf client))
	(program (rpc-client-program client))
	(version (rpc-client-version client))
	(connection (rpc-client-connection client)))
    (unless program (error "Client must have RPC program set."))
    (unless version (error "Client must have RPC version set."))
    (unless connection (error "Client must have RPC connection set."))

    ;; when we're doing gss security levels :integrity or :privacy we need to modify the call args
    (when (typep client 'gss-client)
      (case (gss-client-service client)
	(:integrity 
	 ;; pack and checksum the argument
	 (setf arg-type (let ((w arg-type))
			  (lambda (stream obj)
			    (write-gss-integ stream w obj
					     (gss-client-context client)
					     (gss-client-seqno client))))))
	(:privacy 
	 ;; pack, checksum and encrypt 
	 (setf arg-type (let ((a arg-type))
			  (lambda (stream obj)
			    (write-gss-priv stream a obj
					    (gss-client-context client)
					    (gss-client-seqno client))))))))

    (let* ((buffer (frpc.streams:allocate-buffer))
	   (msg (make-rpc-request program proc 
				  :version version
				  :auth auth
				  :verf verf))
	   (count (frpc.streams:with-buffer-stream (stream buffer)
		    (write-request stream
				   msg
				   arg-type
				   arg))))
      (etypecase connection
	(usocket:datagram-usocket
	 (usocket:socket-send connection buffer count))
	(usocket:stream-usocket
	 (write-sequence buffer (usocket:socket-stream connection) :end count)))
      (rpc-msg-xid msg))))

(defun receive-rpc (client res-type)
  "Waits for an RPC reply. If the TIMEOUT slot of the client is set then will wait
for TIMEOUT seconds, otherwise waits indefinitely.

CLIENT ::= an RPC-CLIENT instance. 
The client MUST have its CONNECTION slot set.

RES-TYPE ::= reader function or symbol naming an XDR type.

Returns (values result xid) where XID is the transaction ID of the 
reply received. This should be used to match up replies with requests
sent from SEND-RPC.
"
  (declare (type rpc-client client)
	   (type (or function symbol) res-type))
  (let ((socket (rpc-client-connection client))
	(timeout (rpc-client-timeout client)))
    (unless socket (error "Client must have RPC connection set."))
    (let ((buffer nil))
      (when (usocket:wait-for-input socket :timeout timeout :ready-only t)
	(etypecase socket 
	  (usocket:datagram-usocket
	   (setf buffer (frpc.streams:allocate-buffer))
	   (multiple-value-bind (%buffer count remote-host remote-port) (usocket:socket-receive socket buffer (length buffer))
	     (declare (ignore %buffer count remote-host remote-port))))
	  (usocket:stream-usocket
	   (setf buffer (read-fragmented-message (usocket:socket-stream socket)))))

	;; when we're doing gss security levels :integrity or :privacy we need to modify the result
	(when (typep client 'gss-client)
	  (case (gss-client-service client)
	    (:integrity 
	     ;; pack and checksum the argument
	     (setf res-type (let ((r res-type))
				 (lambda (stream)
				   (read-gss-integ stream r
						   (gss-client-context client)
						   (gss-client-seqno client))))))
	    (:privacy 
	     ;; pack, checksum and encrypt 
	     (setf res-type (let ((r res-type))
				 (lambda (stream)
				   (read-gss-priv stream r
						  (gss-client-context client)
						  (gss-client-seqno client))))))))

	(let (res msg)
	  (frpc.streams:with-buffer-stream (stream buffer)
	    (multiple-value-bind (%msg verf) (read-response stream res-type)
	      (declare (ignore verf))
	      (setf res (read-xtype res-type stream)
		    msg %msg)))
	  (values res (rpc-msg-xid msg)))))))
  

