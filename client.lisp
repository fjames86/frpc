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
	     (:auth-error (error 'rpc-auth-error :stat (xunion-val denied)))
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

(defmacro use-rpc-port (port)
  `(eval-when (:load-toplevel :compile-toplevel :execute)
     (setf *rpc-port* ,port)))


(defun rpc-connect (host &optional port)
  "Establish a TCP connection to the rpc server."
  (usocket:socket-connect host (or port *rpc-port*)
			  :element-type '(unsigned-byte 8)))

(defun rpc-close (conn)
  "Close the TCP connection to the server."
  (usocket:socket-close conn))
	    
(defmacro with-rpc-connection ((var host &optional port) &body body)
  "Execute the body in the context of a TCP connection."
  `(let ((,var (rpc-connect ,host ,port)))
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
      (nth-value 1 (read-response input result-type)))))

(defun collect-udp-replies (socket timeout result-type)
  "Wait TIMEOUT seconds, collecting as many UDP replies as arrive in that time."
  (do ((replies nil)
       (buffer (nibbles:make-octet-vector 65507))
       (start (get-universal-time))
       (now (get-universal-time) (get-universal-time)))
      ((> (- now start) timeout) replies)
    (when (usocket:wait-for-input socket :timeout (- timeout (- now start)) :ready-only t)
      (multiple-value-bind (%buffer count remote-host remote-port) (usocket:socket-receive socket buffer 65507)
	(declare (ignore %buffer))
	(cond
	  ;; workaround for sbcl bug
	  ((= count #xffffffff)
	   (log:error "recvfrom returned -1"))
	  (t
	   (log:debug "Received response from ~A:~A" remote-host remote-port)
	   (flexi-streams:with-input-from-sequence (input (subseq buffer 0 count))
	     (let ((msg (%read-rpc-msg input)))
	       (log:debug "MSG ID ~A" (rpc-msg-xid msg))
	       ;; FIXME: shouild really check the reply's id
	       (push (list remote-host remote-port (read-xtype result-type input))
		     replies)))))))))
    
(defun send-rpc-udp (socket arg-type arg &key program version proc auth verf request-id)
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
    (usocket:socket-send socket buffer (length buffer))))

(defun broadcast-rpc (arg-type arg result-type 
		     &key host port program version proc auth verf request-id timeout)
  (let ((socket (usocket:socket-connect host port
					:protocol :datagram
					:element-type '(unsigned-byte 8))))
    ;; we need to do this to prevent certain errors getting raised
    (setf (usocket:socket-option socket :broadcast) t)
    (unwind-protect 
	 (progn 
	   (log:debug "Sending to ~A:~A" host port)
	   (send-rpc-udp socket arg-type arg 
			 :program program
			 :version version
			 :proc proc
			 :auth auth
			 :verf verf
			 :request-id request-id)
	   ;; now wait for the replies to come in
	   (log:debug "Collecting replies")
	   (collect-udp-replies socket timeout result-type))
      (usocket:socket-close socket))))
	
(defun call-rpc-udp (host arg-type arg result-type 
		     &key port program version proc auth verf request-id timeout)
  "Send a request to the server via UDP and await a response. Will timeout after TIMEOUT seconds."
  (let ((socket (usocket:socket-connect host port
					:protocol :datagram
					:element-type '(unsigned-byte 8))))
    (unwind-protect 
	 (progn 
	   (log:debug "Sending to ~A:~A" host port)
	   (send-rpc-udp socket arg-type arg 
			 :program program
			 :version version
			 :proc proc
			 :auth auth
			 :verf verf
			 :request-id request-id)
	   ;; now wait for a reply, but only if a timeout has been supplied 
	   ;; otherwise just exit
	   (when timeout
	     (log:debug "Waiting for reply")
	     (if (usocket:wait-for-input socket :timeout timeout :ready-only t)
		 (multiple-value-bind (buffer count remote-host remote-port) (progn (usocket:socket-receive socket nil 65507))
		   (log:debug "Received response from ~A:~A (count ~A)" remote-host remote-port count)
		   ;; sbcl bug 1426667: socket-receive on x64 windows doesn't correctly check for errors 
		   ;; workaround is to check for -1 as an unsigned int
		   (when (= count #xffffffff)
		     (error "Error: recvfrom returned -1"))
		   (flexi-streams:with-input-from-sequence (input buffer :start 0 :end count)
		     (let ((msg (%read-rpc-msg input)))
		       (log:debug "MSG ID ~A" (rpc-msg-xid msg))
		       ;; FIXME: shouild really check the reply's id
		       (read-xtype result-type input))))
		 (error 'rpc-timeout-error))))
      (usocket:socket-close socket))))

(defun call-rpc (arg-type arg result-type 
		 &key (host *rpc-host*) (port *rpc-port*) (program 0) (version 0) (proc 0) 
		   auth verf request-id protocol (timeout 1))
  "Establish a connection and execute an RPC to a remote machine. Returns the value decoded by the RESULT-TYPE.
By default a TCP connection is established and this function will block until a reply is received.

ARG-TYPE should be either a reader function or a symbol naming a valid XTYPE. 

ARG should be a value which can be passed to the ARG-TYPE function.

RESULT-TYPE should be either a writer function or a symbol naming a valid XTYPE.

HOST and PORT name the server to send the message to.

PROGRAM, VERSION and PROC define the RPC procedure.

If provided, AUTH and VERF should be OPAQUE-AUTH structures.

If provided, REQUEST-ID should be an integer specifying the message ID to use. If not provided, the current value of *rpc-msgid* will be used,
*rpc-msgid* will then be incremented.

If TIMEOUT is specified, it will be set as the RECEIVE-TIMEOUT (is using TCP) or to control waiting for UDP responses.

If TIMEOUT-ERROR is non-nil a UDP timeout will signal an error. Returns NIL otherwise.

PROTOCOL should be :TCP, :UDP or :BROADCAST. :TCP is the default, and will block until a reply is recieved.
:UDP will wait for up to TIMEOUT seconds for a reply and will raise an RPC-TIMEOUT-ERROR if it doesn't receive one.
:BROADCAST should be used for UDP broadcasts. The client will wait for up to TIMEOUT seconds and collect all the repsonses
received in that time. Note that it will return a list of (host port result) instead of just the result.
"
  (ecase protocol
    ((:tcp nil)
     (with-rpc-connection (conn host port)
       ;; if timeout specified then set the socket option
       (when timeout
	 (setf (usocket:socket-option conn :receive-timeout) timeout))
       (call-rpc-server conn arg-type arg result-type 
			:request-id request-id
			:program program
			:version version
			:proc proc
			:auth auth
			:verf verf)))
    (:udp
     (call-rpc-udp host arg-type arg result-type
		   :port port
		   :request-id request-id
		   :program program
		   :version version
		   :proc proc
		   :auth auth
		   :verf verf
		   :timeout timeout))
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
		    :timeout timeout))))

(defmacro defrpc (name proc arg-type result-type &rest options)
  "Declare an RPC interface and define a client function that invokes CALL-RPC. 
This MUST be defined before a partner DEFHANDLER form.

NAME should be a symbol naming the client function to be defined.

PROC should be an integer or constant form.

ARG-TYPE and RESULT-TYPE should be XDR type specification forms."
  (alexandria:with-gensyms (gprogram gversion gproc)
    (let ((arg-reader (alexandria:symbolicate '%read- name '-arg))
	  (arg-writer (alexandria:symbolicate '%writer- name '-arg))
	  (res-reader (alexandria:symbolicate '%read- name '-res))
	  (res-writer (alexandria:symbolicate '%writer- name '-res)))
    `(let ((,gprogram ,*rpc-program*)
	   (,gversion ,*rpc-version*)
	   (,gproc ,proc))

       ;; define the serializers
       (defreader ,arg-reader ,arg-type)
       (defwriter ,arg-writer ,arg-type)
       (defreader ,res-reader ,result-type)
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
		     (host ,*rpc-host*) (port ,*rpc-port*) auth verf request-id protocol (timeout 1))
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
				     :timeout timeout))
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

       ;; define a server handler
       (%defhandler ,gprogram ,gversion ,gproc 
		    (function ,arg-reader) 
		    (function ,res-writer) 
		    nil)))))

