
(in-package #:frpc)

;; UDP servers need to work differently from TCP because we need 
;; run one on both the client and server, so that the client can
;; listen for respones
;; So whereas the TCP server needs only handle :CALL messages (and
;; rejects :REPLY type messages), the UDP server needs to handle both.
;; The question is, how should it handle the replies? There are typically
;; 2 approaches:
;; 1. store the replies in a queue and signal an event to any clients
;; which are waiting. 
;; 2. Clients register a callback which gets executed with the reply
;; when it arrives.
;; I think the event-bases optioin is preferrable (option 1).

;;
;; There is a major difference in how clients decode the result with UDP requests.
;; Whereas with TCP the client is given a stream, with UDP we only get a buffer.
;; Maybe it's best for the clients to be given only an opaque buffer as a reply,
;; and it is up to them to parse it. the DEFRPC macro already is able to define
;; a result reader fucntion, so it wouldn't be too much extra work to add this.

(defstruct (udp-rpc-server (:include rpc-server)
			   (:constructor %make-udp-rpc-server))
  (lock (bt:make-lock))
  (condv (bt:make-condition-variable))
  replies)

(defun make-udp-rpc-server (&optional programs)
  (%make-udp-rpc-server :programs programs))

(defun enqueue-reply (server id reply)
  "Enqueue a reply and signal to any waiting threads that there is a new 
message to recieve. The reply should be an array of octets."
  (declare (type udp-rpc-server server)
	   (type integer id))
  (bt:with-lock-held ((udp-rpc-server-lock server))
    (push (cons id reply) 
	  (udp-rpc-server-replies server))
    (bt:condition-notify (udp-rpc-server-condv server))))

(defun %get-reply (server id &optional (dispose t))
  "Finds the message with ID, but leaves it in the queue. Removes from the queue if DISPOSE is T.
You MUST hold the server lock when calling this function." 
  (declare (type udp-rpc-server server)
	   (type integer id))
  (let ((reply (assoc id (udp-rpc-server-replies server) :test #'=)))
    (cond
      ((not reply) (values nil nil))
      (dispose (setf (udp-rpc-server-replies server)
		     (remove reply (udp-rpc-server-replies server)))
	       (values (cdr reply) t))
      (t (values (cdr reply) t)))))

(defun wait-for-reply (server id)
  "Blocks until a reply for message ID is recieved."
  (declare (type udp-rpc-server server)
	   (type integer id))	   
  (do ((res nil)
       (done nil))
      (done res)
    (bt:with-lock-held ((udp-rpc-server-lock server))
      (multiple-value-bind (reply found) (%get-reply server id)
	(when found 
	  (setf res reply
		done t)))
      (bt:condition-wait (udp-rpc-server-condv server) (udp-rpc-server-lock server)))))

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

(defun process-request (server input-stream &key program version proc id)
  "Returns a packed buffer containing the response to send back to the caller."
  (let ((h (find-handler program version proc)))
    (cond
      ((and (rpc-server-programs server)
	    (not (member program (rpc-server-programs server))))
       ;; not in allowed programs list
       (info "Program ~A not in program list" program)
       (pack #'%write-rpc-msg 
	     (make-rpc-response :accept :prog-mismatch
			      :id id)))
      ((not h)
       ;; no handler
       (info "No handler registered for ~A:~A:~A" program version proc)
       (pack #'%write-rpc-msg
	     (make-rpc-response :accept :prog-mismatch
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
  (info "Buffer ~S" buffer)
  (flexi-streams:with-input-from-sequence (input buffer)
    (let ((msg (%read-rpc-msg input)))
      (ecase (xunion-tag (rpc-msg-body msg))
	(:reply
	 ;; is a reply -- read the rest of the stream and enqueue 
	 ;; to the replies list
	 (info "Recieved reply from ~A:~A" remote-host reply-port)
	 (enqueue-reply server (rpc-msg-xid msg) (%read-until-eof input)))
	(:call 
	 ;; is a call -- lookup the proc handler and send a reply 
	 (info "Recived call from ~A:~A" remote-host reply-port)
	 (let ((call (xunion-val (rpc-msg-body msg))))
	   (let ((return-buffer (process-request server input
						 :program (call-body-prog call) 
						 :version (call-body-vers call) 
						 :proc (call-body-proc call)
						 :id (rpc-msg-xid msg))))
	     (let ((socket (usocket:socket-connect remote-host reply-port
						   :protocol :datagram
						   :element-type '(unsigned-byte 8))))
	       (info "Sending reply")
	       (usocket:socket-send socket return-buffer (length return-buffer))
	       (usocket:socket-close socket)))))))))

(defun run-udp-server (server listen-port &optional reply-port)
  "Run the UDP server until server exiting flag is set."
  (unless reply-port (setf reply-port listen-port))
  (let ((socket (usocket:socket-connect nil 0
					:protocol :datagram
					:element-type '(unsigned-byte 8)
					:timeout 1 ;; put a read timeout on the socket so that we effectively poll
					:local-port listen-port)))
    (unwind-protect
	 (do ()
	     ((rpc-server-exiting server))
	   (info "Polling UDP")
	   (when (usocket:wait-for-input socket :timeout 1 :ready-only t)
	     (multiple-value-bind (buffer length remote-host remote-port) (usocket:socket-receive socket nil 65507)	    
	       (info "Buffer: ~A length: ~A" (if buffer (type-of buffer) nil) length)
	       (when buffer 
		 (info "Recieved msg from ~A:~A" remote-host remote-port)
		 (handler-case 
		     (handle-udp-request server (subseq buffer 0 length)
					 :remote-host remote-host
					 :reply-port reply-port)
		   (error (e)
		     (info "Error handling: ~S" e)))))))
      (usocket:socket-close socket))))

;; server framework

(defmethod start-rpc-server ((server udp-rpc-server) &key (port *rpc-port*))
  "Start example UDP server"
  (setf (rpc-server-thread server)
	(bt:make-thread (lambda ()
			  (run-udp-server server port))
			:name (format nil "rpc-server-thread UDP port ~A" port)))
  server)

(defmethod stop-rpc-server ((server udp-rpc-server))
  "Stop example UDP server"
  (setf (rpc-server-exiting server) t)
  (bt:join-thread (rpc-server-thread server))
  nil)

