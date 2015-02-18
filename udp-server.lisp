
(in-package #:frpc)

(defstruct (udp-rpc-server :include rpc-server)
  (lock (bt:make-lock))
  (condv (bt:make-condition-variable))
  replies)

(defun enqueue-reply (server id reply)
  (declare (type udp-rpc-server server)
	   (type integer id))
  (bt:with-lock-held ((udp-rpc-server-lock server))
    (push (cons id reply) 
	  (udp-rpc-server-replies server))
    (bt:condition-notify (udp-rpc-server-condv server))))

(defun wait-for-reply (server id)
  (declare (type udp-rpc-server server)
	   (type integer id))	   
  (do ((res nil)
       (done nil))
      (done res)
    (bt:with-lock-held ((udp-rpc-server-lock server))
      (bt:condition-wait (udp-rpc-server-condv server) (udp-rpc-server-lock server))
      (let ((reply (first (udp-rpc-server-replies server))))
	(when (= (car reply) id)
	  (setf res (cdr (pop (udp-rpc-server-replies server)))
		done t))))))

(defun get-reply (server id)
  (declare (type udp-rpc-server server)
	   (type integer id))
  (bt:with-lock-held ((udp-rpc-server-lock server))
    (let ((reply (assoc id (udp-rpc-server-replies server) :test #'=)))
      (if reply
	  (values (cdr reply) t)
	  (values nil nil)))))


(defun handle-udp-reply (stream msg)
  nil)

(defun handle-udp-request ()
  nil)

(defun run-udp-server (server listen-port &optional reply-port)
  (let ((socket (usocket:socket-connect nil 0
					:protocol :datagram
					:element-type '(unsigned-byte 8)
					:timeout 1 ;; put a read timeout on the socket so that we effectively poll
					:local-port listen-port)))
    (unwind-protect
	 (do ()
	     ((rpc-server-exiting server))
	   (info "Polling UDP")
	   (multiple-value-bind (buffer length remote-host remote-port) (usocket:socket-receive socket nil nil)	    
	     (declare (ignore length))
	     (when buffer 
	       (info "Remote: ~A:~A" remote-host remote-port)
	       (handler-case 
		   (let ((return-buffer 
			  (flexi-streams:with-output-to-sequence (output)
			    (flexi-streams:with-input-from-sequence (input buffer)
			      (handle-request input output (rpc-server-programs server))))))
		     ;; send the return buffer back to the host 
		     ;; FIXME: what to do if we recieve a :REPLY message instead of a :CALL?
		     ;; we need to get the reply value and put it away somewhere
		     (info "Replying to ~A:~A" remote-host (or reply-port listen-port))
		     (let ((rsock (usocket:socket-connect remote-host (or reply-port listen-port) ;;remote-port)
							  :protocol :datagram
							  :element-type '(unsigned-byte 8))))		   
		       (unwind-protect (usocket:socket-send rsock return-buffer (length return-buffer))		     
			 (usocket:socket-close rsock))))
		 (error (e)
		   (info "Error handling: ~A" e))))))
      (usocket:socket-close socket))))

;; server framework
(defun start-udp-server (port &key programs)
  "Start example UDP server"
  (let ((server (make-rpc-server :programs programs)))
    (setf (rpc-server-thread server)
	  (bt:make-thread (lambda ()
			    (run-udp-server server port))
			  :name (format nil "rpc-server-thread UDP port ~A" port)))
    server))

(defun stop-udp-server (server)
  "Stop example UDP server"
  (setf (rpc-server-exiting server) t)
  (bt:join-thread (rpc-server-thread server)))

