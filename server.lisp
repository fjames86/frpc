

(in-package #:frpc)

(defun handle-request (stream)
  (handler-case 
      (let ((msg (%read-rpc-msg stream)))
	;; validate the message
	(cond
	  ((not (eq (xunion-tag (rpc-msg-body msg)) :call))
	   ;; not a call
	   (%write-rpc-msg stream
			   (make-rpc-response :reject :rpc-mismatch
					      :id (rpc-msg-xid msg))))
	  (t 
	   (let* ((call (xunion-val (rpc-msg-body msg)))
		  (h (find-handler (call-body-prog call)
				   (call-body-vers call)
				   (call-body-proc call))))
	     (cond
	       ((not h)
		;; no handler registered
		(%write-rpc-msg stream
				(make-rpc-response :accept :proj-mismatch
						   :id (rpc-msg-xid msg))))
	       (t 
		(destructuring-bind (arg-type res-type handler) h
		  (handler-case 
		      (let ((arg (read-xtype arg-type stream)))
			(let ((res (funcall handler arg)))
			  (%write-rpc-msg stream 
					  (make-rpc-response :accept :success
							     :id (rpc-msg-xid msg)))
			  (write-xtype res-type stream res)))
		    (error ()
		      (%write-rpc-msg stream (make-rpc-response :accept :rpc-mismatch
								:id (rpc-msg-xid msg))))))))))))
    (error ()
      (write-xtype 'rpc-msg stream
		   (make-rpc-response :reject :rpc-mismatch)))))

				     
(defmacro defhandler (name (var &key (program '*rpc-program*) (version '*rpc-version*)) proc &body body)
  (alexandria:with-gensyms (gprogram gversion gproc gh gha garg-type gres-type)
    `(let* ((,gprogram ,program)
	    (,gversion ,version)
	    (,gproc ,proc)
	    (,gh (find-handler ,gprogram ,version ,gproc)))
       (unless ,gh
	 (error "RPC ~A:~A:~A not yet declared. DEFRPC first!" ,gprogram ,version ,gproc))
       (destructuring-bind (,garg-type ,gres-type ,gha) ,gh
	 (declare (ignore ,gha))
	 (defun ,name (,var)
	   ,@body)
	 (%defhandler ,gprogram ,gversion ,gproc ,garg-type ,gres-type (function ,name))))))

(defun run-rpc-server (port)
  (let ((socket (usocket:socket-listen usocket:*wildcard-host* port
				       :reuse-address t
				       :element-type '(unsigned-byte 8))))
    (unwind-protect 
	 (loop 
	    (let ((conn (usocket:socket-accept socket)))
	      (unwind-protect (handle-request (usocket:socket-stream conn))
		(usocket:socket-close conn))))
      (usocket:socket-close socket))))

(defparameter *server-thread* nil)

(defun start-rpc-server (port)
  (setf *server-thread*
	(bt:make-thread (lambda () (catch 'terminate-server (run-rpc-server port)))
			:name "RPC-SERVER")))

(defun stop-rpc-server ()
  (bt:interrupt-thread *server-thread*
		       (lambda () (throw 'terminate-server nil))))

