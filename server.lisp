;;;; Copyright (c) Frank James 2015 <frank.a.james@gmail.com>
;;;; This code is licensed under the MIT license.


(in-package #:frpc)

;; ------------- server handlers ---------------------

;; stores an assoc list for each program id
;; each program id stores an alist of version ids
;; each version id stores an aslist of handlers
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
  (let ((p (assoc program *handlers*)))
    (if (and p version)
	(let ((v (assoc version (cdr p))))
	  (if (and v proc)
	      (cdr (assoc proc (cdr v)))
	      (cdr v)))
	(cdr p))))
				     
(defmacro defhandler (name (var &key (program '*rpc-program*) (version '*rpc-version*)) proc &body body)
  (alexandria:with-gensyms (gprogram gversion gproc gh gha garg-type gres-type)
    `(let* ((,gprogram ,program)
	    (,gversion ,version)
	    (,gproc ,proc)
	    (,gh (find-handler ,gprogram ,version ,gproc)))
       (unless ,gh
	 (error "RPC ~A:~A:~A not yet declared. DEFRPC first!" 
		,gprogram ,version ,gproc))
       (destructuring-bind (,garg-type ,gres-type ,gha) ,gh
	 (declare (ignore ,gha))
	 (defun ,name (,var)
	   ,@body)
	 (%defhandler ,gprogram ,gversion ,gproc ,garg-type ,gres-type (function ,name))))))

;; ------------------------- handle a request from the stream --------------


(defun handle-request (stream &optional output)
;;  (info "Handling request")
  (handler-case 
      (let ((msg (%read-rpc-msg stream)))
	(info "Recieved message ID ~A" (rpc-msg-xid msg))
	;; validate the message
	(cond
	  ((not (eq (xunion-tag (rpc-msg-body msg)) :call))
	   ;; not a call
	   (info "Bad request: not a call")
	   (%write-rpc-msg (or output stream)
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
		(info "No handler registered")
		(%write-rpc-msg (or output stream)
				(make-rpc-response :accept :prog-mismatch
						   :id (rpc-msg-xid msg))))
	       (t 
		(destructuring-bind (arg-type res-type handler) h
		  (handler-case 
		      (let ((arg (read-xtype arg-type stream)))
			(info "Passing arg to handler")
			(let ((res (funcall handler arg)))
			  (info "Call successful")
			  (%write-rpc-msg (or output stream)
					  (make-rpc-response :accept :success
							     :id (rpc-msg-xid msg)))
			  (write-xtype res-type (or output stream) res)))
		    (error (e)
		      (info "Error handling: ~A" e) 
		      (%write-rpc-msg (or output stream)
				      (make-rpc-response :accept :rpc-mismatch
							 :id (rpc-msg-xid msg))))))))))))
    (error (e)
      (info "Error reading msg: ~A" e)
      (write-xtype 'rpc-msg 
		   (or output stream)
		   (make-rpc-response :reject :rpc-mismatch))))
;;  (info "Flushing output")
  (force-output (or output stream))
  (info "Finished request"))

;; ------------- socket server ----------

(defparameter *server-thread* nil)
(defparameter *server-exiting* nil)

(defun run-rpc-server (port &key (timeout 1))
  (let ((socket (usocket:socket-listen usocket:*wildcard-host* port
				       :reuse-address t  
				       :element-type '(unsigned-byte 8))))
    (unwind-protect 
	 (loop 
	    (progn
	      (when *server-exiting* (return-from run-rpc-server))
	      ;; wait for connection
;;	      (info "Waiting for connection")
	      (when (usocket:wait-for-input socket :timeout timeout :ready-only t)
		(when *server-exiting* (return-from run-rpc-server))
;;		(info "Socket ready to connect")
		(let ((conn (usocket:socket-accept socket)))
		  (info "Connected to ~A:~A" (usocket:get-peer-address conn) (usocket:get-peer-port conn))
		  (handler-case (handle-request (usocket:socket-stream conn) 
						(usocket:socket-stream conn))
		    (error (e)
		      (info "Error: ~A" e)))
		  (usocket:socket-close conn)))))
      (usocket:socket-close socket))))

(defun start-rpc-server (port)
  (unless *server-thread*
    (setf *server-exiting* nil
	  *server-thread*
	  (bt:make-thread (lambda () 
			    (catch 'terminate-server (run-rpc-server port))
			    (info "Server thread terminated."))
			  :name "RPC-SERVER"))))


(defun stop-rpc-server ()
  (when *server-thread*
    (setf *server-exiting* t)
    (bt:interrupt-thread *server-thread*
			 (lambda () 
			   (info "Terminating server thread")
			   (throw 'terminate-server nil)))
    (bt:join-thread *server-thread*)
    (setf *server-thread* nil)))


;; ---------------------

;; for testing/experimenting purposes

(defmacro with-local-stream (&rest type-forms)
  `(flexi-streams:with-input-from-sequence 
       (input (flexi-streams:with-output-to-sequence (output)
		,@(mapcar (lambda (type-form)
			    `(write-xtype ',(first type-form) output ,(second type-form)))
			  type-forms)))
     (list ,@(mapcar (lambda (type-form)
		       `(read-xtype ',(first type-form) input))
		     type-forms))))

(defmacro with-local-server ((arg-type res-type &key (program 0) (version 0) (proc 0)) &body body)
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
