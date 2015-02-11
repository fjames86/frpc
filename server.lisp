
(in-package #:frpc)

(defmacro with-rpc-server ((var port) &body body)
  (alexandria:with-gensyms (gsocket)
    `(let ((,gsocket (usocket:socket-listen usocket:*wildcard-host* port
					    :reuse-address t
					    :element-type '(unsigned-byte 8))))
       (unwind-protect 
	    (progn ,@body)
	 (usocket:socket-close ,gsocket)))))


