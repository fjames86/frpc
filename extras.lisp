;;;; Copyright (c) Frank James 2015 <frank.a.james@gmail.com>
;;;; This code is licensed under the MIT license.

;;; This file contains extra bits and pieces of frpc that require access to the rpcbind functions.

(in-package #:frpc)

;; need to put this here because it requires the rpcbind calls 
(defmethod initialize-instance :after ((client rpc-client) &rest initargs &key)
  (declare (ignore initargs))
  (let ((program (rpc-client-program client))
        (host (rpc-client-host client)))

    ;; if the program is named by a symbol/string then find its number or fail
    (unless (or (null program) (integerp program))
      (let ((p (find-program program)))
        (if p 
            (setf program (second p)
                  (rpc-client-program client) (second p))
            (error "Unknown program ~S" program))))

    (when (and host program)
      (let ((port (frpc.bind:call-get-port program 
                                           :query-protocol (rpc-client-protocol client)
                                           :host host
                                           :timeout (or (rpc-client-timeout client) 1)
                                           :connection (rpc-client-connection client))))
        (cond
          ((zerop port) 
           (error "Program ~A not mapped by port mapper" program))
          ((and (rpc-client-port client)
                (not (= port (rpc-client-port client))))
           (setf (rpc-client-port client) port))
          (t 
           (setf (rpc-client-port client) port)))))))


