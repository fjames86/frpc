;;;; Copyright (c) Frank James 2015 <frank.a.james@gmail.com>
;;;; This code is licensed under the MIT license.

;;; This file contains extra bits and pieces of frpc that require access to the rpcbind functions.

(in-package #:frpc)

;; need to put this here because it requires the rpcbind calls 
(defmethod initialize-instance :after ((client rpc-client) &rest initargs &key)
  (declare (ignore initargs))
  (let ((program (rpc-client-program client))
        (version (rpc-client-version client))
        (host (rpc-client-host client)))
    (when (and host program)
      (let ((port (frpc.bind:call-get-port program 
                                           (or version 0)
                                           :query-protocol (rpc-client-protocol client)
                                           :host host
                                           :timeout (or (rpc-client-timeout client) 1)
                                           :connection (rpc-client-connection client))))
        (cond
          ((zerop port) 
           (error "Program ~A.~A not mapped by remote port mapper" 
                  program version))
          ((and (rpc-client-port client)
                (not (= port (rpc-client-port client))))
           (error "Program ~A.~A not mapped to specified port ~A" program version (rpc-client-port client)))
          (t 
           (setf (rpc-client-port client) port)))))))


