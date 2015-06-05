;;;; Copyright (c) Frank James 2015 <frank.a.james@gmail.com>
;;;; This code is licensed under the MIT license.

(defpackage #:frpc-des
  (:use #:cl #:frpc)
  (:export #:des-client
           #:des-init
           #:des-public
           #:des-conversation

	   ;; access to local database
	   #:add-public-key
	   #:remove-public-key
	   #:public-key-list
	   #:find-public-key

	   ;; RPC interface to database
           #:call-null
           #:call-get
           #:call-set
	   #:call-unset
	   #:call-list))





