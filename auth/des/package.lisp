;;;; Copyright (c) Frank James 2015 <frank.a.james@gmail.com>
;;;; This code is licensed under the MIT license.

(defpackage #:frpc-des
  (:use #:cl #:frpc)
  (:export #:des-client
           #:des-init
           #:des-public
           #:des-conversation

	   #:call-null
           #:call-set
           #:call-encrypt
           #:call-decrypt
           #:call-gen
           #:call-get))


