;;;; Copyright (c) Frank James 2015 <frank.a.james@gmail.com>
;;;; This code is licensed under the MIT license.

;;; Implements the yellow-pages (NIS) protocol messages


(defpackage #:yp
  (:use #:cl #:frpc))

(in-package #:yp)

(defxenum ypstat 
  (:true 1)
  (:nomore 2)
  (:false 0)
  (:nomap -1)
  (:nodom -2)
  (:nokey -3)
  (:badop -4)
  (:baddb -5)
  (:yperr -6)
  (:badargs -7)
  (:yp-vers -8))

(defxtype* domainname () :string)
(defxtype* mapname () :string)
(defxtype* peername () :string)
(defxtype* keydat () (:varray* :octet))
(defxtype* valdat () (:varray* :octet))

(defxtype* req-key () (:list domainname mapname keydat))
(defxtype* req-nokey () (:list domainname mapname))
(defxtype* resp-val () (:list ypstat valdat))
(defxtype* resp-key-val () (:list valdat keydat))
(defxtype* resp-master () (:list ypstat peername))
(defxtype* resp-order () (:list ypstat :uint32))
(defxtype* resp-all () (:optional resp-key-val))
(defxtype* maplist () (:list mapname (:optional maplist)))
(defxtype* resp-maplist () (:list ypstat (:optional maplist)))


(use-rpc-program 100004 2)
(use-rpc-host '*rpc-host* '*rpc-port*)

(defrpc call-null 0 :void :void)
(defrpc call-domain 1 domainname :boolean
  (:documentation "Return of TRUE indicates the NIS server serves the named domain, FALSE indicates no support."))
(defrpc call-domain-nonack 2 domainname :boolean
  (:documentation "Same as CALL-DOMAIN, but does not reply if no support. Use for broadcasts"))
(defrpc call-match 3 req-key resp-val
  (:documentation "Returns the right-hand value for a passed left-hand key within a named map and domain."))
(defrpc call-first 4 req-key resp-key-val
  (:documentation "Returns the first left-value pair from a named domain and map."))
(defrpc call-next 5 req-key resp-key-val
  (:documentation "Returns the key-value pair following a passed key-value pair wthin a named domain and map."))
;;(defrpc call-xfr 6 req-xfr resp-fxr
;;  (:documentation "Indicates to a server that a map should be updated."))
(defrpc call-clear 7 :void :void
  (:documentation "Instructs an NIS server to close the current map so that old versions of the disk file don't get held open."))
(defrpc call-all 8 req-nokey resp-all)
(defrpc call-master 9 req-nokey resp-master)
(defrpc call-order 10 req-nokey resp-order)
(defrpc call-maplist 11 domainname resp-maplist)

;; ---------------------------

(defpackage #:ypbind
  (:use #:cl #:frpc))

(in-package #:ypbind)

(use-rpc-program 100007 2)

(defxenum bind-resp-type
  (:success 1)
  (:fail 2))

(defxtype* bind-binding () (:list (:varray :octet 4) (:varray :octet 2))) ;; fixme: length not multiple of 4?
(defxunion bind-resp (bind-resp-type)
  (:success bind-binding)
  (:fail :uint32))
(defxtype* bind-setdom () (:list yp::domainname bind-binding :uint32))

(defrpc call-null 0 :void :void)
(defrpc call-domain 1 yp::domainname bind-resp)
(defrpc call-setdom 2 bind-setdom :void)
