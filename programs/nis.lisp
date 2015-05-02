;;;; Copyright (c) Frank James 2015 <frank.a.james@gmail.com>
;;;; This code is licensed under the MIT license.


;; http://www.revathi.in/symsssss/root/usr/include/rpcsvc/nis.x

(defpackage #:nis
  (:use #:cl #:frpc))

(in-package #:nis)

(defconstant +NIS-MAXSTRINGLEN+ 255)
(defconstant +NIS-MAXNAMELEN 1024)
(defconstant +NIS-MAXATTRNAME 32)
(defconstant +NIS-MAXATTRVAL 2048)
(defconstant +NIS-MAXCOLUMNS 64)
(defconstant +NIS-MAXATTR 16)
(defconstant +NIS-MAXPATH 1024)
(defconstant +NIS-MAXREPLICAS 128)
(defconstant +NIS-MAXLINKS 16)

(defxtype* netobj () (:varray* :octet))

(defxtype* nis-name () :string)

(defxtype* nis-attr () 
  (:plist :ndx nis-name
	  :val (:varray* :octet)))

(defxenum zotypes ()
  (:bogus 0)
  (:no-obj 1)
  (:directory 2)
  (:group 3)
  (:table 4)
  (:entry 5)
  (:link 6)
  (:private 7))

(defxenum nstype ()
  (:unknown 0)
  (:nis 1)
  (:sunyp 2)
  (:ivy 3)
  (:dns 4)
  (:x500 5)
  (:dnans 6)
  (:xchs 7)
  (:cds 8))

(defxtype* oar-mask () 
  (:plist :rights :uint64 
	  :otype zotypes))

(defxtype* endpoint ()
  (:plist uaddr :string
	  family :string
	  proto :string))

(defxstruct nis-server ()
  (name nis-name)
  (ep (:varray endpoint))
  (keytype :uint64)
  (pkey netobj))

(defxstruct directory-obj ()
  (name nis-name)
  (type nstype)
  (servers (:varray nis-server))
  (ttl :uint64)
  (armask (:varray oar-mask)))

(defxtype* entry-col ()
  (:plist :flags :uint64
	  :value (:varray* :octet)))

(defxtype* entry-obj ()
  (:plist :type :string
	  :cols (:varray entry-col)))

(defxtype* group-obj ()
  (:plist :flags :uint64
	  :members (:varray nis-name)))

(defxtype* link-obj ()
  (:plist :type zotypes
	  :attrs (:varray nis-attr)
	  :name nis-name))

(defxtype* table-col ()
  (:plist :name :string
	  :flags :uint64
	  :rights :uint64))

(defxstruct table-obj ()
  (type :string)
  (maxcol :int32)
  (sep :uint32)
  (cols (:varray table-col))
  (path :string))

(defxunion objdata (zotypes)
  (:directory-obj directory-obj)
  (:group-obj group-obj)
  (:table-obj table-obj)
  (:entry-obj entry-obj)
  (:link-obj link-obj)
  (:private-obj (:varray* :octet))
  (:no-obj :void)
  (:bogus-obj :void)
  (otherwise :void))

(defxtype* nis-oid () (:list :uint64 :uint64))

(defxstruct nis-object ()
  (oid nis-oid)
  (name nis-name)
  (owner nis-name)
  (group nis-group)
  (domain nis-name)
  (access :uint64)
  (ttl :uint64)
  (data objdata))

(defxenum nis-error ()
  (:success 0)
  (:s-success 1)
  (:notfound 2)
  (:s-notfound 3)
  (:cache-expired 4)
  (:name-unreachable 5)
  (:unknown-obj 6)
  (:try-again 7)
  (:system-error 8)
  (:chain-broken 9)
  (:permission 10)
  (:not-owner 11)
  (:not-me 12)
  (:no-memory 13)
  (:name-exists 14)
  (:not-master 15)
  (:invalid-obj 16)
  (:bad-name 17)
  (:no-callback 18)
  (:cbresults 19)
  (:no-such-name 20)
  (:not-unique 21)
  (:ib-mod-error 22)
  (:no-such-table 23)
  (:type-mismatch 24)
  (:link-name-error 25)
  (:partial 26)
  (:too-many-attrs 27)
  (:rpc-error 28)
  (:bad-attribute 29)
  (:not-searchable 30)
  (:cberror 31)
  (:foreign-ns 32)
  (:bad-object 33)
  (:not-same-obj 34)
  (:mod-fail 35)
  (:bad-request 36)
  (:not-empty 37)
  (:coldstart-err 38)
  (:resync 39)
  (:fail 40)
  (:unavail 41)
  (:resync 42)
  (:srvauth 43)
  (:client-auth 44)
  (:nofilespace 45)
  (:noproc 46)
  (:dumplater 47))

(defxstruct nis-result ()
  (status nis-error)
  (objects (:varray nis-object))
  (cookie netobj)
  (zticks :uint64)
  (dticks :uint64)
  (aticks :uint64)
  (cticks :uint64))

(defxtype* ns-request ()
  (:plist :name nis-name
	  :object (:optional nis-object)))

(defxstruct ib-request ()
  (name nis-name)
  (srch nis-attr)
  (flags :uint64)
  (obj (:optional nis-object))
  (host (:optional nis-server))
  (buffsize :uint64)
  (cookie netobj))

(defxtype* ping-args ()
  (:plist :dir nis-name
	  :stamp :uint64))

(defxenum log-entry-t
  (:log-nop 0)
  (:add-name 1)
  (:rem-name 2)
  (:mod-name-old 3)
  (:mod-name-new 4)
  (:add-ibase 5)
  (:rem-ibase 6)
  (:mod-ibase 7)
  (:upd-stamp 8))

(defxstruct log-entry ()
  (time :uint64)
  (type log-entry-t)
  (princp nis-name)
  (name nis-name)
  (attrs (:varray nis-attr))
  (object nis-object))

(defxtype* log-result ()
  (:plist :status nis-error
	  :cookie netobj
	  :entries (:varray log-entry)))

(defxtype* cp-result ()
  (:plist :status nis-error
	  :zticks :uint64
	  :dticks :uint6$))

(defxtype* nis-tag ()
  (:plist :type :uint64
	  :val :string))

(defxtype* nis-taglist () (:varray nis-tag))

(defxtype* dump-args ()
  (:plist :dir nis-name
	  :time :uint64
	  :cbhost (:optional nis-server)))

(defxtype* fd-args ()
  (:plist :name nis-name
	  :requester nis-name))

(defxstruct fd-result ()
  (status nis-error)
  (source nis-name)
  (data (:varray* :octet))
  (signature (:varray* :octet)))

;; rpc interfaace
(use-rpc-program 100300 3)

(defrpc call-null 0 :void :void)

(defrpc call-nis-lookup 1
  ns-request 
  nis-result)

(defrpc call-nis-add 2
  ns-request
  nis-result)

(defrpc call-nis-modify 3
  ns-request
  nis-result)

(defrpc call-nis-remove 4
  ns-request
  nis-result)

(defrpc call-nis-iblist 5
  ib-request
  nis-result)

(defrpc call-nis-ibadd 6
  ib-request
  nis-result)

(defrpc call-nis-ibmodify 7
  ib-request
  nis-result)

(defrpc call-nis-ibremove 8
  ib-request
  nis-result)

(defrpc call-nis-ibfirst 9
  ib-request
  nis-result)

(defrpc call-nis-ibnext 10
  ib-request
  nis-result)

(defrpc call-nis-finddirectory 12
  fd-args
  fd-result)

(defrpc call-nis-status 14
  nis-taglist
  nis-taglist)

(defrpc call-nis-dumplog 15
  dump-args
  log-result)

(defrpc call-nis-dump 16
  dump-args
  log-result)

(defrpc call-nis-callback 17
  netobj
  :boolean)

(defrpc call-nis-cptime 18
  nis-name
  :uint64)

(defrpc call-nis-checkpoint 19
  nis-name
  cp-result)

(defrpc call-nis-ping 20
  ping-args
  :void)

(defrpc call-nis-servstate 21
  nis-taglist
  nis-taglist)

(defrpc call-nis-mkdir 22
  nis-name
  nis-error)

(defrpc call-nis-rmdir 23
  nis-name
  nis-error)

(defrpc callnis-updkeys 24
  nis-name
  nis-error)


