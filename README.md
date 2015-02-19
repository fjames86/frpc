# FRPC
Frank's XDR/RPC library is an implementation of the ONC-RPC ("SunRPC") protocol. The library is composed of two
components: implementing the eXtensible Data Representation (XDR), which is the method of serializing the messages, 
and the Remote Procedure Call (RPC) system itself, which uses XDR to exchange messages.

1. XDR serializer
----------------

The xdr serializer is largely decoupled from the rpc implementation, meaning it could be used for other purposes 
if required.

1.1 Primitive types
----------------------

All types must have a reader and writer defined. The base function and macro sugar for defining XDR types is 
```
(%defxtype 'mynewtype 
           (lambda (stream) 
             "A function which reads octets from the stream and returns the object"
             body)
           (lambda (stream obj)
             "A function which writes bytes to the stream which represent the object."
             body))

(defxtype mynewtype
  ((stream)
   reader-body)
  ((stream obj)
   writer-body))
```

This defun's a reader and writer (by default %read-mynewtype and %write-mynewtype) with their respective bodies 
and registers these using %defxtype.

The primitive types which come predefined are:
* :int32 :uint32 :int64 :uint64 :octet
* :string
* :boolean
* :real32 :real64
* :void

1.2 enums
------------

Define enum types using
```
(defxenum enum-type
  (symbol integer)
  ...)
```

Lookup a corresponding integer or symbol using
```
(enum enum-type val)
```
where val is either an integer or a symbol.

1.3 unions
-----------

Define union types using
```
(defxunion union-type (enum-type)
  ((enum-symbol type-name)
  ...))
```

1.4 structures
----------------

Define structures using

```
(defxstruct struct-name ()
  ((slot-name type-name &optional initial-value)
  ...))
```

1.5 Internals/advanced 
-------------------------

Since this is rather cumbersome, especially for more complicated types such as structs, there is a system 
to generate the typical Lisp forms required for such functions. 

* symbols map to the xtype named by that symbol
* (:list &rest forms) maps to an ordered list of objects of those types
* (:alist &rest (tag form)) maps to an alist of key/value pairs, with each key being the symbol tag, and each value being of type specified by form.
* (:optional form) maps to either an object of type form, or nil 
* (:union enum-type &rest cases) where enum-type is the name of an enum (as defined by defxenum). cases are passed directly to a Lisp (case ...) form, the keys of each case should be enums, the body of each case should be a form. The final case may be an OTHERWISE case.
* (:struct struct-name &rest (slot-name form)) where struct-name names a structure, defined with defxstruct, and slot-name is the name of a slot in that structure. 

These rules can be defined recursively.

```
(defxtype* mynewtype ()
  (:alist 
    (a :uint32)
    (b :uint32)
    (c (:optional mynewtype))))
```



2. Defining RPC interfaces
----------------------------

RPC interfaces are given a unique integer called a program number. Each program may have multiple
versions of its interface, with each version having a different set of functions/arguments. Each procedure
in the interface is also given a unique number. Together these 3 integers define the procedure identifier.

In FRPC, both clients and servers must define the interface. Servers must additionally implement handlers
for each of the procedures.

For instance, a client should write
```
(with-rpc-program (10001 1)
  (defrpc call-hello (:string :string) 0)
  (defrpc call-g    oodbye (:uint32 :string) 1)))
```
This defines 2 Lisp functions to call out to an RPC server to execute the procedures.

A servers should write
```
(with-rpc-program (1001)
  (with-rpc-version (1)
    (defrpc call-hello (:string :string) 0)
    (defhandler handle-hello (msg)
      (format nil "Hello, ~A!" msg))

    (defrpc call-goodbye (:uint32 :string) 1)
    (defhandler handle-goodbye (u)
      (format nil "Goodbye ~A!" u))))
```

3. RPC Server
----------------

At present only a single (singly-threaded) server may run at any one time. 
```
(start-rpc-server 8000)

(stop-rpc-server)
```

4. Examples
-------------

I have typed in some simple example programs, see e.g. examples/hello.lisp 

5. License
------------

Released under the terms of the MIT license.

Frank James 
Febuary 2015.







