# FRPC
FRPC is an implementation of the ONC-RPC ("SunRPC") protocol. 

The library is composed to two largely decoupled components: the eXtensible Data Representation (XDR) serializer,
which is used to define message structures and other on-wire formats, and the RPC framework itself.

FRPC makes it simple and easy to define ONC-RPC clients and servers. See the sister-project, Nefarious, which 
uses FRPC to implement an NFSv3 client and server.

##1. Defining RPC interfaces

RPC interfaces are given a unique integer called a program number. Each program may have multiple
versions of its interface, with each version having a different set of functions/arguments. Each procedure
in the interface is also given a unique integer. Together these 3 integers define the procedure identifier.

In FRPC, both clients and servers must define the interface. This supplies argument and result types.
Servers must additionally implement handlers for each procedure they wish to support.

For instance, a client should write:
```
(defrpc call-hello 0 :string :string)
(defrpc call-goodbye 1 :uint32 :string)
```

This defines a Lisp function for each RPC proc to call out to an RPC server to execute the procedures.

Servers should additionally implement handlers for the procedures they wish to support
```
(defhandler handle-hello (msg 0)
  (format nil "Hello, ~A!" msg))

(defhandler handle-goodbye (u 0)
  (format nil "Goodbye ~A!" u))))
```
DEFHANDLER defines a function which accepts a single argument, which will be the value decoded according
to the rule defined for the arg-type by the DEFRPC form. The handler function should return a single value which will be 
passed to the result-type serializer defined by the DEFRPC form. Handlers should never signal errors because 
the RPC protocol does not have a generalized mechanism of reporting errors. Instead, your RPC interface will typically
define a set of error statuses which will be returned as a part of the handler's return value.

The types provided to DEFRPC can be a generalized type specifier, as described below in section 4.5.

## 2. Client


The DEFRPC macro defines a wrapper around the underlying CALL-RPC function, with default values provided 
for the argument writer, result reader, program, version and proc arguments. 

Thus, with the example above, the client will be able to call a remote RPC server using, e.g., 

```
(call-hello "hello" :host "10.1.1.1" :port 8000)
```

### 2.1 CALL-RPC

The low-level client functionality is provided by CALL-RPC. This function is used by a client to 
send an RPC request to a remote server and blocks until a reply is received.

## 3. RPC Server

Singly-threaded TCP and UDP servers are currently implemented. See examples for usage.

```
;; make a server instance
(defvar *server* (make-rpc-server))

;; start the server in a new thread, it will listen for requests on TCP and UDP ports 8000
(start-rpc-server *server* :tcp-ports '(8000) :udp-ports '(8000))

;; stop the server thread
(stop-rpc-server *server*)
```

## 4. XDR serializer

The XDR serializer is largely decoupled from the rpc implementation. This means it 
could be used for other purposes as a generalised binary serialization system for any purpose.

### 4.1 Primitive types

The primitive types which come predefined are:
* :int32 :uint32 :int64 :uint64 :octet
* :string
* :boolean
* :real32 :real64
* :void

You may define new primitive types using:
```
(defxtype name ((:reader reader-name) (:writer writer-name))
  ((stream) body-reading-value-from-stream)
  ((stream obj) body-writing-obj-to-stream))
```
Only very rare circumstances should require doing this.

The optional parameters READER-NAME and WRITER-NAME are the function names
generated for the type's reader and writer. If not provided, %READ- and %WRITE- 
prepended with the type's name are used.

Use XTYPE-READER and XTYPE-WRITER to lookup the type's reader
and writer functions.

Use READ-XTYPE and WRITE-XTYPE to reader/write an instance of 
the type to/from a stream.

Use PACK/UNPACK to store/extract instances from buffers rather than streams.

### 4.2 enums

Define enum types using
```
(defxenum enum-name
  ((symbol integer)
  ...))
```

Lookup a corresponding integer or symbol using
```
(enum enum-type val)
```
where val is either an integer or a symbol.

### 4.3 unions

Define union types using
```
(defxunion union-type (enum-type)
  ((enum-symbol type-name)
  ...))
```

### 4.4 structures

Define structures using

```
(defxstruct struct-name ()
  ((slot-name type-name &optional initial-value)
  ...))
```

### 4.5 Generalized types

```
(defxtype* name ()
  form)
```

Where the FORM is:
* a symbol, naming another xtype
* (:list &rest forms) 
* (:alist &rest (tag form))
* (:plist &rest key form)
* (:struct struct-name &rest (slot-name form))
* (:union enum-name &rest (enum-keys form))
* (:array form length) 
* (:varray form &optional length) 
* (:varray* form &optional length)

These rules can be applied recursively. 

You may define local readers and writers using WITH-READER and WITH-WRITER macros.

### 5. Examples

I have typed in some simple example programs. 
For more serious usages, see port-mapper.lisp or Nefarious, an NFS implementation in Common Lisp.

### 6. License

Released under the terms of the MIT license.

Frank James 
Febuary 2015.

