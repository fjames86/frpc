;;;; Copyright (c) Frank James 2015 <frank.a.james@gmail.com>
;;;; This code is licensed under the MIT license.

;;; This file defines a program which can parse x files (XDR definition files) as distributed for input
;;; into the rpcgen program commonly used to write rpc programs on UNIX platforms. 
;;; The intention is to be able to generate a skeleton Lisp file which largely corresponds to the original x 
;;; definition. Probably some hand modifications will be required but it should at least make things easier,
;;; particularly with large/complicated interfaces.

;; Note: this is not finished yet.

;;(ql:quickload '("yacc" "cl-lex"))

(defpackage #:frpc.gen
  (:use #:cl #:yacc #:cl-lex))

(in-package #:frpc.gen)

;; 5. THE XDR LANGUAGE SPECIFICATION

;;    5.1 Notational Conventions

;;    This specification uses an extended Back-Naur Form notation for
;;    describing the XDR language.  Here is a brief description of the
;;    notation:

;;    (1) The characters '|', '(', ')', '[', ']', '"', and '*' are special.
;;    (2) Terminal symbols are strings of any characters surrounded by
;;    double quotes.
;;    (3) Non-terminal symbols are strings of non-special characters.
;;    (4) Alternative items are separated by a vertical bar ("|").
;;    (5) Optional items are enclosed in brackets.
;;    (6) Items are grouped together by enclosing them in parentheses.
;;    (7) A '*' following an item means 0 or more occurrences of that item.

;;    For example,  consider  the  following pattern:

;;          "a " "very" (", " "very")* [" cold " "and "]  " rainy "
;;          ("day" | "night")

;;    An infinite number of strings match this pattern. A few of them are:
;;          "a very rainy day"
;;          "a very, very rainy day"
;;          "a very cold and  rainy day"
;;          "a very, very, very cold and  rainy night"

;; 5.2 Lexical Notes

;;    (1) Comments begin with '/*' and terminate with '*/'.
;;    (2) White space serves to separate items and is otherwise ignored.
;;    (3) An identifier is a letter followed by an optional sequence of
;;    letters, digits or underbar ('_'). The case of identifiers is not
;;    ignored.
;;    (4) A constant is a sequence of one or more decimal digits,
;;    optionally preceded by a minus-sign ('-').

;; 5.3 Syntax Information

;;       declaration:
;;            type-specifier identifier
;;          | type-specifier identifier "[" value "]"
;;          | type-specifier identifier "<" [ value ] ">"
;;          | "opaque" identifier "[" value "]"
;;          | "opaque" identifier "<" [ value ] ">"
;;          | "string" identifier "<" [ value ] ">"
;;          | type-specifier "*" identifier
;;          | "void"

;;       value:
;;            constant
;;          | identifier

;;       type-specifier:
;;            [ "unsigned" ] "int"
;;          | [ "unsigned" ] "hyper"
;;          | "float"
;;          | "double"
;;          | "bool"
;;          | enum-type-spec
;;          | struct-type-spec
;;          | union-type-spec
;;          | identifier

;;       enum-type-spec:
;;          "enum" enum-body

;;       enum-body:
;;          "{"
;;             ( identifier "=" value )
;;             ( "," identifier "=" value )*
;;          "}"

;;       struct-type-spec:
;;          "struct" struct-body

;;       struct-body:
;;          "{"
;;             ( declaration ";" )
;;             ( declaration ";" )*
;;          "}"

;;       union-type-spec:
;;          "union" union-body

;;       union-body:
;;          "switch" "(" declaration ")" "{"
;;             ( "case" value ":" declaration ";" )
;;             ( "case" value ":" declaration ";" )*
;;             [ "default" ":" declaration ";" ]
;;          "}"

;;       constant-def:
;;          "const" identifier "=" constant ";"

;;       type-def:
;;            "typedef" declaration ";"
;;          | "enum" identifier enum-body ";"
;;          | "struct" identifier struct-body ";"
;;          | "union" identifier union-body ";"

;;       definition:
;;            type-def
;;          | constant-def

;;       specification:
;;            definition *

;; 5.4 Syntax Notes

;;    (1) The following are keywords and cannot be used as identifiers:
;;    "bool", "case", "const", "default", "double", "enum", "float",
;;    "hyper", "opaque", "string", "struct", "switch", "typedef", "union",
;;    "unsigned" and "void".

;;    (2) Only unsigned constants may be used as size specifications for
;;    arrays.  If an identifier is used, it must have been declared
;;    previously as an unsigned constant in a "const" definition.

;;    (3) Constant and type identifiers within the scope of a specification
;;    are in the same name space and must be declared uniquely within this
;;    scope.

;;    (4) Similarly, variable names must  be unique within  the scope  of
;;    struct and union declarations. Nested struct and union declarations
;;    create new scopes.

;;    (5) The discriminant of a union must be of a type that evaluates to
;;    an integer. That is, "int", "unsigned int", "bool", an enumerated
;;    type or any typedefed type that evaluates to one of these is legal.
;;    Also, the case values must be one of the legal values of the
;;    discriminant.  Finally, a case value may not be specified more than
;;    once within the scope of a union declaration.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun select-n (n)
    (lambda (&rest args) (nth n args)))
  
  (defun select-these (nums)
    (lambda (&rest args) 
      (do ((i 0 (1+ i))
	   (res nil)
	   (n nums (cdr n))
	   (l args (cdr l)))
	  ((null l) (nreverse res))
	(when (= (car n) i)
	  (push (car l) res))))))

(define-string-lexer xdr-lexer 
  ("\\=" (return (values '|=| '|=|)))
  ("\\{" (return (values '|{| '|{|)))
  ("\\}" (return (values '|}| '|}|)))
  ("\\;" (return (values '|;| '|;|)))
  ("\\," (return (values '|,| '|,|)))
  ("\\<" (return (values '|<| '|<|)))
  ("\\>" (return (values '|>| '|>|)))
  ("\\[" (return (values '|[| '|[|)))
  ("\\]" (return (values '|]| '|]|)))
  ("\\(" (return (values '|(| '|(|)))
  ("\\)" (return (values '|)| '|)|)))
  ("\\*" (return (values '|*| '|*|)))
  ("struct" (return (values 'struct 'struct)))
  ("enum" (return (values 'enum 'enum)))
  ("typedef" (return (values 'typedef 'typedef)))
  ("const" (return (values 'const 'const)))
  ("int" (return (values 'int 'int)))
  ("hyper" (return (values 'hyper 'hyper)))
  ("float" (return (values 'float 'float)))
  ("double" (return (values 'double 'double)))
  ("unsigned" (return (values 'unsigned 'unsigned)))
  ("string" (return (values 'string 'string)))
  ("default" (return (values 'default 'default)))
  ("bool" (return (values 'bool 'bool)))
  ("opaque" (return (values 'opaque 'opaque)))
  ("case" (return (values 'case 'case)))
  ("default" (return (values 'default 'default)))
  ("program" (return (values 'program 'program)))
  ("version" (return (values 'version 'version)))
  ("void" (return (values 'void 'void)))
  ("[-]?[0-9]+" (return (values 'constant (parse-integer $@))))
  ("\\w+" (return (values 'identifier (alexandria:symbolicate (string-upcase $@)))))
  ("/\\*([\\S\\s]*)\\*/" (return (values 'block-comment $1)))
  ("//(.*)\\\n" (return (values 'line-comment $1))))
           
(defun test-lexer (string)
  (let ((l (xdr-lexer string)))
    (do (done)
        (done)
      (multiple-value-bind (token val) (funcall l)
        (if token
            (format t "~S ~S~%" token val)
            (setf done t))))))


(define-parser *xdr-parser*
  (:start-symbol specification)
  (:terminals (|;| |{| |}| |=| |(| |)| |,| |[| |]| |<| |>| |*|
                   identifier constant 
                   struct union enum typedef const default case 
                   unsigned int hyper float double bool void string opaque
	           program version))

  (declaration
   (type-specifier identifier)
   (type-specifier identifier |[| value |]|
                   (lambda (a b c d e) (declare (ignore c e))
                           (list b `(:varray* ,a ,d))))
   (type-specifier identifier |<| value |>|
                   (lambda (a b c d e) (declare (ignore c e))
                           (list b `(:varray ,a ,d))))
   (type-specifier identifier |<| |>|
                   (lambda (a b c e) (declare (ignore c e))
                           (list b `(:varray ,a))))
   (opaque identifier |[| value |]|
	   (lambda (a b c d e) (declare (ignore a c e))
		   (list b `(:varray* :octet ,d))))
   (opaque identifier |<| |>|
	   (lambda (a b c d) (declare (ignore a c d))
		   (list b `(:varray* :octet))))
   (opaque identifier |<| value |>|
	   (lambda (a b c d e) (declare (ignore a c e))
		   (list b `(:varray* :octet ,d))))
   (string identifier |<| |>| 
           (lambda (a b c e) (declare (ignore a c e))
                   (list b :string)))
   (string identifier |<| value |>| 
           (lambda (a b c d e) (declare (ignore a c d e))
                   (list b :string)))
   (type-specifier |*| identifier
		   (lambda (a b c) (declare (ignore b))
			   (list c `(:optional ,a))))
   (void (lambda (a) (declare (ignore a)) :void)))

  
  (value 
   constant 
   identifier)

  (type-specifier 
   (unsigned int (lambda (u i) (declare (ignore u i)) :uint32))
   (int (lambda (i) (declare (ignore i)) :int32))
   (unsigned hyper (lambda (u h) (declare (ignore u h)) :uint64))
   (hyper (lambda (h) (declare (ignore h)) :int64))
   (float (lambda (f) (declare (ignore f)) :real32))
   (double (lambda (d) (declare (ignore d)) :real64))
   (bool (lambda (b) (declare (ignore b)) :boolean))
   enum-type-spec
   struct-type-spec
   union-type-spec
   identifier)

  (enum-type-spec 
   (enum enum-body (lambda (a b) (declare (ignore a)) b)))

  (enum-body 
   (|{| enum-body-list |}| (select-n 1)))

  (enum-body-list 
   (identifier |=| value 
               (lambda (a b c) (declare (ignore b)) 
		       (list (list (intern (string-upcase a) :keyword) c))))
   (enum-body-list |,| identifier |=| value
                   (lambda (prev comma id eq val)
                     (declare (ignore comma eq))
                     (append prev 
                           (list (list (intern (string-upcase id) :keyword) val))))))
			   
  (struct-type-spec 
   (struct |{|  struct-body |}| (select-n 2)))

  (struct-body 
   (declaration |;| (lambda (a b) (declare (ignore b)) (list a)))
   (struct-body declaration |;| (lambda (a b c) 
                                  (declare (ignore c))
                                  (append a (list b)))))

  (union-type-spec 
   (union union-body (lambda (a b) (declare (ignore a)) b)))

  (union-body-list 
   (case value |:| declaration |;| 
	 (lambda (a b c d e) (declare (ignore a c e))
		 (list `(,b ,d))))
   (union-body-list case value |:| declaration |;|
		    (lambda (a b c d e f) (declare (ignore b d f))
			    (append a 
				    (list `(,c ,e)))))
   (union-body-list default |:| declaration |;|
		    (lambda (a b c d e) (declare (ignore b c e))
			    (append a 
				    (list `(otherwise ,d))))))

  (union-body 
   (switch |(| declaration |)| |{| union-body-list |}|
	   (lambda (a b c d e f g) (declare (ignore a b d e g))
		   `(:union ,c ,@f))))

  (constant-def 
   (const identifier |=| constant |;|
          (lambda (a b c d e) (declare (ignore a c e))
		  `(defconstant ,(alexandria:symbolicate '+ b '+)
		     ,d))))

  (type-def 
   (typedef declaration |;|
            (lambda (td decl sc)
              (declare (ignore td sc))
	      `(defxtype* ,(alexandria:symbolicate (string-upcase (car decl)))
		   () ,(cadr decl))))
   (enum identifier enum-body |;| 
         (lambda (e id b c)
           (declare (ignore e c))
           `(defxenum ,id () ,@b)))
   (struct identifier |{| struct-body |}| |;|
           (lambda (a b c d e f) 
             (declare (ignore a c e f))
             `(defxstruct ,(alexandria:symbolicate (string-upcase b))
		  () ,@d)))
   (union identifier union-body |;|
	  (lambda (a b c d) (declare (ignore a d))
		  `(defxunion ,b () ,@c))))

  (program-def
   (program identifier |{| version-def |}| |=| value |;|
	    (lambda (a b c d e f g h)
	      (declare (ignore a c e f h))
	      `(:program ,b ,g 
			 ,@d))))

  (version-def 
   (version identifier |{| rpc-def-list |}| |=| value |;|
	    (lambda (a b c d e f g h)  
	      (declare (ignore a c e f h))
	      (list `(:version ,b ,g ,@d))))
   (version-def version identifier |{| rpc-def-list |}| |=| value |;|
		(lambda (a b c d e f g h i)
		  (declare (ignore b d f g i))
		  (append a 
			  (list `(:version ,c ,h ,@e))))))

  (rpc-def-list 
   (rpc-type-spec identifier |(| rpc-type-spec |)| |=| value |;|
		   (lambda (a b c d e f g h)
		     (declare (ignore c e f h))
		     (list `(defrpc ,b ,g ,d ,a))))
   (rpc-def-list rpc-type-spec identifier |(| rpc-type-spec |)| |=| value |;|
		 (lambda (a b c d e f g h i)
		   (declare (ignore d f g i))
		   (append a 
			   (list `(defrpc ,c ,h ,e ,b))))))

  (rpc-type-spec 
   (void (lambda (a) (declare (ignore a)) :void))
   type-specifier)

  (definition 
    (type-def)
    (constant-def)
    (program-def))
    
  (specification 
   definition
   (specification definition)))

       
(defun test-parser (string)
  (parse-with-lexer (xdr-lexer string) *xdr-parser*))

