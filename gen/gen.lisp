;;;; Copyright (c) Frank James 2015 <frank.a.james@gmail.com>
;;;; This code is licensed under the MIT license.

;;; This file defines a program which can parse x files (XDR definition files) as distributed for input
;;; into the rpcgen program commonly used to write rpc programs on UNIX platforms. 
;;; The intention is to be able to generate a skeleton Lisp file which largely corresponds to the original x 
;;; definition. Probably some hand modifications will be required but it should at least make things easier,
;;; particularly with large/complicated interfaces.

(defpackage #:frpcgen
  (:use #:cl #:yacc #:cl-lex #:frpc)
  (:export #:gen
           #:parse-definition))

(in-package #:frpcgen)

;; Usage: (gen pathspec)
;; converts a .x file into a .lisp file which contains the sort of code
;; for input into frpc.

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
  ("\\:" (return (values '|:| '|:|)))
  ("struct" (return (values 'struct 'struct)))
  ("union" (return (values 'union 'union)))
  ("switch" (return (values 'switch 'switch)))
  ("enum" (return (values 'enum 'enum)))
  ("typedef" (return (values 'typedef 'typedef)))
  ("const" (return (values 'const 'const)))
  ("int" (return (values 'int 'int)))
  ("long" (return (values 'int 'int)))
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
  ("0x([0-9a-fA-F]+)" (return (values 'constant (parse-integer (or $1 "") :radix 16))))
  ("[-]?[0-9]+" (return (values 'constant (parse-integer $@))))
  ("\\\"([0-9a-fA-F]+)\\\"" (return (values 'constant (parse-integer (or $1 "") :radix 16))))
  ("\\w+" (return (values 'identifier (alexandria:symbolicate (substitute #\- #\_ (string-upcase $@))))))
  "/\\*(.|\\\n)*?\\*/" ;; for multi-line comments
  "//(.*)\\\n"  ;; single line comments
  "\\%.*?\\\n"
  "\\#.*?\\\n")
           
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
  (:terminals (|;| |{| |}| |=| |(| |)| |,| |[| |]| |<| |>| |*| |:|
                   identifier constant 
                   struct union enum typedef const default case switch
                   unsigned int hyper float double bool void string opaque
	           program version))

  (declaration
   (type-specifier identifier (lambda (a b) (list b a)))
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
   (void (lambda (a) (declare (ignore a)) :void))
   (struct identifier identifier (lambda (a b c) (declare (ignore a))
					 (list c b))))

  
  (value 
   constant 
   identifier)

  (type-specifier 
   (unsigned int (lambda (u i) (declare (ignore u i)) :uint32))
   (unsigned (lambda (a) (declare (ignore a)) :uint32))
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
   (|{| enum-body-list |}| (lambda (a b c) (declare (ignore a c)) b)))

  (enum-body-list 
   (identifier (lambda (a) (list (list (intern (string-upcase a) :keyword) 0))))
   (identifier |=| value 
               (lambda (a b c) (declare (ignore b)) 
		       (list (list (intern (string-upcase a) :keyword) c))))
   (enum-body-list |,| identifier
		   (lambda (a b c) (declare (ignore b))
			   (append a (list (list (intern (string-upcase c) :keyword) (length a))))))
   (enum-body-list |,| identifier |=| value
                   (lambda (prev comma id eq val)
                     (declare (ignore comma eq))
                     (append prev 
                           (list (list (intern (string-upcase id) :keyword) val))))))
			   
  (struct-type-spec 
   (struct |{|  struct-body |}| 
	   (lambda (a b c d) (declare (ignore a b d))
		   `(:struct ,c)))
   (struct |*| |{| struct-body |}|
	   (lambda (a b c d e) (declare (ignore a b c e))
		   `(:optional (:struct ,d)))))

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
           `(defxenum ,id ,@b)))
   (struct identifier |{| struct-body |}| |;|
           (lambda (a b c d e f) 
             (declare (ignore a c e f))
             (let ((res (cond
			  ((<= (length d) 4)
			   `(defxtype* ,(alexandria:symbolicate (string-upcase b)) ()
			      (:plist ,@(mapcan (lambda (slot) 
						  (destructuring-bind (name type) slot 
						    `(,name ,type)))
						d))))
			  (t `(defxstruct ,(alexandria:symbolicate (string-upcase b))
				  () ,@d)))))
	       res)))
   (struct |*| identifier |{| struct-body |}| |;|
	   (lambda (a b c d e f g) (declare (ignore a b d f g))
		   `(progn (defxstruct ,(alexandria:symbolicate c '*) () ,@e)
			   (defxtype* ,c () (:optional ,(alexandria:symbolicate c '*))))))
;;   (typedef struct identifier identifier |;|
;;	    (lambda (a b c d e) (declare (ignore a b e))
;;		    `(defxtype* ,d () ,c)))
   (typedef struct identifier |*| identifier |;|
	    (lambda (a b c d e f) (declare (ignore a b d f))
		    `(defxtype* ,e () (:optional ,c))))
   (union identifier union-body |;|
	  (lambda (a b c d) (declare (ignore a d))
		  `(defxunion ,b (,(cadr (cadr c)))
		     ,@(mapcar (lambda (decl)
				 (destructuring-bind (id type) decl
				   (list (cond
					   ((eq id 'otherwise) 
					     id)
					   ((integerp id) id)
					   (t
					    (intern (string id) :keyword)))
					 (if (listp type)
					     (cadr type)
					     type))))					     
			       (cddr c))))))

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
		     (list `(defrpc ,(alexandria:symbolicate 'call- b) ,g ,d ,a))))
   (rpc-def-list rpc-type-spec identifier |(| rpc-type-spec |)| |=| value |;|
		 (lambda (a b c d e f g h i)
		   (declare (ignore d f g i))
		   (append a 
			   (list `(defrpc ,(alexandria:symbolicate 'call- c) ,h ,e ,b))))))

  (rpc-type-spec 
   (void (lambda (a) (declare (ignore a)) :void))
   type-specifier
   (struct identifier (lambda (a b) (declare (ignore a))
			      `(:struct ,b))))

  (definition 
    (type-def (lambda (a) a))
    constant-def
    program-def)
    
  (specification 
   (definition)
   (specification definition (lambda (a b) 
			       (append a (list b))))))

       
(defun test-parser (string)
  (parse-with-lexer (xdr-lexer string) *xdr-parser*))

(defun gen (pathspec &optional outfile)
  "Parse the XDR definition file named by PATHSPEC. Generates a lisp file named by OUTFILE or PATHSPEC.lisp 
with Lisp content suitable for use with frpc. Some hand modifications will be required.

Returns the parsed contents."
  (let ((body
	 (with-open-file (f pathspec :direction :input)
	   (with-output-to-string (s)
	     (do ((l (read-line f nil nil) (read-line f nil nil)))
		 ((null l))
	       (princ l s) 
	       (fresh-line s))))))
;;    (test-lexer body)
    (let ((forms (test-parser body)))
      (with-open-file (f (or outfile 
			     (merge-pathnames (make-pathname :type "lisp")
					      (pathname pathspec)))
			 :direction :output
			 :if-exists :supersede)
	(terpri f)
	(multiple-value-bind (sec min hour day month year) (decode-universal-time (get-universal-time))
	  (format f ";;; Autogenerated from ~A at ~A-~A-~A ~A:~A:~A ~%" 
		  pathspec 
		  year month day hour min sec))
	(terpri f)
	(let ((name (pathname-name (pathname pathspec)))
	      (*print-case* :downcase))
      (pprint 
       `(defpackage ,(make-symbol (string-upcase name))
          (:use #:cl #:frpc) 
          (:export ,@(do ((calls nil)
                          (flist forms (cdr flist)))
                         ((null flist) calls)
                         (let ((form (car flist)))
                           (when (eq (car form) :program)
                             (destructuring-bind (pname id &rest versions) (cdr form)
                               (declare (ignore pname id))
                               (dolist (version versions)
                                 (destructuring-bind (v vname num &rest rpcs) version
                                   (declare (ignore v vname num))
                                   (dolist (rpc rpcs)
                                     (push (make-symbol (string-upcase (cadr rpc)))
                                           calls))))))))))
       f)
      (terpri f)
      (pprint `(in-package ,(make-symbol (string-upcase name))) f)
      (terpri f))
	(let ((*print-case* :downcase))
	  (dolist (form forms)
	    (cond
	      ((eq (car form) :program)
	       ;; special case for program specifications
	       (destructuring-bind (pname id &rest versions) (cdr form)
		 (pprint `(defprogram ,pname ,id) f)
		 (terpri f)
		 (dolist (version versions)
		   (destructuring-bind (v vname num &rest rpcs) version
		     (declare (ignore v vname))
		     (dolist (rpc rpcs)
		       (pprint (append rpc `((:program ,pname ,num)))
			       f)
		       (terpri f))))))
	      (t 
	       (pprint form f)
	       (terpri f)))))
	forms))))

      
(defun parse-definition (pathspec)
  "Parse the XDR definition in PATHSPEC. Returns the parsed contents."
  (let ((body
         (with-open-file (f pathspec :direction :input)
           (with-output-to-string (s)
             (do ((l (read-line f nil nil) (read-line f nil nil)))
                 ((null l))
               (princ l s) 
               (fresh-line s))))))
    ;;    (test-lexer body)
    (test-parser body)))


