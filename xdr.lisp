;;;; Copyright (c) Frank James 2015 <frank.a.james@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package #:frpc)

;; types:

;; int32/uint32 int64/uint64 -- add uint8 (octet) type???
;; string == variable length array of octets cast to string
;; floats
;; void == 0 length 
;; boolean == enum { TRUE, FALSE }

;; fixed/variable  size array
;; fixed/variable data == array of octets???

;; structs
;; enums
;; tagged unions

(defvar *xtypes* (make-hash-table))

(defun %defxtype (name reader writer)
  (setf (gethash name *xtypes*)
	(list reader writer)))

(defun xtype-reader (name) 
  (declare (type symbol name))
  (let ((fn (first (gethash name *xtypes*))))
    (if fn
	fn
	(error "No type ~S" name))))

(defun xtype-writer (name) 
  (declare (type symbol name))
  (let ((fn (second (gethash name *xtypes*))))
    (if fn
	fn
	(error "No type ~S" name))))

(defmacro defxtype (name (&rest options) ((reader-stream) &body reader-body) ((writer-stream obj) &body writer-body))
  (let ((reader (let ((n (cadr (assoc :reader options))))
		  (if n 
		      n
		      (alexandria:symbolicate '%read- name))))
	(writer (let ((n (cadr (assoc :writer options))))
		  (if n
		      n 
		      (alexandria:symbolicate '%write- name)))))
    `(progn
       (defun ,reader (,reader-stream)
	 ,@reader-body)
       (defun ,writer (,writer-stream ,obj)
	 ,@writer-body)
       (%defxtype ',name (function ,reader) (function ,writer)))))
       

(defun read-xtype (type stream)
  (cond
    ((functionp type)
     (funcall type stream))
    (t 
     (funcall (xtype-reader type) stream))))

(defun write-xtype (type stream obj)
  (cond
    ((functionp type)
     (funcall type stream obj))
    (t 
     (funcall (xtype-writer type) stream obj))))

(defun pad-index (index)
  (let ((m (mod index 4)))
    (if (zerop m)
	index
	(+ index (- 4 m)))))


(defun pack (writer obj)
  "Write the object into an octet-buffer."
  (flexi-streams:with-output-to-sequence (v :element-type 'nibbles:octet)
    (funcall writer v obj)))

(defun unpack (reader buffer)
  "Read the object from an octet-buffer."
  (flexi-streams:with-input-from-sequence (v buffer)
    (funcall reader v)))

;; ------ type definitions ---

(defxtype :int32 ((:reader read-int32) (:writer write-int32))
  ((stream)
   (nibbles:read-sb32/be stream))
  ((stream obj)
   (nibbles:write-sb32/be obj stream)))

(defxtype :int64 ((:reader read-int64) (:writer write-int64))
  ((stream)
   (nibbles:read-sb64/be stream))
  ((stream obj)
   (nibbles:write-sb64/be obj stream)))

(defxtype :uint32 ((:reader read-uint32) (:writer write-uint32))
  ((stream)
   (nibbles:read-ub32/be stream))
  ((stream obj)
   (nibbles:write-ub32/be obj stream)))

(defxtype :uint64 ((:reader read-uint64) (:writer write-uint64))
  ((stream)
   (nibbles:read-ub64/be stream))
  ((stream obj)
   (nibbles:write-ub64/be obj stream)))

(defun read-array-padding (stream array-length)
  (let ((m (mod array-length 4)))
    (unless (zerop m)
      (read-sequence (loop :for i :below (- 4 m) :collect 0) stream)))
  nil)
(defun write-array-padding (stream array-length)
  (let ((m (mod array-length 4)))
    (unless (zerop m)
      (write-sequence (subseq #(0 0 0) 0 (- 4 m)) stream))))

(defxtype :string ((:reader read-xstring) (:writer write-xstring))
  ((stream)
   (let ((len (read-uint32 stream)))
     (when (> len +max-octet-array-length+)
      (error "Attempted to read a silly size array ~DMB" (truncate len (* 1024 1024))))
     (let ((buff (nibbles:make-octet-vector (pad-index len))))
       (read-sequence buff stream)
       (babel:octets-to-string buff :start 0 :end len))))
  ((stream obj)
   (let* ((octets (babel:string-to-octets obj))
	  (length (length octets)))
     (write-uint32 stream length)
     (write-sequence octets stream)
     (write-array-padding stream length))))

(defxtype :boolean ((:reader read-boolean) (:writer write-boolean))
  ((stream)
   (not (zerop (read-int32 stream))))
  ((stream obj)
   (if obj
       (write-int32 stream 1)
       (write-int32 stream 0))))

(defxtype :void ()
  ((stream)
   (declare (ignore stream))
   nil)
  ((stream obj)
   (declare (ignore stream obj))
   nil))

(defxtype :real32 ()
  ((stream)
   (nibbles:read-ieee-single/be stream))
  ((stream obj)
   (nibbles:write-ieee-single/be obj stream)))

(defxtype :real64 ()
  ((stream)
   (nibbles:read-ieee-double/be stream))
  ((stream obj)
   (nibbles:write-ieee-double/be obj stream)))

(defxtype :octet ((:reader read-octet) (:writer write-octet))
  ((stream)
   (read-byte stream))
  ((stream obj)
   (write-byte obj stream)))

;; read fixed arrays into vectors
(defun read-fixed-array (reader stream length)
  (let ((arr (make-array length)))
    (dotimes (i length)
      (setf (aref arr i) (funcall reader stream)))
    arr))

(defun write-fixed-array (writer stream array)
  (let ((length (length array)))
    (dotimes (i length)
      (funcall writer stream (aref array i))))
  nil)

(defconstant +max-octet-array-length+ (* 50 1024 1024))

(defun read-octet-array (stream &optional buffer)
  (let ((len (read-uint32 stream)))
    (when (> len +max-octet-array-length+)
      (error "Attempted to read a silly size array ~DMB" (truncate len (* 1024 1024))))
    (let ((sequence (or buffer (nibbles:make-octet-vector len))))
      (dotimes (i len)
        (setf (elt sequence i)
              (read-octet stream)))
      sequence)))

(defun write-octet-array (stream sequence &key (start 0) end)
  (unless end (setf end (length sequence)))
  (let ((len (- end start)))
    (write-uint32 stream len)
    (do ((i start (1+ i)))
        ((= i end) len)
      (write-octet stream (aref sequence i)))
    (write-array-padding stream len))
  nil)
                

;; read optional or return nil
(defun read-optional (reader stream)
  (let ((present (read-boolean stream)))
    (when present 
      (funcall reader stream))))

(defun write-optional (writer stream &optional obj)
  (cond
    (obj
     (write-boolean stream t)
     (funcall writer stream obj))
    (t (write-boolean stream nil))))


;; ---------------------

;; enums: 
;; compile-time pairings of symbols to integers
;; we don't need to generate readers/writers for these because they are gone
;; by the time the data is serialized. we only need a way of pairing the symbols
;; to ints and gettings their partners back.
;; we could of course just define them all as consts -- that would not be ideal
;; instead store each set as an alist in an ht

(defvar *enums* (make-hash-table))

(defmacro defxenum (name &rest slots)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf (gethash ',name *enums*)
	   (list ,@(let ((n 0))
		     (mapcar (lambda (slot)
			       (prog1
				   (if (listp slot)
				       (destructuring-bind (slot-name slot-val) slot
					 (declare (type integer slot-val))
					 (setf n slot-val)
					 `(cons ',slot-name ,slot-val))
				       `(cons ',slot ,n))
				 (incf n)))
			     slots))))))


(defun enum (enum slot)
  (let ((enums (if (listp enum)
                   enum 
                   (gethash enum *enums*))))
    (unless enums
      (error "No enum defined for ~S" enum))
    (etypecase slot
      (symbol
       (dolist (s enums)
         (when (eq slot (car s))
           (return-from enum (cdr s)))))
      (integer 
       (dolist (s enums)
         (when (= slot (cdr s))
           (return-from enum (car s))))))
    slot))

(defun enump (enum-type)
  (nth-value 1 (gethash enum-type *enums*)))

(defun read-enum (stream enum-type)
  (enum enum-type (read-int32 stream)))

(defun write-enum (stream enum-type slot)
  (write-int32 stream (enum enum-type slot)))

;; ----------------------

;; compilers for the reader/writer 
;; we need to eval-when it so we can use it in our type definition macros

(eval-when (:compile-toplevel :load-toplevel :execute)

(defun compile-reader (forms stream-sym)
  (flet ((compile-struct (struct-type slots)
	   (let ((gobj (gensym "OBJ")))
	     `(let ((,gobj (,(alexandria:symbolicate 'make- struct-type))))
		,@(mapcar (lambda (slot)
			    (unless (symbolp slot)
			      (destructuring-bind (slot-name form &rest init) slot 
				(declare (ignore init))
				(let ((accessor (alexandria:symbolicate struct-type '- slot-name)))
				  `(setf (,accessor ,gobj) ,(compile-reader form stream-sym))))))
			  slots)
		,gobj)))
	 (compile-union (enum-type arms)
	   (let ((gtag (gensym "TAG")))
	     `(let ((,gtag (enum ',enum-type (read-int32 ,stream-sym))))
		(make-xunion ,gtag
			     (case ,gtag
			       ,@(mapcar (lambda (arm)
					   (destructuring-bind (keys &rest forms) arm
					     (if (cdr forms)
						 `(,keys ,(compile-reader `(:list ,@forms) stream-sym))
						 `(,keys ,(compile-reader (car forms) stream-sym)))))
					 arms))))))
	 (compile-list (forms)
	   `(list ,@(mapcar (lambda (form)
			      (compile-reader form stream-sym))
			    forms)))
	 (compile-plist (forms)
	   (alexandria:with-gensyms (gplist)
	     `(let ((,gplist nil))
		,@(do ((%forms forms (cddr %forms))
		       (rlist nil))
		      ((null %forms) (reverse rlist))
		     (let ((tag (car %forms))
			   (form (cadr %forms)))
		       (push `(setf (getf ,gplist ',tag)
				    ,(compile-reader form stream-sym))
			     rlist)))
		,gplist)))
	 (compile-alist (forms)
	   `(list ,@(mapcar (lambda (pair)
			      (destructuring-bind (tag &rest forms) pair
				(if (cdr forms)
				    `(cons ',tag ,(compile-reader `(:list ,@forms) stream-sym))
				    `(cons ',tag ,(compile-reader (car forms) stream-sym)))))
			    forms))))
    (cond
      ((symbolp forms)
       ;; symbol naming type
       (if (enump forms)
	   `(enum ',forms (read-int32 ,stream-sym))
	   `(funcall (xtype-reader ',forms) ,stream-sym)))
      ((not (listp forms))
       (error "Unable to compile form ~S" forms))
      ((null (cdr forms))
       ;; list of single form, just compile that 
       (compile-reader (car forms) stream-sym))
      (t 
       (ecase (car forms)
	 (:struct
	  ;; a structure form
	  (destructuring-bind (struct-type &rest slots) (cdr forms)
	    (compile-struct struct-type slots)))
	 (:union
	  ;; (:union enum-type (:enum-val &rest forms))
	  (destructuring-bind (enum-type &rest arms) (cdr forms)
	    (compile-union enum-type arms)))
	 (:optional 
	  ;; (:optional form)
	  `(unless (zerop (read-int32 ,stream-sym))
	     ,(compile-reader (cadr forms) stream-sym)))
	 (:alist 
	  ;; like a structure but generates alists instead
	  (compile-alist (cdr forms)))
	 (:plist 
	  ;; list a structurebut generates a plist instead
	  (compile-plist (cdr forms)))
	 (:list 
	  ;; a list of values (essentially unnamed strucuture)
	  (compile-list (cdr forms)))
	 (:array 
	  ;; (:array form length)
	  (destructuring-bind (form length) (cdr forms)
	    (alexandria:with-gensyms (garr gi)
	      `(let ((,garr (make-array ,length)))
		 (dotimes (,gi ,length)
		   (setf (aref ,garr ,gi)
			 ,(compile-reader form stream-sym)))
		 ,garr))))
	 (:varray*
	  ;; (:varray* form &optional max-length)
	  (destructuring-bind (form &optional max-length) (cdr forms)
	    (alexandria:with-gensyms (garr gi glen)
	      `(let ((,glen (read-uint32 ,stream-sym)))
             (when (> ,glen +max-octet-array-length+)
               (error "Attempted to read a silly size array ~DMB" (truncate ,glen (* 1024 1024))))
             (let ((,garr (make-array ,glen)))
		 ,@(when max-length 
		     `((when (> ,glen ,max-length) 
			 (error "Length ~S exceeds maximum length ~S" ,glen ,max-length))))
		 (dotimes (,gi ,glen)
		   (setf (aref ,garr ,gi)
			 ,(compile-reader form stream-sym)))
         ;; read array padding if required to, but only if an octet vector
         ,@(when (eq form :octet)
            `((read-array-padding stream ,glen)
		 ,garr))
	 ,garr)))))
	 (:varray 
	  ;; (:varray form &optional max-length)
	  (destructuring-bind (form &optional max-length) (cdr forms)
	    (alexandria:with-gensyms (glen gi)
	      `(let ((,glen (read-uint32 ,stream-sym)))
		 ,@(when max-length 
			 `((when (> ,glen ,max-length) 
			     (error "Length ~S exceeds maximum length ~S" ,glen ,max-length))))
		 (prog1 (loop :for ,gi :below ,glen :collect 
			     ,(compile-reader form stream-sym))
		   ;; read array padding if required to, but only if an octet vector
		   ,@(when (eq form :octet)
			   `((read-array-padding stream ,glen))))))))
         )))))
		    
 
(defun compile-writer (forms stream-sym obj-form)
  (flet ((compile-struct (struct-type slots)
	   (let ((gobj (gensym "OBJ")))
	     `(let ((,gobj ,obj-form))
		,@(mapcar (lambda (slot)
			    (unless (symbolp slot)
			      (destructuring-bind (slot-name form &rest init) slot 
				(declare (ignore init))
				(let ((accessor (alexandria:symbolicate struct-type '- slot-name)))
				  (compile-writer form stream-sym `(,accessor ,gobj))))))
			  slots))))
	 (compile-union (enum-type arms)
	   (alexandria:with-gensyms (gtag gval)
	     `(let ((,gval (xunion-val ,obj-form))
		    (,gtag (xunion-tag ,obj-form)))
		(write-int32 ,stream-sym (enum ',enum-type ,gtag))
		(case ,gtag
		  ,@(mapcar (lambda (arm)
			      (destructuring-bind (keys &rest forms) arm
				(if (cdr forms)
				    `(,keys ,(compile-writer `(:list ,@forms) stream-sym gval))
				    `(,keys ,(compile-writer (car forms) stream-sym gval)))))
			    arms)))))
	 (compile-list (forms)
	   (let ((glist (gensym "LIST")))
	     `(let ((,glist ,obj-form))
		,@(mapcar (lambda (form)
			    `(progn 
			       ,(compile-writer form stream-sym `(car ,glist))
			       (setf ,glist (cdr ,glist))))
			  forms))))
	 (compile-plist (forms)
	   (let ((glist (gensym "LIST")))
	     `(let ((,glist ,obj-form))
		,@(do ((%forms forms (cddr %forms))
		       (rlist nil))
		      ((null %forms) (reverse rlist))
		    (let ((tag (car %forms))
			  (form (cadr %forms)))
		      (push (compile-writer form stream-sym `(getf ,glist ',tag))
			    rlist))))))
	 (compile-alist (forms)
	   (let ((glist (gensym "LIST")))
	     `(do ((,glist ,obj-form (cdr ,glist)))
		  ((null ,glist))
		(ecase (caar ,glist)
		  ,@(mapcar (lambda (pair)
			      (destructuring-bind (tag &rest forms) pair
				`(,tag 
				  ,(if (cdr forms)
				       (compile-writer `(:list ,@forms) stream-sym `(cdar ,glist))
				       (compile-writer (car forms) stream-sym `(cdar ,glist))))))
			    forms))))))
    (cond 
      ((symbolp forms)
       ;; symbol naming type
       (if (enump forms)
	   `(write-int32 ,stream-sym (enum ',forms ,obj-form))
	   `(funcall (xtype-writer ',forms) ,stream-sym ,obj-form)))
      ((not (listp forms))
       (error "Unable to compile form ~S" forms))
      ((null (cdr forms))
       ;; list of single form -- compile as a single form
       (compile-writer (car forms) stream-sym obj-form))
      (t 
       (ecase (car forms)
	 (:struct
	  (destructuring-bind (struct-type &rest slots) (cdr forms)
	    (compile-struct struct-type slots)))
	 (:union
	  (destructuring-bind (enum-type &rest arms) (cdr forms)
	    (compile-union enum-type arms)))
	 (:optional 
	  ;; (:optional form)
	  ;; FIXME: there is a bug if the form is :boolean and the value is NIL because 
	  ;; we can't tell the difference between it being provided (and false) 
	  ;; and not provided. but who uses optional booleans anyway? 
	  (let ((gobj (gensym "OBJ")))
	    `(let ((,gobj ,obj-form))
	       (cond
		 (,gobj
		  (write-int32 ,stream-sym 1)
		  ,(compile-writer (cadr forms) stream-sym gobj))
		 (t
		  (write-int32 ,stream-sym 0))))))
	 (:alist 
	  (compile-alist (cdr forms)))
	 (:list 
	  (compile-list (cdr forms)))
	 (:plist 
	  (compile-plist (cdr forms)))
	 (:array 
	  ;; (:array form length)
	  (destructuring-bind (form length) (cdr forms)
	    (alexandria:with-gensyms (gobj gi glen)
	      `(let* ((,gobj ,obj-form)
		      (,glen (length ,gobj)))
		 (unless (= ,glen ,length) 
		   (error "Fixed array length doesn't match declaration."))
		 (dotimes (,gi ,glen)
		   ,(compile-writer form 
				    stream-sym 
				    `(aref ,gobj ,gi)))))))
	 (:varray*
	  ;; (:varray form &optional max-length)
	  (destructuring-bind (form &optional max-length) (cdr forms)
	    (alexandria:with-gensyms (gobj glen gi)
	      `(let* ((,gobj ,obj-form)
		      (,glen (length ,gobj)))
		 ,@(when max-length
		     `((when (> ,glen ,max-length)
			 (error "Length ~S exceeds max length ~S" ,glen ,max-length))))
		 (write-uint32 ,stream-sym ,glen)
		 (dotimes (,gi ,glen)
		   ,(compile-writer form stream-sym `(aref ,gobj ,gi)))
         ;; write padding if required to, but only if this is an octet array
         ,(when (eq form :octet)
            `(write-array-padding ,stream-sym ,glen))
		 nil))))
	 (:varray
	  ;; (:varray form &optional max-length)
	  (destructuring-bind (form &optional max-length) (cdr forms)
	    (alexandria:with-gensyms (gobj glen gi)
	      `(let* ((,gobj ,obj-form)
		      (,glen (length ,gobj)))
		 ,@(when max-length
			 `((when (> ,glen ,max-length)
			     (error "Length ~S exceeds max length ~S" ,glen ,max-length))))
		 (write-uint32 ,stream-sym ,glen)
		 (dolist (,gi ,gobj)
		   ,(compile-writer form stream-sym gi))
         ;; write padding at the end, but only if this is an octet array
         ,(when (eq form :octet)
           `(write-array-padding ,stream-sym ,glen))
		 nil))))
	 )))))
  
) ;; eval-when

		  
;; ----------------- unions -------------------

(defun make-xunion (tag val)
  (cons tag val))
(defun xunion-tag (un) (car un))
(defun xunion-val (un) (cdr un))

(defmacro defxunion (name (enum &rest options) &rest arms)
  `(defxtype ,name (,@options)
     ((stream)
      ,(compile-reader `(:union ,enum ,@arms) 'stream))
     ((stream obj)
      ,(compile-writer `(:union ,enum ,@arms) 'stream 'obj))))

;; -------------

;; structures 

(defmacro defxstruct (name-and-options options &rest slots)
  (let ((name (if (symbolp name-and-options)
			   name-and-options
			   (car name-and-options))))
  `(progn
     ;; define the structure
     (defstruct ,name-and-options
       ,@(mapcar (lambda (slot)
		   (if (symbolp slot)
		       slot
		       (destructuring-bind (slot-name slot-form &rest slot-options) slot
			 (cond
			   (slot-options
			    `(,slot-name ,@slot-options))
			   ((enump slot-form)
			    `(,slot-name 0))
			   ((symbolp slot-form)
			    (case slot-form
			      ((:int32 :int64 :uint32 :uint64 :octet)
			       `(,slot-name 0))
			      (:string
			       `(,slot-name ""))
			      ((:real32 :real64) 
			       `(,slot-name 0.0))
			      (otherwise
			       `(,slot-name))))
			   (t
			    `(,slot-name))))))
		 slots))

     ;; define the xtype
     (defxtype ,name ,options
       ((stream)
	,(compile-reader `(:struct ,name ,@slots) 'stream))
       ((stream obj)
	,(compile-writer `(:struct ,name ,@slots) 'stream 'obj)))

     ;; for REPL users
     ',name)))


(defun read-xtype-list (stream type)
  "A useful utility function to read a list-type object. I.e. a type
which consists of a structure followed by an optional next structure, e.g.
\(defxstruct a \(x :int32\) \(y :int32\) \(next \(:optional a\)\)\)
"
  (do ((list nil)
       (done nil))
      (done list)
    (push (read-xtype type stream) list)
    (let ((b (read-xtype :boolean stream)))
      (unless b (setf done t)))))

(defun write-xtype-list (stream type list)
  (do ((%list list (cdr %list)))
      ((null %list))
    (write-xtype type stream (car %list))
    (if (cdr %list)
        (write-xtype :boolean stream t)
        (write-xtype :boolean stream nil))))

;; ----------

;; lambda creators
(defmacro with-reader ((var spec) &body body)
  `(flet ((,var (stream)
	    ,(compile-reader spec 'stream)))
     ,@body))

(defmacro with-writer ((var spec) &body body)
  `(flet ((,var (stream obj)
	    ,(compile-writer spec 'stream 'obj)))
     ,@body))

(defmacro with-reader/writer ((reader writer spec) &body body)
  `(with-reader (,reader ,spec)
     (with-writer (,writer ,spec) 
       ,@body)))

;; defun versions
(defmacro defreader (name spec)
  `(defun ,name (stream)
     ,(compile-reader spec 'stream)))
(defmacro defwriter (name spec)
  `(defun ,name (stream obj)
     ,(compile-writer spec 'stream 'obj)))
(defmacro defxtype* (name options spec)
  (let ((reader (let ((n (cadr (assoc :reader options))))
		  (if n
		      n 
		      (alexandria:symbolicate '%read- name))))
	(writer (let ((n (cadr (assoc :writer options))))
		  (if n
		      n 
		      (alexandria:symbolicate '%write- name)))))		    
    (cond
      ((symbolp spec)
       ;; if the spec is a symbol then it is a trivial redefinition of another type. just associate 
       ;; this symbol with the original's reader/writer and don't define any new functions 
       `(progn
	  (eval-when (:compile-toplevel :load-toplevel :execute)
	    (%defxtype ',name nil nil))
	  (%defxtype ',name (xtype-reader ',spec) (xtype-writer ',spec))))
       (t
	;; a more complicated spec requires functions
	`(progn
	   (defreader ,reader ,spec)
	   (defwriter ,writer ,spec)
	   (eval-when (:compile-toplevel :load-toplevel :execute)
	     (%defxtype ',name nil nil))
	   (%defxtype ',name (function ,reader) (function ,writer)))))))






;; there is often a need to allocate a number of contexts, which may periodically be flushed
;; to avoid over-allocating contexts, store them in a fixed-size array. when it is exhausted, they 
;; are overwritten
(defun make-cyclic-buffer (len)
  (cons 0
	(make-array len :initial-element nil)))

(defun cyclic-push (cbuffer val)
  (setf (aref (cdr cbuffer) (car cbuffer))
	val)
  (setf (car cbuffer)
	(mod (1+ (car cbuffer)) (length (cdr cbuffer)))))

(defun cyclic-find-if (predicate cbuffer)
  (dotimes (i (length (cdr cbuffer)))
    (let ((item (aref (cdr cbuffer) i)))
      (when (and item (funcall predicate item))
	(return-from cyclic-find-if item))))
  nil)
