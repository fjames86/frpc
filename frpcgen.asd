;;;; Copyright (c) Frank James 2015 <frank.a.james@gmail.com>
;;;; This code is licensed under the MIT license.

(asdf:defsystem :frpcgen
  :name "frpcgen"
  :author "Frank James"
  :description "A protocol compiler for frpc."
  :license "MIT"
  :components 
  ((:module :gen
	    :components
	    ((:file "gen"))))
  :depends-on (:frpc :yacc :cl-lex))
