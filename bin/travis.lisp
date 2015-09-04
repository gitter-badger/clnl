(setf *compile-print* nil)
(require 'asdf)
;(setf asdf:*central-registry* (list #p"deps/"))
(asdf:initialize-source-registry `(:source-registry (:tree ,(car (directory "src"))) :IGNORE-INHERITED-CONFIGURATION))
(asdf:load-system :clnl.internal)
(asdf:load-system :clnl-test.internal)

(load "bin/all.lisp")
