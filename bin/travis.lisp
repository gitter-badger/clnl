(setf *compile-print* nil)
(require 'asdf)
(setf asdf:*central-registry* (list #p"deps/"))
(asdf:load-system :clnl.internal)
(asdf:load-system :clnl-test.internal)

(load "bin/all.lisp")
