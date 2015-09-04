(setf *compile-print* nil)
(require 'asdf)
(asdf:load-system :clnl)
(clnl:run)
