(require 'asdf)
(setf asdf:*central-registry* (list #p"deps/"))
(asdf:load-system :clnl-test)
(sb-ext:quit :unix-status (if (clnl-test:run-all-tests) 0 1))
