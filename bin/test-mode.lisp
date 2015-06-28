(require 'asdf)
(setf asdf:*central-registry* (list #p"deps/"))
(asdf:load-system :clnl-test)
(clnl-test:run)
