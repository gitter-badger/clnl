(require 'asdf)
(setf asdf:*central-registry* (list #p"deps/"))
(asdf:load-system :cl-nl-test)
(sb-ext:quit :unix-status (if (cl-nl-test:run-all-tests) 0 1))
