(require 'asdf)
(setf asdf:*central-registry* (list #p"deps/"))
(asdf:load-system :cl-nl-test)
(cl-nl-test:run)
