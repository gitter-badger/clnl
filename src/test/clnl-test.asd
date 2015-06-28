(asdf:defsystem clnl-test
  :name "Experiment Tests"
  :version "0.0.1"
  :maintainer "Frank Duncan (frank@kank.com)"
  :author "Frank Duncan (frank@kank.com)"
  :serial t
  :components ((:file "package")
               (:file "main")
               (:file "simpletests"))
  :depends-on (:clnl :ironclad))
