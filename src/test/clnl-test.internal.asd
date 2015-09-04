; For why this is the way it is, see src/main/clnl.asd
(asdf:defsystem clnl-test.internal
  :components ((:file "package")
               (:file "main")
               (:file "simpletests")
               (:file "viewtests")))
