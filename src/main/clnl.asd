(asdf:defsystem clnl
  :name "Experiment"
  :version "0.0.1"
  :maintainer "Frank Duncan (frank@kank.com)"
  :author "Frank Duncan (frank@kank.com)"
  :serial t
  :components ((:file "package")
               (:file "lex")
               (:file "parse")
               (:file "nvm")
               (:file "transpile")
               (:file "random")
               (:file "main"))
  :depends-on (:cl-ppcre :mt19937))
