(in-package #:cl-nl)

(defun e (ast) ast)

(defun r (str)
 (let
  ((ast (cl-nl.lexer:lex str)))
  (format t "AST for ~S became ~S~%" str ast)
  ast))

(defun p (result) result)

(defun run ()
 (loop for str = (read-line)
       while str
       do (p (e (r str))))

 ;(format t "AH HA~%")
 )
