(in-package #:cl-nl)

(defun e (ast) ast)

(defun r (str)
 (let*
  ((lexed-ast (let ((ast (cl-nl.lexer:lex str))) (format t "Via lexing, AST for ~S became ~S~%" str ast) ast))
   (parsed-ast (let ((ast (cl-nl.parser:parse lexed-ast))) (format t "Via parsing, AST for ~S became ~S~%" lexed-ast ast) ast))
   (transpiled-ast (let ((ast (cl-nl.transpiler:transpile-commands parsed-ast))) (format t "Via transpiling, AST for ~S became ~S~%" parsed-ast ast) ast)))
  (eval transpiled-ast)))

(defun p (result) result)

(defun run ()
 (loop for str = (progn (format t "> ") (force-output) (read-line))
       while str
       do (p (e (r str)))))

(defun boot ()
 (cl-nl.random:set-seed 15)
 (cl-nl.nvm:create-world)
 )

(defun run-commands (cmds)
 (eval (cl-nl.transpiler:transpile-commands (cl-nl.parser:parse  (cl-nl.lexer:lex cmds)))))
