(in-package #:clnl)

(defun e (ast) ast)

(defun r (str)
 (let*
  ((lexed-ast (let ((ast (clnl-lexer:lex str)))
               (format t "Via lexing, AST for~%~S~% became~%~S~%~%" str ast) ast))
   (parsed-ast (let ((ast (clnl-parser:parse lexed-ast)))
                (format t "Via parsing, AST for~%~S~% became~%~S~%~%" lexed-ast ast) ast))
   (transpiled-ast (let ((ast (clnl-transpiler:transpile-commands parsed-ast)))
                    (format t "Via transpiling, AST for~%~S~% became~%~S~%" parsed-ast ast) ast)))
  (eval transpiled-ast)))

(defun p (result) result)

(defun run ()
 (loop
  :for str := (progn (format t "> ") (force-output) (read-line))
  :while str
  :do (p (e (r str))))
 (sb-ext:exit))

(defun boot ()
 (clnl-random:set-seed 15)
 (clnl-nvm:create-world))

(defun run-commands (cmds)
 (eval (clnl-transpiler:transpile-commands (clnl-parser:parse (clnl-lexer:lex cmds)))))

(defun run-reporter (reporter)
 (eval (clnl-transpiler:transpile-reporter (car (clnl-parser:parse (clnl-lexer:lex reporter))))))
