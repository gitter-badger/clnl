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
 "RUN => RESULT

ARGUMENTS AND VALUES:

  RESULT: undefined, the system terminates at the end of the loop

DESCRIPTION:

  RUN implements a very simple REPL."
 (loop
  :for str := (progn (format t "> ") (force-output) (read-line))
  :while str
  :do (p (e (r str))))
 (sb-ext:exit))

(defun boot ()
 "BOOT => RESULT

ARGUMENTS AND VALUES:

  RESULT: undefined

DESCRIPTION:

  BOOT does exactly that, boots the clnl system in a clean state.  The seed
  is set so that multiple runs will evaluate to the same."
 (clnl-random:set-seed 15)
 (clnl-nvm:create-world))

(defun run-commands (cmds)
 "RUN-COMMANDS CMDS => RESULT

ARGUMENTS AND VALUES:

  CMDS: A string that may have one more NetLogo commands
  RESULT: undefined

DESCRIPTION:

  RUN-COMMANDS will take NetLogo commands, put them through the various
  stages need to turn them into Common Lisp code, and run it."
 (eval (clnl-transpiler:transpile-commands (clnl-parser:parse (clnl-lexer:lex cmds)))))

(defun run-reporter (reporter)
 "RUN-REPORTER REPORTER => RESULT

ARGUMENTS AND VALUES:

  REPORTER: A string that should have only one reporter
  RESULT: The value reported by the NVM

DESCRIPTION:

  RUN-REPORTER will take a NetLogo REPORTER, put it through the various
  stages need to turn them into Common Lisp code, run it, and return the RESULT."
 (eval (clnl-transpiler:transpile-reporter (car (clnl-parser:parse (clnl-lexer:lex reporter))))))
