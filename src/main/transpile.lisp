(in-package #:clnl-transpiler)

(defparameter *prims* nil)

(defun prim-name (prim) (getf prim :name))
(defun prim-type (prim) (getf prim :type))
(defun prim-func (prim) (getf prim :func))
(defun is-reporter (prim) (eql :reporter (getf prim :type)))
(defun is-command (prim) (eql :command (getf prim :type)))

(defun find-prim (symb) (find symb *prims* :key #'prim-name))

; Let this grow, slowly but surely, eventually taking on calling context, etc.
; For now, it's just a
(defun transpile-commands (parsed-ast)
 "TRANSPILE-COMMANDS PARSED-AST => AST

ARGUMENTS AND VALUES:

  PARSED-AST: An ast as returned by the parser
  AST: An common lisp AST that can be actually run in a common lisp instance

DESCRIPTION:

  TRANSPILE-COMMANDS takes a unambigious PARSED-AST and converts it to
  Common Lisp code.

  Calling eval on that code should work correctly as long as you have a
  running engine.  This is the entry point for commands, so it does
  extra checking to ensure that commands are actually in the PARSED-AST."
 `(progn
   ,@(mapcar #'transpile-command parsed-ast)))

(defun transpile-command (command)
 (cond
  ((not (listp command)) (error "Expected a statement of some sort"))
  ((not (find-prim (car command))) (error "Couldn't find the command for ~S" (car command)))
  ((not (is-command (find-prim (car command)))) (error "Expected command, got ~S" (car command)))
  (t `(,(prim-func (find-prim (car command))) ,@(mapcar #'transpile-reporter (cdr command))))))

(defun transpile-reporter (reporter)
 "TRANSPILE-REPORTER REPORTER => AST

ARGUMENTS AND VALUES:

  REPORTER: An ast returned from the parser.
  AST: An common lisp AST that can be actually run in a common lisp instance

DESCRIPTION:

  TRANSPILE-REPORTER takes a unambigious PARSED-AST and converts it to
  Common Lisp code.

  Calling eval on that code should work correctly as long as you have a
  running engine.  This is the entry point for reporters, so it does
  extra checking to ensure that the reporter is actually in the REPORTER.

  The Common lisp code that is returned, when run, will return some value."
 (cond
  ((numberp reporter) reporter) ; The parser converts to double for us
  ((symbolp reporter) reporter) ; The parser should have checked that having a symbol here is ok
  ((not (listp reporter)) (error "Expected a statement of some sort"))
  ((eql :command-block (car reporter)) (transpile-command-block reporter))
  ((not (find-prim (car reporter))) (error "Couldn't find the reporter for ~S" (car reporter)))
  ((not (is-reporter (find-prim (car reporter)))) (error "Expected reporter, got ~S" (car reporter)))
  (t `(,(prim-func (find-prim (car reporter))) ,@(mapcar #'transpile-reporter (cdr reporter))))))

(defun transpile-command-block (block)
 `(lambda () ,@(mapcar #'transpile-command (cdr block))))

(defmacro defprim (name type nvm-func)
 `(push
   (list :name ,name :type ,type :func ',nvm-func)
   *prims*))

; We count on the parser to handle arguemnts for us, when collating things.
(defprim :ask :command clnl-nvm:ask)
(defprim :crt :command clnl-nvm:create-turtles)
(defprim :fd :command clnl-nvm:forward)
(defprim :random-float :reporter clnl-nvm:random-float)
(defprim :show :command clnl-nvm:show)
(defprim :turtles :reporter clnl-nvm:turtles)
