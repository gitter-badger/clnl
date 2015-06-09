(in-package #:cl-nl.transpiler)

; This is responsible for taking an ast and turning it into valid CL code
; targeting the nvm.  Here is where start to care about commands versus reporters
; and ensuring that things are in the right place.  The reason we wait until here
; is because we want to allow someone else to play with the AST before handing it off
; to us.  For instance, the command center wants to add "show" to reporters, and
; the users dictate based on entry point whether they are expecting a command
; or a reporter.  So monitors can say "hey, transpile this reporter" and we'll check
; to make sure it actually is.

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
 `(progn
   ,@(mapcar #'transpile-command parsed-ast)))

(defun transpile-command (command)
 (cond
  ((not (listp command)) (error "Expected a statement of some sort"))
  ((not (find-prim (car command))) (error "Couldn't find the command for ~S" (car command)))
  ((not (is-command (find-prim (car command)))) (error "Expected command, got ~S" (car command)))
  (t `(,(prim-func (find-prim (car command))) ,@(mapcar #'transpile-reporter (cdr command))))))

(defun transpile-reporter (reporter)
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
(defprim :ask :command cl-nl.nvm::ask)
(defprim :crt :command cl-nl.nvm::create-turtles)
(defprim :fd :command cl-nl.nvm::fd)
(defprim :random-float :reporter cl-nl.nvm::random-float)
(defprim :show :command cl-nl.nvm::show)
(defprim :turtles :reporter cl-nl.nvm::turtles)
