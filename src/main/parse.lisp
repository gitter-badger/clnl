(in-package #:cl-nl.parser)

; Ok, after thinking about this a little, the parser is completely contextual
; based on what has come before.  We can't do a contextless parsing, like we
; could in other languages, due to amiguity about reporters vs reporter tasks
;
; So, for instance, we could have:
;   x + y => (+ x y)
;   x + y => (x (task +) y)
; So the definition of "+" is completely dependent on the nature of x
;
; The goal of this parser should be to turn in the amiguous lexed ast representing
; NetLogo into an unambigious S-expression, and nothing more, so things like
; expectation of commands being the first symbol is not be necessary until later
;
; In general, the parser will:
;  * Parse the structure of the lexed output first
;  * Parse the structure of the individual expressions (finding ('s and ['s and doing the right thing)
;  * Coalate things into an unambigious expressions
;  * Then we're done, let someone else make it evaluatable
;    - We don't really care if things are commands or reporters right now

(defparameter *prims* nil)

(defun prim-name (prim) (getf prim :name))
(defun prim-num-args (prim) (length (getf prim :args)))

(defun find-prim (symb) (find symb *prims* :key #'prim-name))

; We don't care if it's a command!
;(defun is-command (symb)
; (let
;  ((prim (find-prim symb)))
; (and prim (eql :command (getf prim :type)))))
  
; Make this only as complicated as it needs to be, letting it grow
; as we take on more and more of the language
(defun parse (lexed-ast)
 (cond
  ((not lexed-ast) nil)
  ((numberp (car lexed-ast)) (cons (coerce (car lexed-ast) 'double-float) (parse (cdr lexed-ast))))
  ((and (symbolp (car lexed-ast)) (find-prim (car lexed-ast)))
   (let*
    ((prim (find-prim (car lexed-ast)))
     (num-args (prim-num-args prim))
     (parsed-remainder (parse (cdr lexed-ast))))
    (cons
     (cons
      (prim-name prim)
      (butlast parsed-remainder (- (length parsed-remainder) num-args)))
     (nthcdr num-args parsed-remainder))))
  (t (error "Couldn't parse ~S" lexed-ast))))

(defmacro defprim (name args)
 `(push
   (list :name ,name :args ',args)
   *prims*))

; This list of prims will get combined with the mapping to actual code later
(defprim :crt (:number))
