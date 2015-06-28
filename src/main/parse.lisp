(in-package #:clnl-parser)

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
(defun prim-args (prim) (getf prim :args))

(defun find-prim (symb) (find symb *prims* :key #'prim-name))

; Make this only as complicated as it needs to be, letting it grow
; as we take on more and more of the language
(defun parse (lexed-ast)
 (cond
  ((not lexed-ast) nil)
  ((numberp (car lexed-ast)) (cons (coerce (car lexed-ast) 'double-float) (parse (cdr lexed-ast))))
  ((eql :[ (car lexed-ast)) (parse-block (cdr lexed-ast)))
  ((and (symbolp (car lexed-ast)) (find-prim (car lexed-ast)))
   (let*
    ((prim (find-prim (car lexed-ast)))
     (num-args (prim-num-args prim))
     (parsed-remainder (parse (cdr lexed-ast))))
    (cons
     (cons
      (prim-name prim)
      (mapcar
       #'help-arg
       (prim-args prim) 
       (butlast parsed-remainder (- (length parsed-remainder) num-args))))
     (nthcdr num-args parsed-remainder))))
  (t (error "Couldn't parse ~S" lexed-ast))))

(defun help-arg (arg-type arg)
 (case arg-type
  (:command-block
   (if (not (and (consp arg) (eql 'block (car arg))))
       (error "Required a block, but found a ~A" arg)
       (cons :command-block (cdr arg))))
  (t arg)))

(defun parse-block (tokens)
 (multiple-value-bind (in-block after-block) (find-closing-bracket tokens)
  (cons
   (cons
    'block
    (parse in-block))
   (parse after-block))))

(defun find-closing-bracket (tokens &optional (depth 0))
 (cond
  ((not tokens) (error "Failed to find a matching closing bracket"))
  ((and (eql :] (car tokens)) (= depth 0)) (values nil (cdr tokens)))
  (t (multiple-value-bind
      (in-block after-block)
      (find-closing-bracket (cdr tokens) (case (car tokens) (:[ (1+ depth)) (:] (1- depth)) (t depth)))
      (values (cons (car tokens) in-block) after-block)))))

(defmacro defprim (name args)
 `(push
   (list :name ,name :args ',args)
   *prims*))

; This list of prims will get combined with the mapping to actual code later
; Current list of argument types we accept:
; - :number
; - :agentset
; - :command-block
; - t - any type
(defprim :ask (:agentset :command-block))
(defprim :crt (:number))
(defprim :fd (:number))
(defprim :random-float (:number))
(defprim :show (t))
(defprim :turtles ())
