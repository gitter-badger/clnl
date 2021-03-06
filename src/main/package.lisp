(defpackage #:clnl (:use :common-lisp)
 (:export :run :boot :run-commands :run-reporter)
 (:documentation
  "Main CLNL package

The entry point for general purpose clnl startup, as well as
the place that ties all the parts together into a cohesive whole."))

(defpackage #:clnl-parser
 (:use :common-lisp)
 (:export :parse)
 (:documentation
  "CLNL Parser

All the code to convert the list of tokens coming from the lexer
into an ast that can be transpiled later."))

(defpackage #:clnl-random
 (:use :common-lisp)
 (:shadow #:export)
 (:export #:export #:set-seed #:next-int #:next-double)
 (:documentation
  "Wrapper around mt19937.

mt19937 implements a merseinne twister that must be adapted a little in
order to match the implementation in the main NetLogo codebase which tries
to match how java.util.Random works.  Turtles, all the way down."))

(defpackage #:clnl-transpiler
 (:use :common-lisp)
 (:export :transpile-commands :transpile-reporter)
 (:documentation
  "CLNL Transpiler

The transpiler is responsible for taking an ast and turning it into valid CL code
targeting the nvm.  Here is where start to care about commands versus reporters
and ensuring that things are in the right place.  The reason we wait until here
is because we want to allow someone else to play with the AST before handing it off
to us.  For instance, the command center wants to add \"show\" to reporters, and
the users dictate based on entry point whether they are expecting a command
or a reporter.  So monitors can say \"hey, transpile this reporter\" and we'll check
to make sure it actually is.

Furthermore, the lisp code that any netlogo code would be transpiled to should
use exported symbols, such that anyone writing NetLogo code in lisp could use
the nvm in the same way that comes out of this transpiler
All the code to convert the list of tokens coming from the lexer
into an ast that can be transpiled later."))

(defpackage #:clnl-nvm
 (:use :common-lisp)
 (:export :export-world :create-world :current-state
  ; API as used by transpiled NetLogo programs
  #:ask
  #:create-turtles
  #:forward
  #:random-float
  #:show
  #:turtles)
 (:documentation
  "CLNL NVM

NetLogo Virtual Machine: the simulation engine."))

(defpackage #:clnl-lexer
 (:use :common-lisp)
 (:export :lex)
 (:documentation
  "CLNL Lexer

The primary code responsible for tokenizing NetLogo code."))

(defpackage #:clnl-interface
 (:use :common-lisp)
 (:export :run :export-view)
 (:documentation
  "CLNL Interface

The NetLogo view interface using opengl.  This is responsible for taking the
current state of the enging and displaying it.  Will not house any interface
components."))

(defpackage #:clnl-cli
 (:use :common-lisp :cl-charms/low-level)
 (:export :run)
 (:documentation
  "CLNL CLI

The main NetLogo interface for interacting with the program.  Since CLNL is
a command line interface program with a view for display purposes only, this
is where all the features that the traditional NetLogo UI lives."))
