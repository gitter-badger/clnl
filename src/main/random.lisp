(defpackage #:cl-nl.random
 (:use :common-lisp)
 (:export :set-seed :next-int :next-double))

(in-package #:cl-nl.random)

; This is a wrapper around the very nice mersenne twister mt19937 to match
; NetLogo's implementation that tries to match how java.util.Random works
 
(defun set-seed (n)
 (setf mt19937:*random-state* (mt19937::make-random-object :state (mt19937:init-random-state n))))

(defun next-int (n)
 (rem (ash (mt19937:random-chunk mt19937:*random-state*) -1) n))

(defun next-double (&optional (n 1d0))
 (let
  ((y (mt19937:random-chunk mt19937:*random-state*))
   (z (mt19937:random-chunk mt19937:*random-state*)))
 (*
  (/
   (+ (ash (ash y -6) 27) (ash z -5))
   (coerce (ash 1 53) 'double-float))
  n)))
