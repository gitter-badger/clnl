(in-package #:clnl-random)

; This is a wrapper around the very nice mersenne twister mt19937 to match
; NetLogo's implementation that tries to match how java.util.Random works
 
(defun set-seed (n)
 (setf mt19937:*random-state* (mt19937::make-random-object :state (mt19937:init-random-state n))))

(defun next-int (n)
 (if
  (= n (logand n (- n) ))
  (ash (* n (ash (mt19937:random-chunk mt19937:*random-state*) -1) ) -31)
  (rem (ash (mt19937:random-chunk mt19937:*random-state*) -1) n)))

(defun next-double (&optional (n 1d0))
 (let
  ((y (mt19937:random-chunk mt19937:*random-state*))
   (z (mt19937:random-chunk mt19937:*random-state*)))
 (*
  (/
   (+ (ash (ash y -6) 27) (ash z -5))
   (coerce (ash 1 53) 'double-float))
  n)))

; Oh, export world, you WILL be mine
(defun export ()
 (let
  ((state
    (map
     'list
     (lambda (x) (if (logbitp (1- 32) x) (dpb x (byte 32 0) -1) x))
     (mt19937::random-state-state mt19937:*random-state*))))
  (format nil "0 ~A ~A ~A 0.0 false ~{~A~^ ~}"
   (first state) (second state) (third state)
   (nthcdr 3 state))))
