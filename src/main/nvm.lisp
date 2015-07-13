(in-package #:clnl-nvm)

; This is the engine.  Yay.

(defvar *current-id* 0)

(defstruct turtle who color heading xcor ycor)
(defvar *turtles* nil)
(defvar *myself* nil)
(defvar *self* nil)

(defun show (n)
 "Prints value in the Command Center, preceded by this agent, and followed by a carriage return.

See http://ccl.northwestern.edu/netlogo/docs/dictionary.html#show"
 (format t "Showing: ~A~%" (dump-object n)))

(defun create-turtle ()
 (setf
  *turtles*
  (nconc
   *turtles*
   (list
    (make-turtle
     :who *current-id*
     :color (coerce (+ 5 (* 10 (clnl-random:next-int 14))) 'double-float)
     :heading (coerce (clnl-random:next-int 360) 'double-float)
     :xcor 0d0
     :ycor 0d0))))
 (incf *current-id*))

(defun turtles ()
 "Reports the agentset consisting of all turtles. 

See http://ccl.northwestern.edu/netlogo/docs/dictionary.html#turtles"
 *turtles*)

(defun ask (agent-set fn)
 "The specified agent or agentset runs the given commands.

See http://ccl.northwestern.edu/netlogo/docs/dictionary.html#ask"
 (let
  ((iter (shufflerator agent-set)))
  (loop
   :for agent := (funcall iter)
   :while agent
   :do (let ((*myself* *self*) (*self* agent)) (funcall fn)))))

(defun shufflerator (agent-set)
 (let
  ((copy (copy-list agent-set))
   (i 0)
   (agent nil))
  (flet
   ((fetch ()
     (let
      ((idx (when (< i (1- (length copy))) (+ i (clnl-random:next-int (- (length copy) i))))))
      (when idx (setf agent (nth idx copy)))
      (when idx (setf (nth idx copy) (nth i copy)))
      (incf i))))
   (fetch) ; we pre-fetch because netlogo does, rng sync hype!
   (lambda ()
    (cond
     ((> i (length copy)) nil)
     ((= i (length copy)) (incf i) (car (last copy)))
     (t (let ((result agent)) (fetch) result)))))))

(defun random-float (n)
 "If number is positive, returns a random floating point number greater than
or equal to 0 but strictly less than number.

If number is negative, returns a random floating point number less than or equal
to 0, but strictly greater than number.

If number is zero, the result is always 0. 

See http://ccl.northwestern.edu/netlogo/docs/dictionary.html#random-float"
 (clnl-random:next-double n))

(defun forward (n)
 "The turtle moves forward by number steps, one step at a time. (If number is
negative, the turtle moves backward.) 

See http://ccl.northwestern.edu/netlogo/docs/dictionary.html#forward"
 (when (not (turtle-p *self*)) (error "Gotta call fd in turtle scope, dude (~A)" *self*))
 (setf (turtle-xcor *self*) (+ (turtle-xcor *self*) (* n (sin (* pi (/ (turtle-heading *self*) 180))))))
 (setf (turtle-ycor *self*) (+ (turtle-ycor *self*) (* n (cos (* pi (/ (turtle-heading *self*) 180)))))))

(defun create-turtles (n)
 "Creates number new turtles at the origin. New turtles have random integer
headings and the color is randomly selected from the 14 primary colors.

If commands are supplied, the new turtles immediately run them.

See http://ccl.northwestern.edu/netlogo/docs/dictionary.html#create-turtles"
 (loop :for i :from 1 :to n :do (create-turtle)))

(defun create-world ()
 (setf *turtles* nil)
 (setf *current-id* 0))

; These match netlogo's dump
(defgeneric dump-object (o))

(defmethod dump-object ((n double-float))
 (multiple-value-bind (int rem) (floor n)
  (if (eql 0d0 rem)
   (format nil "~A" int)
   (let
    ((output (format nil "~D" n)))
    ; Someday we'll have d<posint>, but this is not that day!
    (cl-ppcre:regex-replace "d-" (cl-ppcre:regex-replace "d0" output "") "E-")))))

(defmethod dump-object ((o string)) o)

(defun export-world ()
 (format nil "~{~A~%~}"
  (list
   (format nil "~S" "RANDOM STATE")
   (format nil "~S" (clnl-random:export))
   ""
   (format nil "~S" "GLOBALS")
   (format nil "~A~A"
    "\"min-pxcor\",\"max-pxcor\",\"min-pycor\",\"max-pycor\",\"perspective\",\"subject\","
    "\"nextIndex\",\"directed-links\",\"ticks\",")
   (format nil "\"-1\",\"1\",\"-1\",\"1\",\"0\",\"nobody\",\"~A\",\"\"\"NEITHER\"\"\",\"-1\"" *current-id*)
   ""
   (format nil "~S" "TURTLES")
   (format nil "~A~A"
    "\"who\",\"color\",\"heading\",\"xcor\",\"ycor\",\"shape\",\"label\",\"label-color\","
    "\"breed\",\"hidden?\",\"size\",\"pen-size\",\"pen-mode\"")
   (format nil "~{~A~%~}"
    (mapcar
     (lambda (turtle)
      (format nil
       "\"~A\",\"~A\",\"~A\",\"~A\",\"~A\",~A"
       (turtle-who turtle)
       (dump-object (turtle-color turtle))
       (dump-object (turtle-heading turtle))
       (dump-object (turtle-xcor turtle))
       (dump-object (turtle-ycor turtle))
       "\"\"\"default\"\"\",\"\"\"\"\"\",\"9.9\",\"{all-turtles}\",\"false\",\"1\",\"1\",\"\"\"up\"\"\""))
     *turtles*))
   (format nil "~S" "PATCHES")
   "\"pxcor\",\"pycor\",\"pcolor\",\"plabel\",\"plabel-color\""
   "\"-1\",\"1\",\"0\",\"\"\"\"\"\",\"9.9\""
   "\"0\",\"1\",\"0\",\"\"\"\"\"\",\"9.9\""
   "\"1\",\"1\",\"0\",\"\"\"\"\"\",\"9.9\""
   "\"-1\",\"0\",\"0\",\"\"\"\"\"\",\"9.9\""
   "\"0\",\"0\",\"0\",\"\"\"\"\"\",\"9.9\""
   "\"1\",\"0\",\"0\",\"\"\"\"\"\",\"9.9\""
   "\"-1\",\"-1\",\"0\",\"\"\"\"\"\",\"9.9\""
   "\"0\",\"-1\",\"0\",\"\"\"\"\"\",\"9.9\""
   "\"1\",\"-1\",\"0\",\"\"\"\"\"\",\"9.9\""
   ""
   (format nil "~S" "LINKS")
   "\"end1\",\"end2\",\"color\",\"label\",\"label-color\",\"hidden?\",\"breed\",\"thickness\",\"shape\",\"tie-mode\""
   "")))
