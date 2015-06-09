(in-package #:cl-nl.nvm)

; This is the engine.  Yay.

(defvar *current-id* 0)

(defstruct turtle who color heading xcor ycor)
(defvar *turtles* nil)
(defvar *myself* nil)
(defvar *self* nil)

(defun show (n)
 (format t "Showing: ~A~%" (dump-object n)))

(defun create-turtle ()
 (setf
  *turtles*
  (nconc
   *turtles*
   (list
    (make-turtle :who *current-id*
                 :color (coerce (+ 5 (* 10 (cl-nl.random:next-int 14))) 'double-float)
                 :heading (coerce (cl-nl.random:next-int 360) 'double-float)
                 :xcor 0d0
                 :ycor 0d0))))
 (incf *current-id*))

(defun turtles () *turtles*)

(defun ask (agent-set fn)
 (let
  ((iter (shufflerator agent-set)))
  (loop for agent = (funcall iter)
        while agent
        do (let ((*myself* *self*) (*self* agent)) (funcall fn)))))

(defun shufflerator (agent-set)
 (let
  ((copy (copy-list agent-set))
   (i 0)
   (agent nil))
  (flet
   ((fetch ()
     (let
      ((idx (when (< i (1- (length copy))) (+ i (cl-nl.random:next-int (- (length copy) i))))))
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
 (cl-nl.random:next-double n))

(defun fd (n)
 (when (not (turtle-p *self*)) (error "Gotta call fd in turtle scope, dude (~A)" *self*))
 (setf (turtle-xcor *self*) (+ (turtle-xcor *self*) (* n (sin (* pi (/ (turtle-heading *self*) 180))))))
 (setf (turtle-ycor *self*) (+ (turtle-ycor *self*) (* n (cos (* pi (/ (turtle-heading *self*) 180)))))))

(defun create-turtles (n)
 (loop for i from 1 to n do (create-turtle)))

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
       (cl-ppcre:regex-replace "d-" (cl-ppcre:regex-replace "d0" output "") "E-"))))) ; Someday we'll have d<posint>, but this is not that day!
(defmethod dump-object ((o string)) o)

(defun export-world ()
 (format nil "~{~A~%~}"
  (list
   (format nil "~S" "RANDOM STATE")
   (format nil "~S" (cl-nl.random:export))
   ""
   (format nil "~S" "GLOBALS")
   "\"min-pxcor\",\"max-pxcor\",\"min-pycor\",\"max-pycor\",\"perspective\",\"subject\",\"nextIndex\",\"directed-links\",\"ticks\","
   (format nil "\"-1\",\"1\",\"-1\",\"1\",\"0\",\"nobody\",\"~A\",\"\"\"NEITHER\"\"\",\"-1\"" *current-id*)
   ""
   (format nil "~S" "TURTLES")
   "\"who\",\"color\",\"heading\",\"xcor\",\"ycor\",\"shape\",\"label\",\"label-color\",\"breed\",\"hidden?\",\"size\",\"pen-size\",\"pen-mode\""
   (format nil "~{~A~%~}"
    (mapcar
     (lambda (turtle)
      (format nil
       "\"~A\",\"~A\",\"~A\",\"~A\",\"~A\",\"\"\"default\"\"\",\"\"\"\"\"\",\"9.9\",\"{all-turtles}\",\"false\",\"1\",\"1\",\"\"\"up\"\"\""
       (turtle-who turtle)
       (dump-object (turtle-color turtle))
       (dump-object (turtle-heading turtle))
       (dump-object (turtle-xcor turtle))
       (dump-object (turtle-ycor turtle))
       ))
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
   ""
   )))
