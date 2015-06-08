(in-package #:cl-nl.nvm)

; This is the engine.  Yay.

(defvar *current-id* 0)

(defstruct turtle who color heading)
(defvar *turtles* nil)

(defun show (n)
 (format t "Showing: ~A~%" n))

(defun create-turtle ()
 (push
  (make-turtle :who *current-id*
               :color (coerce (+ 5 (* 10 (cl-nl.random:next-int 14))) 'double-float)
               :heading (coerce (cl-nl.random:next-int 360) 'double-float))
  *turtles*)
 (incf *current-id*))

(defun create-turtles (n)
 (loop for i from 1 to n do (create-turtle)))

(defun create-world ()
 (setf *turtles* nil)
 (setf *current-id* 0))

(defun format-num (n)
 (multiple-value-bind (int rem) (floor n)
  (if (eql 0d0 rem)
      (format nil "~A" int)
      (format nil "~F" n))))

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
       "\"~A\",\"~A\",\"~A\",\"0\",\"0\",\"\"\"default\"\"\",\"\"\"\"\"\",\"9.9\",\"{all-turtles}\",\"false\",\"1\",\"1\",\"\"\"up\"\"\""
       (turtle-who turtle)
       (format-num (turtle-color turtle))
       (format-num (turtle-heading turtle))))
     (reverse *turtles*)))
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
