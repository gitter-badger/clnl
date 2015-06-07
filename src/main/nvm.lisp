(in-package #:cl-nl.nvm)

; This is the engine.  Yay.

(defun create-turtles (n)
 (format t "HELLO WORLD ~A~%" n))

(defun export-world ()
 (format nil "~{~A~%~}"
  (list
   (format nil "~S" "RANDOM STATE")
   (format nil "~S" (cl-nl.random:export))
   ""
   (format nil "~S" "GLOBALS")
   "\"min-pxcor\",\"max-pxcor\",\"min-pycor\",\"max-pycor\",\"perspective\",\"subject\",\"nextIndex\",\"directed-links\",\"ticks\","
   "\"-1\",\"1\",\"-1\",\"1\",\"0\",\"nobody\",\"0\",\"\"\"NEITHER\"\"\",\"-1\""
   ""
   (format nil "~S" "TURTLES")
   "\"who\",\"color\",\"heading\",\"xcor\",\"ycor\",\"shape\",\"label\",\"label-color\",\"breed\",\"hidden?\",\"size\",\"pen-size\",\"pen-mode\""
   ""
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
