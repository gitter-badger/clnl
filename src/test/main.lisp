(in-package #:cl-nl-test)

(defparameter *tests* nil)

(defun run-and-print-test (test)
 (let
  ((green (format nil "~c[1;32m" #\Esc))
   (red (format nil "~c[1;31m" #\Esc))
   (result (funcall (cadr test))))
  (format t "~A- ~S ~A~c[0m~%" (if result green red) (car test) (if result "passed" "failed") #\Esc)
  result))

(defun run-tests (tests)
 (let
  ((final-result t))
  (loop for test in tests
        for result = (run-and-print-test test)
        do (setf final-result (and final-result result)))
  final-result))

(defun run-all-tests ()
 (format t "~%Here we goooooooo~%")
 (run-tests (reverse *tests*)))
  
(defun run-tests-matching (match)
 (run-tests (remove-if-not (lambda (test-name) (cl-ppcre:scan (format nil "^~A$" match) test-name)) *tests* :key #'car)))

(defun find-test (name)
 (or
  (find name *tests* :test #'string= :key #'car)
  (error "Couldn't find test with name: ~A" name)))

(defun test-debug (name) (format t "----~%~A~%" (funcall (third (find-test name)))))
(defun test-scala-prog (name) (format t "----~%~A~%" (fourth (find-test name))))
(defun test-scala-input (name) (format t "----~%~A~%" (fifth (find-test name))))

(defmacro defsimpletest (name test-fn debug-fn scala-prog scala-input)
 `(progn
   ;(when (find-test ,name) (error "Test with name ~S already exists, abort, abort" ,name))
   (push
    (list ,name ,test-fn ,debug-fn ,scala-prog ,scala-input)
    *tests*)))

; To be used only with the simplest of tests, just a list of commands and a checksum of the
; world after they've been run.
(defmacro defsimplecommandtest (name commands checksum)
 `(defsimpletest
   ,name
   (lambda ()
    (cl-nl:boot)
    (cl-nl:run-commands ,commands)
    (string= ,checksum (checksum-world)))
   (lambda ()
    (cl-nl:boot)
    (cl-nl:run-commands ,commands)
    (format nil "~A~A"
     (cl-nl.nvm:export-world)
     (checksum-world)))
   "bin/runcmd.scala"
   (format nil "~A~%" ,commands)))

(defmacro defsimplereportertest (name reporter value checksum)
 `(defsimpletest
   ,name
   (lambda ()
    (cl-nl:boot)
    (and
     (string= (cl-nl.nvm:dump-object (cl-nl:run-reporter ,reporter)) ,value)
     (string= ,checksum (checksum-world))))
   (lambda ()
    (cl-nl:boot)
    (format nil "~A~%~A~A"
     (cl-nl.nvm:dump-object (cl-nl:run-reporter ,reporter))
     (cl-nl.nvm:export-world)
     (checksum-world)))
   "bin/runreporter.scala"
   (format nil "~A~%" ,reporter)))

(defun checksum-world ()
 (format nil "~{~2,'0X~}"
  (map 'list #'identity
   (ironclad:digest-sequence
    :sha1
    (map '(vector (unsigned-byte 8)) #'char-code (cl-nl.nvm:export-world))))))

(defun run ()
 (loop for str = (progn (format t "> ") (force-output) (read-line))
       while str
       do (progn (asdf:load-system :cl-nl-test) (run-tests-matching str))))
