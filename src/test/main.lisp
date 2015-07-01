(in-package #:clnl-test)

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
 (format t "~%~c[1;33mHere we goooooooo~c[0m~%" #\Esc #\Esc)
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
    (clnl:boot)
    (clnl:run-commands ,commands)
    (string= ,checksum (checksum-world)))
   (lambda ()
    (clnl:boot)
    (clnl:run-commands ,commands)
    (format nil "~A~A"
     (clnl-nvm:export-world)
     (checksum-world)))
   "bin/runcmd.scala"
   (format nil "~A~%" ,commands)))

(defmacro defsimplereportertest (name reporter value checksum)
 `(defsimpletest
   ,name
   (lambda ()
    (clnl:boot)
    (and
     (string= (clnl-nvm:dump-object (clnl:run-reporter ,reporter)) ,value)
     (string= ,checksum (checksum-world))))
   (lambda ()
    (clnl:boot)
    (format nil "~A~%~A~A"
     (clnl-nvm:dump-object (clnl:run-reporter ,reporter))
     (clnl-nvm:export-world)
     (checksum-world)))
   "bin/runreporter.scala"
   (format nil "~A~%" ,reporter)))

(defun checksum-world ()
 (format nil "~{~2,'0X~}"
  (map 'list #'identity
   (ironclad:digest-sequence
    :sha1
    (map '(vector (unsigned-byte 8)) #'char-code (clnl-nvm:export-world))))))

(defun run ()
 (loop for str = (progn (format t "> ") (force-output) (read-line))
       while str
       do (progn (asdf:load-system :clnl-test) (run-tests-matching str))))
