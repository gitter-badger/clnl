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

(defun find-test (name) (find name *tests* :test #'string= :key #'car))

(defun diagnose-test (name)
 (when (not (find-test name)) (error "Couldn't find test with name: ~A" name))
 (format t "----~%~A~%" (funcall (caddr (find-test name)))))

(defun test-commands (name)
 (let
  ((test (find-test name)))
  (when (not test) (error "Couldn't find test with name: ~A" name))
  (format t "----~%")
  (format t "~A~%" (funcall (fourth test)))))

; To be used only with the simplest of tests, just a list of commands and a checksum of the
; world after they've been run.
(defmacro defsimpletest (name commands checksum)
 `(progn
   ;(when (find-test ,name) (error "Test with name ~S already exists, abort, abort" ,name))
   (push
    (list
     ,name
     (lambda ()
      (cl-nl:boot)
      (cl-nl:run-commands ,commands)
      (string= ,checksum (checksum-world)))
     (lambda ()
      (cl-nl:boot)
      (cl-nl:run-commands ,commands)
      (cl-nl.nvm:export-world)
      )
     (lambda () ,commands))
   *tests*)))

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
