(defpackage :k-test
  (:use
   #:cl
   #:k-shared))
(in-package :k-test)

(defparameter *test-directory* (merge-pathnames "tests/" *this-directory*))
(defun trim-empty-lines (a)
  (delete-if (lambda (x)
	       (zerop (length x)))
	     (split-sequence:split-sequence #\Newline a)))

(defun print-with-lines (a)
  (let ((line-number 0))
    (dolist (item a)
      (print line-number)
      (princ item)
      (incf line-number))))


(defun testfoo (&optional (testfunc (function test)))
  (loop for i from 2 to 7 do
       (funcall testfunc i))
  (values))

(defun test (&optional (n *chapter*))
  (let ((str (write-to-string n)))
    (flet ((%test (ref in toplevel)
	     (with-output-to-string (stream)
	       (let ((all (make-broadcast-stream stream
						 *standard-output*
						 )))
		 (let ((input-file-name (merge-pathnames in *test-directory*)))
		   (with-open-file (file input-file-name)
		     (let ((*output?*  all)
			   (*input?* file))
		       (funcall toplevel))))))))
      (%test
       (concatenate 'string "chapter" str ".out")
       (concatenate 'string "chapter" str ".k")
       (lambda () (toplevel n)))))
  (values))
#+nil
(defun test2 (&optional (n *chapter*))
  (let ((str (write-to-string n)))
    (flet ((%test (out in toplevel)
	     (let ((k-shared::*ast2-stuff* nil))
	       (with-output-to-string (stream)
		 (let ((input-file-name (merge-pathnames in *test-directory*))
		       (output-file-name (merge-pathnames out *test-directory*)))
		   (with-open-file (file input-file-name)
		     (let ((*input?* file))
		       (funcall toplevel)))
		   (with-open-file (file output-file-name :direction :output :if-exists :append)
		     (let ((*print-case* :downcase))
		       (print `(define-chapter-test ,n ,(nreverse k-shared::*ast2-stuff*)) file))))))))
      (%test
       "testcases.lisp"
       (concatenate 'string "chapter" str ".k")
       (lambda () (toplevel n)))))
  (values))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *chapter-test-cases* (make-array 16)))

(defmacro define-chapter-test (chapter-number data)
  (setf (aref *chapter-test-cases* chapter-number) data)
  (values))
