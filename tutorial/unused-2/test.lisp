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
