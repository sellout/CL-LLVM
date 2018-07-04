(defpackage :k-test
  (:use #:cl)
  (:export
   #:*this-directory*))
(in-package :k-test)

(defparameter *this-directory* (filesystem-util:this-directory))

(defun trim-empty-lines (a)
  (let ((value
	 (delete-if (lambda (x)
		      (zerop (length x)))
		    (split-sequence:split-sequence #\Newline a)))
	(acc nil))
    (dolist (x value)
      (push x acc)
      (push "
" acc)
      )
    (apply #'concatenate 'string (nreverse acc))))

(defun test7 ()
  (let ((ref
	 (trim-empty-lines
	  (alexandria:read-file-into-string
	   (merge-pathnames *this-directory* "chapter7.out"))))
	(output
	 (trim-empty-lines
	  (with-output-to-string (stream)
	    (let ((all (make-broadcast-stream stream
					;*standard-output*
					      )))
	      (let ((input-file-name (merge-pathnames *this-directory* "chapter7.k")))
		(print input-file-name)
		(with-open-file (file input-file-name)
		  (kaleidoscope.chapter7:toplevel all file))))))))
    (print ref)
    (print output)
    (values)))
