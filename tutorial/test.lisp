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

(defun wow (n)
  (let ((str (write-to-string n)))
    (values
     (concatenate 'string "chapter" str ".out")
     (concatenate 'string "chapter" str ".k")
     (lambda () (toplevel n)))))

(defun testfoo ()
  (loop for i from 2 to 7 do
       (test i)))

(defun test (&optional (n *chapter*))
  (multiple-value-bind (out in toplevel) (wow n)
    (%test out in toplevel n)))

(defun %test (ref in toplevel n)
  (let ((ref
	 (trim-empty-lines
	  (alexandria:read-file-into-string
	   (merge-pathnames ref *test-directory*))))
	(output
	 (trim-empty-lines
	  (with-output-to-string (stream)
	    (let ((all (make-broadcast-stream stream
					*standard-output*
					      )))
	      (let ((input-file-name (merge-pathnames in *test-directory*)))
		(print input-file-name)
		(with-open-file (file input-file-name)
		  (let ((*output?*  all)
			(*input?* file))
		    (funcall toplevel)))))))))
    (let ((line-number 0))
      (cond ((block out
	       (mapc 
		(lambda (x y)
		  (unless (string= x y)
		    (return-from out t))
		  (incf line-number))
		ref
		output)
	       nil)
	     (print-with-lines ref)
	     (print-with-lines output)
	     (format t "~&line # ~s different" line-number))
	    (t
	     (terpri)
	     (terpri)
	     (format *output?* "test ~a OK" n)
	     (terpri)
	     (terpri))))))
