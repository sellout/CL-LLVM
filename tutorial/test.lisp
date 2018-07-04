(defpackage :k-test
  (:use
   #:cl
   #:k-shared))
(in-package :k-test)

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
     (symbol-function
      (find-symbol "TOPLEVEL"
		   (find-package
		    (concatenate 'string "KALEIDOSCOPE.CHAPTER" str)))))))

(defun testfoo ()
  (loop for i from 2 to 7 do
       (test i)))

(defun test (n)
  (multiple-value-bind (out in toplevel) (wow n)
    (%test out in toplevel)))

(defun %test (ref in toplevel)
  (let ((ref
	 (trim-empty-lines
	  (alexandria:read-file-into-string
	   (merge-pathnames *this-directory* ref))))
	(output
	 (trim-empty-lines
	  (with-output-to-string (stream)
	    (let ((all (make-broadcast-stream stream
					;*standard-output*
					      )))
	      (let ((input-file-name (merge-pathnames *this-directory* in)))
		(print input-file-name)
		(with-open-file (file input-file-name)
		  (let ((*output?*  all)
			(*input?* file))
		    (funcall toplevel)))))))))
    (print-with-lines ref)
    (print-with-lines output)
    (let ((line-number 0))
      (when 
	  (block out
	    (mapc 
	     (lambda (x y)
	       (incf line-number)
	       (unless (string= x y)
		 (return-from out t)))
	     ref
	     output))
	(format t "~&line # ~s different" line-number)))))
