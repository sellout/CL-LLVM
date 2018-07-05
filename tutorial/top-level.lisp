(in-package :k-shared)

(defun handle-definition ()
  (let ((function (parse-definition)))
    (if function
	(ecase *chapter*
	  ((2)
	   (format *output?* "Parsed a function definition~%"))
	  ((3 4 5 6 7)
	   (let ((lf (codegen function)))
	     (when lf
	       (format *output?* "Read function definition:")
	       (dump-value lf)))))
	(get-next-token))))

(defun handle-extern ()
  (let ((prototype (parse-extern)))
    (if prototype
	(ecase *chapter*
	  ((2)
	   (format *output?* "Parsed an extern~%"))
	  ((3 4 5 6 7)
	   (let ((function (codegen prototype)))
	     (when function
	       (format *output?* "Read extern: ")
	       (dump-value function)))))
	(get-next-token))))

(defun handle-top-level-expression ()
  "Evaluate a top-level expression into an anonymous function."
  (handler-case
      (ecase *chapter*
	((2)
	 (progn (parse-top-level-expression)
		(format *output?* "Parsed a top-level expr~%")))
	((3 4 5 6 7)
	 (let ((lf (codegen (parse-top-level-expression))))
	   (case *chapter*
	     ((3)
	      (format *output?* "Read top-level expression:")
	      (dump-value lf))
	     ((4 5 6 7)
	      (let ((ptr (llvm:pointer-to-global *execution-engine* lf)))
		(case *chapter*
		  ((4 5)
		   (dump-value lf)))
		(format *output?* "Evaluated to ~fD0"
			;; NOTE: The C version of the tutorial only has the JIT side
			;;       of this, so if you have an interpreter, it breaks.
			(if (cffi:pointer-eq ptr lf)        ; we have an interpreter
			    (llvm:generic-value-to-float
			     (llvm:double-type)
			     (llvm:run-function *execution-engine* ptr ()))
			    (cffi:foreign-funcall-pointer ptr () :double)))))
	     ))))
    (kaleidoscope-error (e)
      (get-next-token)
      (format *output?* "error: ~a~%" e))))

(defun main-loop (exit)
  (do ()
      ((main-loop-end))
    (per-loop exit)))
(defun main-loop-end ()
  (eql *current-token* ':tok-eof))
(defun per-loop (exit)
  (format *output?* "~&ready> ")
  (handler-case (case *current-token*
		  (#\; (get-next-token))
		  (:tok-def (handle-definition))
		  (:tok-extern (handle-extern))
		  (:tok-quit (funcall exit))
		  (otherwise (handle-top-level-expression)))
    (kaleidoscope-error (e) (format *output?* "error: ~a~%" e))))

;;; top-level 4 5 6 7
(defvar *execution-engine*)
