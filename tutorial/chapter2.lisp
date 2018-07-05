(in-package :kaleidoscope.chapter2)

;;; top-level

(defun handle-definition ()
  (if (parse-definition)
      (format *output?* "Parsed a function definition~%")
      (get-next-token)))

(defun handle-extern ()
  (if (parse-extern)
      (format *output?* "Parsed an extern~%")
      (get-next-token)))

(defun handle-top-level-expression ()
  "Evaluate a top-level expression into an anonymous function."
  (handler-case 
      (progn (parse-top-level-expression)
             (format *output?* "Parsed a top-level expr~%"))
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

;;; driver

(defun toplevel ()
  ;; install standard binary operators
  ;; 1 is lowest precedence
  (setf (gethash #\< *binop-precedence*) 10
        (gethash #\+ *binop-precedence*) 20
        (gethash #\- *binop-precedence*) 30
        (gethash #\* *binop-precedence*) 40)
  (reset-token-reader)
  (format *output?* "~&ready> ")
  (with-chapter 2
    (get-next-token)
    (callcc (function main-loop)))
  (values))
