(in-package :kaleidoscope.chapter3)

;;; top-level 

(defun handle-definition ()
  (let ((function (parse-definition)))
    (if function
        (let ((lf (codegen function)))
          (when lf
            (format *output?* "Read function definition:")
            (dump-value lf)))
        (get-next-token))))

(defun handle-extern ()
  (let ((prototype (parse-extern)))
    (if prototype
        (let ((function (codegen prototype)))
          (when function
            (format *output?* "Read extern: ")
            (dump-value function)))
        (get-next-token))))

(defun handle-top-level-expression ()
  "Evaluate a top-level expression into an anonymous function."
  (handler-case 
      (let ((lf (codegen (parse-top-level-expression))))
        (format *output?* "Read top-level expression:")
        (dump-value lf))
    (kaleidoscope-error (e)
      (get-next-token)
      (format *output?* "error: ~a~%" e))))

(defun main-loop (exit)
  (do () ((eql *current-token* ':tok-eof))
    (format *output?* "~&ready> ")
    (handler-case (case *current-token*
                    (#\; (get-next-token))
                    (:tok-def (handle-definition))
                    (:tok-extern (handle-extern))
		    (:tok-quit (funcall exit))
                    (otherwise (handle-top-level-expression)))
      (kaleidoscope-error (e) (format *output?* "error: ~a~%" e)))))

;;; "Library" functions that can be "extern'd" from user code.

;;; NOTE: These functions are defined in kaleidoscope-extern.c

;;; driver

(defun toplevel ()
  (llvm:with-objects ((*builder* llvm:builder)
                      (*module* llvm:module "my cool jit"))
    (with-chapter 3
      (reset-token-reader)
      (get-next-token)
      (set-binop-precedence)
      (format *output?* "~&ready> ")
      (callcc (function main-loop)))
    (dump-module *module*))
  (values))
