(in-package :kaleidoscope.chapter4)

;;; top-level

(defvar *execution-engine*)

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
      (let* ((lf (codegen (parse-top-level-expression)))
             (ptr (llvm:pointer-to-global *execution-engine* lf)))
        (dump-value lf)
        (format *output?* "Evaluated to ~f"
                ;; NOTE: The C version of the tutorial only has the JIT side
                ;;       of this, so if you have an interpreter, it breaks.
                (if (cffi:pointer-eq ptr lf) ; we have an interpreter
                    (llvm:generic-value-to-float
                     (llvm:double-type)
                     (llvm:run-function *execution-engine* ptr ()))
                    (cffi:foreign-funcall-pointer ptr () :double))))
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
                      (*module* llvm:module "my cool jit")
                      (*execution-engine* llvm:execution-engine *module*)
                      (*fpm* llvm:function-pass-manager *module*))
    (llvm:add-target-data (llvm:target-data *execution-engine*) *fpm*)
    (progn
      (llvm:add-instruction-combining-pass *fpm*)
      (llvm:add-reassociate-pass *fpm*)
      (llvm:add-gvn-pass *fpm*)
      (llvm:add-cfg-simplification-pass *fpm*))
    (llvm:initialize-function-pass-manager *fpm*)

    (with-chapter 4
      (reset-token-reader)
      (get-next-token)
      (set-binop-precedence)
      (format *output?* "~&ready> ")
      (callcc (function main-loop)))
    (dump-module *module*))
  (values))
