(in-package :kaleidoscope.chapter7)

;;; top-level

(defvar *execution-engine*)

(defun handle-definition ()
  (let ((function (parse-definition)))
    (if function
      (let ((lf (codegen function)))
        (when lf
          (format *output?* "Read function definition:")
          (write-string (llvm:print-value-to-string lf) *output?*)))
      (get-next-token))))

(defun handle-extern ()
  (let ((prototype (parse-extern)))
    (if prototype
      (let ((function (codegen prototype)))
        (when function
          (format *output?* "Read extern: ")
          (write-string (llvm:print-value-to-string function) *output?*)))
      (get-next-token))))

(defun handle-top-level-expression ()
  "Evaluate a top-level expression into an anonymous function."
  (handler-case 
      (let* ((lf (codegen (parse-top-level-expression)))
             (ptr (llvm:pointer-to-global *execution-engine* lf)))
        (format *output?* "Evaluated to ~fD0"
                ;; NOTE: The C version of the tutorial only has the JIT side
                ;;       of this, so if you have an interpreter, it breaks.
                (cond ((cffi:pointer-eq ptr lf) ; we have an interpreter
		       (print "no!" *output?*)
		       (llvm:generic-value-to-float
			(llvm:double-type)
			(llvm:run-function *execution-engine* ptr ())))
		      (t
		       (print "yes!" *output?*)
		       (cffi:foreign-funcall-pointer ptr () :double)))))
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

;;; "Library" functions that can be "extern'd" from user code.

;;; NOTE: These functions are defined in kaleidoscope-extern.c

;;; driver

;(defvar *myjit*)

(defun toplevel ()
 ; (llvm::initialize-native-target?)
 ; (llvm::initialize-native-Asm-parser)
 ; (llvm::initialize-native-asm-printer)
  (llvm:with-objects ((*builder* llvm:builder)
		      (*module* llvm:module "my cool jit")
		      (*execution-engine* llvm:execution-engine *module*)
		      ;(*myjit* llvm:jit-compiler *module*)
		      (*fpm* llvm:function-pass-manager *module*))    
    (llvm:add-target-data (llvm:target-data *execution-engine*) *fpm*)
    ;;passes    
    (progn
      (llvm:add-promote-memory-to-register-pass *fpm*)
      (llvm:add-instruction-combining-pass *fpm*)
      (llvm:add-reassociate-pass *fpm*)
      (llvm:add-gvn-pass *fpm*)
      (llvm:add-cfg-simplification-pass *fpm*))

    (llvm:initialize-function-pass-manager *fpm*)
     
    (with-chapter 7
      (reset-token-reader)
      (get-next-token)
      (set-binop-precedence)
      (format *output?* "~&ready> ")
      (callcc (function main-loop)))
    (dump-module *module*))
  (values))
