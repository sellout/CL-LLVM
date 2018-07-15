(defpackage #:k-shared
  (:use
   #:cl
   #:utility)
  (:export
   #:*output?*
   #:*input?*)
  (:export
   #:callcc)
  (:export
   #:dump-value
   #:dump-module)
  (:export
   #:*this-directory*)
  (:export
   #:chap-package
   #:with-chapter)

  ;;;;ast
  (:export
   :expression
   :number-expression
   :value
   :variable-expression
   :name
   :binary-expression
   :operator
   :lhs
   :rhs
   :call-expression
   :callee
   :arguments
   :function-definition
   :prototype
   :body
   :arguments
   :precedence
   :if-expression
   :_condition
   :then
   :else
   :for-expression
   :var-name
   :start
   :end
   :step
   :body
   :step*
   :unary-expression
   :opcode
   :operand
   :unary-operator-p
   :binary-operator-p
   :operator-name
   :var-expression
   :var-names
   )

  ;;;;lexer
  (:export
   :*identifier-string*
   :*number-value*
   :*current-token*
   :*token-types*
   :get-next-token
   :reset-token-reader
   :chap-tokens
   :with-tokens)

  ;;;;parser
  (:export
   :*binop-precedence*
   :get-precedence
   :parse-identifier-expression
   :parse-number-expression
   :parse-paren-expression
   :parse-primary
   :parse-prototype
   :parse-bin-op-rhs
   :parse-expression
   :parse-definition
   :parse-top-level-expression
   :parse-extern
   :parse-if-expression
   :parse-for-expression
   :parse-unary
   :parse-var-expression)

  ;;;;code-generation
  (:export
   :codegen
   :*module*
   :*builder*
   :*fpm*
   :*execution-engine*)
  (:export
   :kaleidoscope-error
   :set-binop-precedence
   :main-loop
   :*chapter*
   :toplevel)
  )
(in-package #:k-shared)

(defparameter *this-directory* (filesystem-util:this-directory))

(defparameter *output?* (make-synonym-stream '*standard-output*))
(defparameter *input?* (make-synonym-stream '*standard-input*))

(defun callcc (fun)
  (block nil
    (funcall
     fun
     (lambda (&rest values)
       (return-from nil (apply (function values)
			       values))))))

(defun eh? (n)
  (mapcar (lambda (x)
	    (tree-equal x (car n) :test #'equalp))
	  n))
(defmacro eh (&rest body)
  `(quote ,(eh? body)))

(defparameter *chapter* 7)

(define-condition kaleidoscope-error (error)
  ((message :initarg :message :reader message))
  (:report (lambda (condition stream)
	     (write-string (message condition) stream))))

(defparameter *jit?* nil)
(defparameter *fpm?* t)
(defparameter *compile-to-object-code?* t)

;;; install standard binary operators
;;; 1 is lowest precedence
(defun set-binop-precedence (&optional (n *chapter*))
  (case n
    ((7) (setf (gethash #\= *binop-precedence*) 2)))
  (setf (gethash #\< *binop-precedence*) 10
	(gethash #\+ *binop-precedence*) 20
	(gethash #\- *binop-precedence*) 30
	(gethash #\* *binop-precedence*) 40))

(defmacro with-llvm-message ((ptr-var) ptr-form  &body body)
  `(let ((,ptr-var ,ptr-form))      
     (prog1 (progn ,@body)
       (llvm::-dispose-message ,ptr-var))))

(defun print-with-indent (stream string &optional (indent 0))
  (dotimes (index (length string))
    (let ((char (aref string index)))
      (write-char char stream)
      (when (char= char #\Newline)
	(dotimes (repeat indent)
	  (write-char #\Space stream))))))

(defun dump-value (value)
  (with-llvm-message (ptr) (llvm::-print-value-to-string value)
    (print-with-indent *output?* (cffi:foreign-string-to-lisp ptr))))

(defun dump-module (module)
  (with-llvm-message (ptr) (llvm::-print-module-to-string module)
    (print-with-indent *output?* (cffi:foreign-string-to-lisp ptr))))

(defun const-real (real-ty value)
  (if (stringp value)
      (cffi:with-foreign-string (str value)
	(llvm::-const-real-of-string real-ty str))
      (llvm::-const-real real-ty value)))

(defun build-call (builder fn args &optional (name ""))
  (let ((len (length args)))
    (cffi:with-foreign-string (str name)
      (cffi:with-foreign-object (var 'llvm::|LLVMValueRef| len)
	(dotimes (index len)
	  (setf (cffi:mem-aref var 'llvm::|LLVMValueRef| index)
		(elt args index)))
	(llvm::-build-call builder fn var (length args) str)))))

(defun function-type (return-type param-types &key var-arg-p)
  (let ((len (length param-types)))
    (cffi:with-foreign-object (var 'llvm::|LLVMTypeRef| len)
      (dotimes (index len)
	(setf (cffi:mem-aref var 'llvm::|LLVMTypeRef| index)
	      (elt param-types index)))
      (llvm::-function-type return-type var len var-arg-p))))

(defun params (fn)
  (let ((len (llvm::-count-params fn)))
    (cffi:with-foreign-object (ptr 'llvm::|LLVMValueRef| len)
      (llvm::-get-params fn ptr)
      (let ((acc nil))
	(dotimes (index len)
	  (push (cffi:mem-aref ptr 'llvm::|LLVMValueRef| (- len 1 index))
		acc))
	acc))))

(defmacro with-list-c-array ((ptr-var list type) &body body)
  (once-only (list type)
    (with-gensyms (len index item)
      `(let ((,len (list-length ,list)))
	 (when ,len
	   (cffi:with-foreign-object (,ptr-var ,type ,len)	   
	     (let ((,index 0))
	       (dolist (,item ,list)
		 (setf (cffi:mem-aref ,ptr-var ,type ,index)
		       ,item)
		 (incf ,index)))
	     ,@body))))))

(defun build-ret (builder &rest values)
  (case (length values)
    (0 (llvm::-build-ret-void builder))
    (1 (llvm::-build-ret builder (car values)))
    (otherwise
     (with-list-c-array (ptr values 'llvm::|LLVMValueRef|)
       (llvm::-build-aggregate-ret builder values (length values))))))

(defun position-builder (builder block &optional (instr (cffi:null-pointer)))
  (llvm::-position-builder builder block instr))

(defun add-incoming (phi-node incoming-values incoming-blocks)
  (with-list-c-array (ptr1 incoming-values 'llvm::|LLVMValueRef|)
    (with-list-c-array (ptr2 incoming-blocks 'llvm::|LLVMBasicBlockRef|)
      (llvm::-add-incoming phi-node
			   ptr1
			   ptr2
			   (length incoming-values)))))

(defparameter *c-directory* (merge-pathnames "C/" *this-directory*))
;;;;for chap 6 and 5
(cffi:load-foreign-library (merge-pathnames "libkaleidoscope-extern.so.0.1" *c-directory*))

(cffi:defcfun ("putchard" putchard) :double
  (x :double))
(cffi:defcfun ("printd" printd) :double
  (x :double))

(cffi:defcvar (fun1 "fun1") :pointer)
(cffi:defcvar (fun2 "fun2") :pointer)

(cffi:defcallback cbfun1 :void ((x :char))
  (write-char (code-char x) *output?*))
(setf fun1 (cffi:callback cbfun1))

(cffi:defcallback cbfun2 :void ((x :double))
  (format *output?* "~f~&" x))
(setf fun2 (cffi:callback cbfun2))

(progn
  (cffi:defcfun (kaleidoscope-get-target-machine "KaleidoscopeGetTargetMachine") :pointer)
  (cffi:defcfun (kaleidoscope-create "KaleidoscopeCreate") :void)
  (cffi:defcfun (kaleidoscope-destroy "KaleidoscopeDestroy") :void)
  (cffi:defcfun (kaleidoscope-add-module "KaleidoscopeAddModule") :pointer
    (module :pointer))
  (cffi:defcfun (kaleidoscope-remove-module "KaleidoscopeRemoveModule") :void
    (module-handle :pointer))
  (cffi:defcfun (kaleidoscope-find-symbol "KaleidoscopeFindSymbol") :pointer
    (sym :pointer))
  (cffi:defcfun (kaleidoscope-get-symbol-address "KaleidoscopeGetSymbolAddress") :uint64
    (sym :pointer)))

#+nil
(cffi:defcfun (get-target-machine-data "LLVMGetTargetMachineData") :pointer
  (target-machine-ref :pointer))
#+nil
(cffi:defcfun (get-target-machine-triple "LLVMGetTargetMachineTriple") :pointer
  (target-machine-ref :pointer))
#+nil
(cffi:defcfun (dispose-message "LLVMDisposeMessage") :void
  (target-machine--ref :pointer))

;;;;other llvm funs
(in-package :llvm)
;;;;WARNING:: got the functions below to run by adding
#|
extern "C" {

LLVMBool LLVMInitializeNativeTarget__(void) {
  return LLVMInitializeNativeTarget();
}

LLVMBool LLVMInitializeNativeAsmParser__(void) {
  return LLVMInitializeNativeAsmParser();
}

LLVMBool LLVMInitializeNativeAsmPrinter__(void) {
  return LLVMInitializeNativeAsmPrinter();
}

LLVMBool LLVMInitializeNativeDisassembler__(void) {
  return LLVMInitializeNativeDisassembler();
}


}
|#
;;;;to llvm/lib/Target/TargetMachineC.cpp
;;;;why? because it includes llvm-c/Target.h and gets linked into the shared library.
;;;;the functions are wrappers around static inline c functions, which means they are
;;;;not exposed in the shared library
;;;;FIXME: put functions into a diffent file
(progn
  (cffi:defcfun (initialize-native-target? "LLVMInitializeNativeTarget__") :int)
  (cffi:defcfun (initialize-native-asm-parser "LLVMInitializeNativeAsmParser__") :int)
  (cffi:defcfun (initialize-native-asm-printer "LLVMInitializeNativeAsmPrinter__") :int)
  (cffi:defcfun (initialize-native-disassembler "LLVMInitializeNativeDisassembler__") :int))

(progn
  (cffi:defcfun (initialize-all-target-infos "LLVMInitializeAllTargetInfos__") :void)
  (cffi:defcfun (initialize-all-targets "LLVMInitializeAllTargets__") :void)
  (cffi:defcfun (initialize-all-target-m-cs "LLVMInitializeAllTargetMCs__") :void)
  (cffi:defcfun (initialize-all-asm-parsers "LLVMInitializeAllAsmParsers__") :void)
  (cffi:defcfun (initialize-all-asm-printers "LLVMInitializeAllAsmPrinters__") :void))
