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

(defparameter *jit?* t)
(defparameter *fpm?* t)

;;; install standard binary operators
;;; 1 is lowest precedence
(defun set-binop-precedence (&optional (n *chapter*))
  (case n
    ((7) (setf (gethash #\= *binop-precedence*) 2)))
  (setf (gethash #\< *binop-precedence*) 10
	(gethash #\+ *binop-precedence*) 20
	(gethash #\- *binop-precedence*) 30
	(gethash #\* *binop-precedence*) 40))

(defun dump-value (value)
  (write-string (llvm:print-value-to-string value) *output?*))

(defun dump-module (module)
  (write-string (llvm:print-module-to-string module) *output?*))

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
(cffi:defcfun (get-target-machine-data "LLVMGetTargetMachineData") :pointer
  (target-machine-ref :pointer))

(cffi:defcfun (get-target-machine-triple "LLVMGetTargetMachineTriple") :pointer
  (target-machine-ref :pointer))

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
