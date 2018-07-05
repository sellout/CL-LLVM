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
   #:chap-package)

  (:export
   :*identifier-string*
   :*number-value*
   :*current-token*
   :*token-types*
   :get-next-token
   :reset-token-reader
   :chap-tokens
   :with-tokens))
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
(defparameter *this-package* *package*)
(defun repackage (sexp &optional (new-package *package*))
  (setf new-package (find-package new-package))
  (labels ((walk (sexp)
	     (cond ((consp sexp)
		    (cons
		     (walk (car sexp))
		     (walk (cdr sexp))))
		   ((symbolp sexp)
		    (if (eq (symbol-package sexp)
			    *this-package*)
			(intern (string sexp) new-package)
			sexp))
		   (t sexp))))
    (walk sexp)))

(defun chap-package (n)
  (find-package
   (format nil "KALEIDOSCOPE.CHAPTER~s" n)))

(defun dump-value (value)
  (write-string (llvm:print-value-to-string value) *output?*))

(defun dump-module (module)
  (write-string (llvm:print-module-to-string module) *output?*))

(eval-always
  (defparameter *chapcodes* (make-hash-table :test 'equalp)))
(defmacro define-codes (name (&rest chapters) &body codes)
  (setf (gethash name *chapcodes*)
	(cons chapters codes))
  nil)

(eval-always
  (defun %writecodes (chap &optional (package (chap-package chap)))
    (let ((acc nil))
      (dohash (name value) *chapcodes*
	      (declare (ignore name))
	      (when (find chap (car value))
		(mapc (lambda (x) (push x acc))
		      (cdr value))))
      (repackage
       (nreverse acc)
       package))))

(defmacro writecodes (chap)
  (cons 'progn (%writecodes chap)))

;;;;for chap 6 and 5
(cffi:load-foreign-library (merge-pathnames *this-directory* "libkaleidoscope-extern.so.0.1"))

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
