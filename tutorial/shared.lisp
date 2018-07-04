(defpackage #:k-shared
  (:use
   #:cl)
  (:export
   #:*output?*
   #:*input?*)
  (:export
   #:callcc)
  (:export
   #:dump-value
   #:dump-module)
  (:export
   #:*this-directory*))
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

(defun dump-value (value)
  (write-string (llvm:print-value-to-string value) *output?*))

(defun dump-module (module)
  (write-string (llvm:print-module-to-string module) *output?*))


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
(defun initialize-native-target??? ()
  #+mips (initialize-mips-target)
  #+alpha (initialize-alpha-target)
  #+(or ppc ppc64)
  (initialize-powerpc-target)
  #+(or sparc sparc64)
  (initialize-sparc-target)
  #+(or x86 x86-64)
  (progn (initialize-x86-target) (initialize-x86-target-info))
  #-(or mips alpha ppc ppc64 sparc sparc64 x86 x86-64) (return-from initialize-native-target nil)
  t)
#+nil
(cffi:defcfun (initialize-native-target? "LLVMInitializeNativeTarget") :int)

#+nil
(defcfun* "LLVMLinkInMCJIT" :void)
#+nil
(progn
  (defcfun* "LLVMInitializeAllTargetInfos" :void)
  (defcfun* "LLVMInitializeAllTargets" :void)
  (defcfun* "LLVMInitializeAllTargetMCs" :void)
  (defcfun* "LLVMInitializeAllAsmPrinters" :void)
  (defcfun* "LLVMInitializeAllAsmParsers" :void)
  (defcfun* "LLVMInitializeAllDisassemblers" :void))

#+nil
(progn
  (llvm::initialize-all-target-infos)
  (llvm::initialize-all-targets)
  (llvm::initialize-all-target-m-cs)
  (llvm::initialize-all-asm-printers)
  (llvm::initialize-all-asm-parsers)
  (llvm::initialize-all-Disassemblers))
