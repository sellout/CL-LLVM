(in-package :llvm)

(defcfun* "LLVMCreatePassManager" pass-manager)

(defcfun* "LLVMCreateFunctionPassManagerForModule" pass-manager (m module))

(defcfun* "LLVMRunPassManager" :boolean (pm pass-manager) (m module))

(defcfun* "LLVMInitializeFunctionPassManager" :boolean (fpm pass-manager))

(defcfun* "LLVMRunFunctionPassManager" :boolean (fpm pass-manager) (f value))

(defcfun* "LLVMFinalizeFunctionPassManager" :boolean (fpm pass-manager))

(defcfun* "LLVMDisposePassManager" :void (pm pass-manager))
