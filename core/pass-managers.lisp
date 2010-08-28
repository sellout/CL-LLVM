(in-package :llvm)

(defcfun "LLVMCreatePassManager" pass-manager)
(defmethod make-instance ((class (eql 'pass-manager)) &key)
  (create-pass-manager))

(defcfun "LLVMCreateFunctionPassManagerForModule" pass-manager (m module))
(defmethod make-instance
           ((class (eql 'function-pass-manager))
            &key (module (error 'required-parameter-error :name 'module)))
  (create-function-pass-manager-for-module module))

(defcfun "LLVMRunPassManager" :boolean (pm pass-manager) (m module))

(defcfun "LLVMInitializeFunctionPassManager" :boolean (fpm pass-manager))

(defcfun "LLVMRunFunctionPassManager" :boolean (fpm pass-manager) (f value))

(defcfun "LLVMFinalizeFunctionPassManager" :boolean (fpm pass-manager))

(defcfun "LLVMDisposePassManager" :void (pm pass-manager))
