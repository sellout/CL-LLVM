(in-package :llvm)

(defcfun "LLVMCreatePassManager" pass-manager)
(defmethod make-instance ((class (eql 'pass-manager)) &key)
  (create-pass-manager))

(defcfun "LLVMCreateFunctionPassManager" pass-manager (mp module-provider))
(defmethod make-instance
           ((class (eql 'function-pass-manager))
            &key (module-provider
                  (error 'required-parameter-error :name 'module-provider)))
  (create-function-pass-manager module-provider))

(defcfun "LLVMRunPassManager" :boolean (pm pass-manager) (mp module-provider))

(defcfun "LLVMInitializeFunctionPassManager" :boolean (fpm pass-manager))

(defcfun "LLVMRunFunctionPassManager" :boolean (fpm pass-manager) (f value))

(defcfun "LLVMFinalizeFunctionPassManager" :boolean (fpm pass-manager))

(defcfun "LLVMDisposePassManager" :void (pm pass-manager))
