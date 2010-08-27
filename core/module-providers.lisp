(in-package :llvm)

(defcfun "LLVMCreateModuleProviderForExistingModule" module-provider
  (m module))
(defmethod make-instance
           ((class (eql 'module-provider))
            &key (module (error 'required-parameter-error :name 'module)))
  (create-module-provider-for-existing-module module))

(defcfun "LLVMDisposeModuleProvider" :void (mp module-provider))
