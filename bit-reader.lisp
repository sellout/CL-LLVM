(in-package :llvm)

(defcfun "LLVMParseBitcodeInContext" :boolean
  (context context) (mem-buf memory-buffer)
  (out-module (:pointer module)) (out-message (:pointer :string)))
(defun parse-bitcode (mem-buf &key (context (global-context)))
  (with-foreign-objects ((out-module '(:pointer module))
                         (out-message '(:pointer :string)))
    (if (parse-bitcode-in-context context mem-buf out-module out-message)
      (mem-ref out-module 'module)
      (error 'llvm-error :message out-message))))

(defcfun "LLVMGetBitcodeModuleProviderInContext" :boolean
  (context context) (mem-buf memory-buffer)
  (out-mp (:pointer module-provider)) (out-message (:pointer :string)))
(defun bitcode-module-provider (mem-buf &key (context (global-context)))
  (with-foreign-objects ((out-message '(:pointer :string))
                         (out-mp '(:pointer module)))
    (if (get-bitcode-module-provider-in-context context mem-buf
                                                out-mp out-message)
      (mem-ref out-mp 'module)
      (error 'llvm-error :message out-message))))
