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

(defcfun "LLVMGetBitcodeModuleInContext" :boolean
  (context context) (mem-buf memory-buffer)
  (out-m (:pointer module)) (out-message (:pointer :string)))
(defun bitcode-module (mem-buf &key (context (global-context)))
  (with-foreign-objects ((out-message '(:pointer :string))
                         (out-m '(:pointer module)))
    (if (get-bitcode-module-in-context context mem-buf out-m out-message)
      (mem-ref out-m 'module)
      (error 'llvm-error :message out-message))))
