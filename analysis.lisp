(in-package :llvm)

(defcfun (%verify-module "LLVMVerifyModule") :boolean
  (m module) (action verifier-failure-action) (out-message (:pointer :string)))
(defun verify-module (m)
  (with-foreign-object (out-message '(:pointer :string))
    (if (%verify-module m :return-status out-message)
      (error 'llvm-error :message out-message)
      t)))

(defcfun (%verify-function "LLVMVerifyFunction") :boolean
  (fn value) (action verifier-failure-action))
(defun verify-function (fn)
  (not (%verify-function fn :return-status)))

(defcfun* "LLVMViewFunctionCFG" :void (fn value))
(defcfun* "LLVMViewFunctionCFGOnly" :void (fn value))