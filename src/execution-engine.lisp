(in-package :llvm)

(defcfun* "LLVMLinkInJIT" :void)
(defcfun* "LLVMLinkInInterpreter" :void)

;;; operations on generic values

(defcfun* "LLVMCreateGenericValueOfInt" generic-value
  (ty type) (n :unsigned-long-long) (is-signed :boolean))
(defmethod make-instance
           ((class (eql 'generic-value-of-int))
            &key (type (error 'required-parameter-error :name 'type))
                 (value (error 'required-parameter-error :name 'value))
                 (signedp (error 'required-parameter-error :name 'signedp)))
  (create-generic-value-of-int type value signedp))

(defcfun* "LLVMCreateGenericValueOfPointer" generic-value (p (:pointer :void)))
(defmethod make-instance
           ((class (eql 'generic-value-of-pointer))
            &key (pointer (error 'required-parameter-error :name 'pointer)))
  (create-generic-value-of-pointer pointer))

(defcfun* "LLVMCreateGenericValueOfFloat" generic-value (ty type) (n :double))
(defmethod make-instance
           ((class (eql 'generic-value-of-float))
            &key (type (error 'required-parameter-error :name 'type))
                 (value (error 'required-parameter-error :name 'value)))
  (create-generic-value-of-float type value))

(defcfun* "LLVMGenericValueIntWidth" :unsigned-int (gen-val generic-value))

(defcfun* "LLVMGenericValueToInt" :unsigned-long-long
  (gen-val generic-value) (is-signed :boolean))

(defcfun* "LLVMGenericValueToPointer" (:pointer :void) (gen-val generic-value))

(defcfun* "LLVMGenericValueToFloat" :double (ty type) (gen-val generic-value))

(defcfun* "LLVMDisposeGenericValue" :void (gen-val generic-value))

;;; operations on execution engines

(defcfun* "LLVMCreateExecutionEngineForModule" :boolean
  (out-ee (:pointer execution-engine))
  (m module)
  (out-error (:pointer :string)))
(defmethod make-instance
           ((class (eql 'execution-engine))
            &key
            (module (error 'required-parameter-error :name 'module)))
  (with-foreign-objects ((out-ee '(:pointer execution-engine))
                         (out-error '(:pointer :string)))
    (if (create-execution-engine-for-module out-ee module out-error)
      (error 'llvm-error :message out-error)
      (mem-ref out-ee 'execution-engine))))

(defcfun* "LLVMCreateInterpreterForModule" :boolean
  (out-interp (:pointer execution-engine))
  (m module)
  (out-error (:pointer :string)))
(defmethod make-instance
           ((class (eql 'interpreter))
            &key
            (module (error 'required-parameter-error :name 'module)))
  (with-foreign-objects ((out-interp '(:pointer execution-engine))
                         (out-error '(:pointer :string)))
    (if (create-interpreter-for-module out-interp module out-error)
      (error 'llvm-error :message out-error)
      (mem-ref out-interp 'execution-engine))))

(defcfun* "LLVMCreateJITCompilerForModule" :boolean
  (out-jit (:pointer execution-engine))
  (m module)
  (opt-level optimization-level)
  (out-error (:pointer :string)))
(defmethod make-instance
           ((class (eql 'jit-compiler))
            &key
            (module (error 'required-parameter-error :name 'module))
            (optimization-level :default))
  (with-foreign-objects ((out-jit '(:pointer execution-engine))
                         (out-error '(:pointer :string)))
    (if (create-jit-compiler-for-module out-jit
                                        module
                                        optimization-level
                                        out-error)
      (error 'llvm-error :message out-error)
      (mem-ref out-jit 'execution-engine))))

(defcfun* "LLVMDisposeExecutionEngine" :void (ee execution-engine))

(defcfun* "LLVMRunStaticConstructors" :void (ee execution-engine))

(defcfun* "LLVMRunStaticDestructors" :void (ee execution-engine))

(defcfun (%run-function-as-main "LLVMRunFunctionAsMain") :int
  (ee execution-engine) (f value)
  (arg-c :unsigned-int) (arg-v (:pointer (carray :string)))
  (env-p (:pointer (carray :string))))
;; FIXME: Not handling args and env properly here
(defun run-function-as-main (execution-engine function args env)
  (%run-function-as-main execution-engine function (length args) args env))

(defcfun (%run-function "LLVMRunFunction") generic-value
  (ee execution-engine) (f value)
  (num-args :unsigned-int) (args (carray generic-value)))
(defun run-function (ee f args)
  (%run-function ee f (length args) args))

(defcfun* "LLVMFreeMachineCodeForFunction" :void
  (ee execution-engine) (f value))

(defcfun* "LLVMAddModule" :void (ee execution-engine) (m module))

(defcfun (%remove-module "LLVMRemoveModule") :boolean
  (ee execution-engine) (m module)
  (out-mod (:pointer module)) (out-error (:pointer :string)))
(defun remove-module-provider (ee m)
  (with-foreign-objects ((out-mod '(:pointer module))
                         (out-error '(:pointer :string)))
    (if (%remove-module ee m out-mod out-error)
      (error 'llvm-error :message out-error)
      (mem-ref out-mod 'module))))

(defcfun (%find-function "LLVMFindFunction") :boolean
  (ee execution-engine) (name :string) (out-fn (:pointer value)))
(defun find-function (ee name)
  (with-foreign-object (out-fn '(:pointer value))
    (when (not (%find-function ee name out-fn))
      (mem-ref out-fn 'value))))

(defcfun (target-data "LLVMGetExecutionEngineTargetData") target-data
  (ee execution-engine))

(defcfun* "LLVMAddGlobalMapping" :void
  (ee execution-engine) (global value) (addr (:pointer :void)))

(defcfun (pointer-to-global "LLVMGetPointerToGlobal") (:pointer :void)
  (ee execution-engine) (global value))
