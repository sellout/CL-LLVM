(in-package :llvm)

(defcfun* "LLVMContextCreate" context)
(defcfun (global-context "LLVMGetGlobalContext") context)
(defcfun (dispose-context "LLVMContextDispose") :void (c context))

(defcfun* "LLVMModuleCreateWithNameInContext" module
  (module-id :string) (c context))
(defun make-module (name &optional (context (global-context)))
  (module-create-with-name-in-context name context))

(defcfun* "LLVMDisposeModule" :void (m module))

(defcfun (data-layout "LLVMGetDataLayout") :string (m module))
(defcfun* "LLVMSetDataLayout" :void (m module) (triple :string))
(defun (setf data-layout) (triple m)
  (set-data-layout m triple)
  triple)

(defcfun (target "LLVMGetTarget") :string (m module))
(defcfun* "LLVMSetTarget" :void (m module) (triple :string))
(defun (setf target) (triple m)
  (set-target m triple)
  triple)

(defcfun* "LLVMAddTypeName" :int (m module) (name :string) (ty type))
(defcfun* "LLVMDeleteTypeName" :void (m module) (name :string))
(defcfun* "LLVMGetTypeByName" type (m module) (name :string))

(defcfun (%dump-module "LLVMDumpModule") :void (m module))
(defun dump-module (m)
  (finish-output *error-output*)
  (%dump-module m))

(defcfun* "LLVMPrintModuleToString" :string (m module))

(defcfun (%print-module-to-file "LLVMPrintModuleToFile") :boolean
  (m module) (path :string) (out-message (:pointer :string)))

(defun print-module-to-file (m path)
  (with-foreign-objects ((out-message '(:pointer :string)))
    (if (%print-module-to-file m path out-message)
        (throw-llvm-error out-message)
        t)))

(defcfun (module-create-with-name "LLVMModuleCreateWithName") module
  (module-id :string))
