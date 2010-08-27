(in-package :llvm)

(defcfun "LLVMInitializeAllTargetInfos" :void)

(defcfun "LLVMInitializeAllTargets" :void)

(defcfun "LLVMInitializeNativeTarget" :boolean)

(defcfun "LLVMCreateTargetData" target-data (string-rep :string))
(defmethod make-instance
           ((class (eql 'target-data))
            &key (string-representation
                  (error 'required-parameter-error :name 'string-representation)))
  (create-target-data string-representation))

(defcfun "LLVMAddTargetData" :void
  (target-data target-data) (pass-manager pass-manager))

(defcfun "LLVMCopyStringRepOfTargetData" (:pointer :char)
  (target-data target-data))
(defun string-representation (target-data)
  (let ((message (copy-string-rep-of-target-data target-data)))
    (prog1
        (mem-ref message :string)
      (dispose-message message))))

(defcfun "LLVMByteOrder" byte-ordering (target-data target-data))

(defcfun "LLVMPointerSize" :unsigned-int (target-data target-data))

(defcfun (int-pointer-type "LLVMIntPtrType") type (target-data target-data))

(defcfun "LLVMSizeOfTypeInBits" :unsigned-long-long
  (target-data target-data) (type type))

(defcfun (storage-size-of-type "LLVMStoreSizeOfType") :unsigned-long-long
  (target-data target-data) (type type))

(defcfun "LLVMABISizeOfType" :unsigned-long-long
  (target-data target-data) (type type))

(defcfun "LLVMABIAlignmentOfType" :unsigned-int
  (target-data target-data) (type type))

(defcfun "LLVMCallFrameAlignmentOfType" :unsigned-int
  (target-data target-data) (type type))

(defcfun "LLVMPreferredAlignmentOfType" :unsigned-int
  (target-data target-data) (type type))

(defcfun "LLVMPreferredAlignmentOfGlobal" :unsigned-int
  (target-data target-data) (global-var value))

(defcfun "LLVMElementAtOffset" :unsigned-int
  (target-data target-data) (struct-ty type) (offset :unsigned-long-long))

(defcfun "LLVMOffsetOfElement" :unsigned-long-long
  (target-data target-data) (struct-ty type) (element :unsigned-int))

(defcfun "LLVMInvalidateStructLayout" :void
  (target-data target-data) (struct-ty type))

(defcfun "LLVMDisposeTargetData" :void (target-data target-data))
