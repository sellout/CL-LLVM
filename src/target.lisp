(in-package :llvm)

;;; NOTE: Add any new LLVM targets to the following list.
(eval-when (:compile-toplevel :load-toplevel)
  (defvar *targets* '("MBlaze" "CppBackend" "MSIL" "CBackend" "Blackfin"
                      "SystemZ" "MSP430" "XCore" "PIC16" "CellSPU" "Mips" "ARM"
                      "Alpha" "PowerPC" "Sparc" "X86"))
  (defvar *target-info-functions* nil)
  (defvar *target-functions* nil))

(defmacro declare-targets ()
  `(progn ,@(mapcan (lambda (target)
                      `((push (defcfun* ,(format nil
                                                 "LLVMInitialize~aTargetInfo"
                                                 target)
                                  :void)
                              *target-info-functions*)
                        (push (defcfun* ,(format nil "LLVMInitialize~aTarget"
                                                 target)
                                  :void)
                              *target-functions*)))
                    *targets*)))

(declare-targets)
(export *target-info-functions*)
(export *target-functions*)

(defun initialize-all-target-infos ()
  (mapc #'funcall *target-info-functions*)
  (values))

(defun initialize-all-targets ()
  (mapc #'funcall *target-functions*)
  (values))

(defun initialize-native-target ()
  #+mips (progn (initialize-mips-target-info) (initialize-mips-target) t)
  #+alpha (progn (initialize-alpha-target-info) (initialize-alpha-target) t)
  #+(or ppc ppc64)
  (progn (initialize-powerpc-target-info) (initialize-powerpc-target) t)
  #+(or sparc sparc64)
  (progn (initialize-sparc-target-info) (initialize-sparc-target) t)
  #+(or x86 x86-64)
  (progn (initialize-x86-target-info) (initialize-x86-target) t)
  #-(or mips alpha ppc ppc64 sparc sparc64 x86 x86-64) nil)

(defcfun* "LLVMCreateTargetData" target-data (string-rep :string))
(defmethod make-instance
           ((class (eql 'target-data))
            &key (string-representation
                  (error 'required-parameter-error :name 'string-representation)))
  (create-target-data string-representation))

(defcfun* "LLVMAddTargetData" :void
  (target-data target-data) (pass-manager pass-manager))

(defcfun* "LLVMCopyStringRepOfTargetData" (:pointer :char)
  (target-data target-data))
(defun string-representation (target-data)
  (let ((message (copy-string-rep-of-target-data target-data)))
    (prog1
        (mem-ref message :string)
      (dispose-message message))))

(defcfun* "LLVMByteOrder" byte-ordering (target-data target-data))

(defcfun* "LLVMPointerSize" :unsigned-int (target-data target-data))

(defcfun (int-pointer-type "LLVMIntPtrType") type (target-data target-data))

(defcfun* "LLVMSizeOfTypeInBits" :unsigned-long-long
  (target-data target-data) (type type))

(defcfun (storage-size-of-type "LLVMStoreSizeOfType") :unsigned-long-long
  (target-data target-data) (type type))

(defcfun* "LLVMABISizeOfType" :unsigned-long-long
  (target-data target-data) (type type))

(defcfun* "LLVMABIAlignmentOfType" :unsigned-int
  (target-data target-data) (type type))

(defcfun* "LLVMCallFrameAlignmentOfType" :unsigned-int
  (target-data target-data) (type type))

(defcfun* "LLVMPreferredAlignmentOfType" :unsigned-int
  (target-data target-data) (type type))

(defcfun* "LLVMPreferredAlignmentOfGlobal" :unsigned-int
  (target-data target-data) (global-var value))

(defcfun* "LLVMElementAtOffset" :unsigned-int
  (target-data target-data) (struct-ty type) (offset :unsigned-long-long))

(defcfun* "LLVMOffsetOfElement" :unsigned-long-long
  (target-data target-data) (struct-ty type) (element :unsigned-int))

(defcfun* "LLVMInvalidateStructLayout" :void
  (target-data target-data) (struct-ty type))

(defcfun* "LLVMDisposeTargetData" :void (target-data target-data))
