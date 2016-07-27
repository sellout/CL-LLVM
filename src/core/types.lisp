(in-package :llvm)

(defcfun (kind "LLVMGetTypeKind") type-kind
  (ty type))

(defcfun (context "LLVMGetTypeContext") context
  (ty type))

(defcfun* "LLVMInt1TypeInContext" type (c context))
(defun int1-type (&key (context (global-context)))
  (int1-type-in-context context))
(defcfun* "LLVMInt8TypeInContext" type (c context))
(defun int8-type (&key (context (global-context)))
  (int8-type-in-context context))
(defcfun* "LLVMInt16TypeInContext" type (c context))
(defun int16-type (&key (context (global-context)))
  (int16-type-in-context context))
(defcfun* "LLVMInt32TypeInContext" type (c context))
(defun int32-type (&key (context (global-context)))
  (int32-type-in-context context))
(defcfun* "LLVMInt64TypeInContext" type (c context))
(defun int64-type (&key (context (global-context)))
  (int64-type-in-context context))
(defcfun* "LLVMIntTypeInContext" type (c context)
  (num-bits :unsigned-int))
(defun int-type (num-bits &key (context (global-context)))
  (int-type-in-context context num-bits))

(defcfun (width "LLVMGetIntTypeWidth") :unsigned-int (integer-ty type))

(defcfun* "LLVMFloatTypeInContext" type (c context))
(defun float-type (&key (context (global-context)))
  (float-type-in-context context))
(defcfun* "LLVMDoubleTypeInContext" type (c context))
(defun double-type (&key (context (global-context)))
  (double-type-in-context context))
(defcfun* "LLVMX86FP80TypeInContext" type (c context))
(defun x86-fp80-type (&key (context (global-context)))
  (x86-fp80-type-in-context context))
(defcfun* "LLVMFP128TypeInContext" type (c context))
(defun fp128-type (&key (context (global-context)))
  (fp128-type-in-context context))
(defcfun (ppc-fp128-type-in-context "LLVMPPCFP128TypeInContext") type
  (c context))
(defun ppc-fp128-type (&key (context (global-context)))
  (ppc-fp128-type-in-context context))

(defcfun (%function-type "LLVMFunctionType") type
  (return-type type)
  (param-types (carray type)) (param-count :unsigned-int)
  (is-var-arg :boolean))
(defun function-type (return-type param-types &key var-arg-p)
  (%function-type return-type param-types (length param-types) var-arg-p))

(defcfun (function-var-arg-p "LLVMIsFunctionVarArg") :boolean
  (function-ty type))
(defcfun (return-type "LLVMGetReturnType") type (function-ty type))
(defcfun* "LLVMCountParamTypes" :unsigned-int (function-ty type))
(defcfun* "LLVMGetParamTypes" :void (function-ty type) (dest (:pointer type)))
(defun param-types (function-ty)
  (with-pointer-to-list (pointer type (count-param-types function-ty))
    (get-param-types function-ty pointer)))

(defcfun* "LLVMStructTypeInContext" type
  (c context)
  (element-types (carray type)) (element-count :unsigned-int)
  (packed :boolean))
(defun struct-type (element-types packed &key (context (global-context)))
  (struct-type-in-context context element-types (length element-types) packed))
(defcfun* "LLVMCountStructElementTypes" :unsigned-int (struct-ty type))
(defcfun* "LLVMGetStructElementTypes" :void
  (struct-ty type) (dest (:pointer type)))
(defun struct-element-types (struct-ty)
  (with-pointer-to-list (pointer type (count-struct-element-types struct-ty))
    (get-struct-element-types struct-ty pointer)))
(defcfun (packed-struct-p "LLVMIsPackedStruct") :boolean (struct-ty type))
(defcfun* "LLVMStructCreateNamed" type
  (c context)
  (name :string))
(defcfun* "LLVMGetStructName" :string
  (struct type))
(defcfun (%struct-set-body "LLVMStructSetBody") :void
  (struct type)
  (element-types (carray type)) (element-count :unsigned-int)
  (packed :boolean))
(defun struct-set-body (struct-type element-types &optional (packed nil))
  (%struct-set-body struct-type element-types (length element-types) packed))

(defcfun* "LLVMArrayType" type
  (element-type type) (element-count :unsigned-int))
(defcfun (%pointer-type "LLVMPointerType") type
  (element-type type) (address-space :unsigned-int))
(defun pointer-type (element-type &optional (address-space 0))
  (%pointer-type element-type address-space))
(defcfun* "LLVMVectorType" type
  (element-type type) (element-count :unsigned-int))

(defcfun (element-type "LLVMGetElementType") type (ty type))
(defcfun (array-length "LLVMGetArrayLength") :unsigned-int (array-ty type))
(defcfun (address-space "LLVMGetPointerAddressSpace") :unsigned-int
  (pointer-ty type))
(defcfun (size "LLVMGetVectorSize") :unsigned-int (vector-ty type))

(defcfun* "LLVMVoidTypeInContext" type (c context))
(defun void-type (&key (context (global-context)))
  (void-type-in-context context))
(defcfun* "LLVMLabelTypeInContext" type (c context))
(defun label-type (&key (context (global-context)))
  (label-type-in-context context))
(defcfun* "LLVMOpaqueTypeInContext" type (c context))
(defun opaque-type (&key (context (global-context)))
  (opaque-type-in-context context))

(defcfun* "LLVMCreateTypeHandle" type-handle (potentially-abstract-ty type))
(defcfun* "LLVMRefineType" :void (abstract-ty type) (concrete-ty type))
(defcfun* "LLVMResolveTypeHandle" :void (type-handle type-handle))
(defcfun* "LLVMDisposeTypeHandle" :void (type-handle type-handle))

(defcfun (%dump-type "LLVMDumpType") :void (m type))
(defun dump-type (m)
  (finish-output *error-output*)
  (%dump-module m))

(defcfun* "LLVMPrintTypeToString" :string (m type))
