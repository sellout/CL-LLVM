(in-package :llvm)

(defmacro with-builder ((var &optional context) &body body)
  `(let ((,var (make-builder ,@(when context (list context)))))
     (unwind-protect (progn ,@body)
       (dispose-builder ,var))))

(defcfun* "LLVMCreateBuilderInContext" builder (c context))
(defun make-builder (&optional (context (global-context)))
  (create-builder-in-context context))
(defmethod make-instance
           ((class (eql 'builder)) &key (context (global-context)))
  (make-builder context))
(defcfun (%position-builder "LLVMPositionBuilder") builder
  (builder builder) (block basic-block) (instr value))
(defun position-builder (builder block &optional (instr (null-pointer)))
  (%position-builder builder block instr))
(defcfun* "LLVMPositionBuilderBefore" builder (builder builder) (instr value))
(defcfun* "LLVMPositionBuilderAtEnd" builder
  (builder builder) (block basic-block))
;; NOTE: renamed from INSERT-BLOCK to avoid noun/verb confusion
(defcfun (insertion-block "LLVMGetInsertBlock") basic-block (builder builder))
(defcfun* "LLVMClearInsertionPosition" :void
  (builder builder))
(defcfun (%insert-into-builder "LLVMInsertIntoBuilder") :void
  (builder builder) (instr value))
(defcfun* "LLVMInsertIntoBuilderWithName" :void
  (builder builder) (instr value) (name :string))
(defun insert-into-builder (builder instr &key name)
  (if name
    (insert-into-builder-with-name builder instr name)
    (%insert-into-builder builder instr)))
(defcfun* "LLVMDisposeBuilder" :void (builder builder))

(defcfun* "LLVMBuildRetVoid" value (builder builder))
(defcfun (%build-ret "LLVMBuildRet") value (builder builder) (v value))
(defcfun* "LLVMBuildAggregateRet" value
  (builder builder) (ret-vals (carray value)) (n :unsigned-int))
(defun build-ret (builder &rest values)
  (case (length values)
    (0 (build-ret-void builder))
    (1 (%build-ret builder (car values)))
    (otherwise (build-aggregate-ret builder values (length values)))))
(defcfun* "LLVMBuildBr" value (builder builder) (dest basic-block))
(defcfun* "LLVMBuildCondBr" value
  (builder builder) (if value) (then basic-block) (else basic-block))
(defcfun* "LLVMBuildSwitch" value
  (builder builder) (v value) (else basic-block) (num-cases :unsigned-int))
(defcfun (%build-invoke "LLVMBuildInvoke") value
  (builder builder) (fn value) (args (carray value)) (num-args :unsigned-int)
  (then basic-block) (catch basic-block) (name :string))
(defun build-invoke (builder fn args then catch name)
  (%build-invoke builder fn args (length args) then catch name))
(defcfun* "LLVMBuildUnwind" value (builder builder))
(defcfun* "LLVMBuildUnreachable" value (builder builder))

(defcfun* "LLVMAddCase" :void (switch value) (on-val value) (dest basic-block))

(defcfun* "LLVMBuildAdd" value
  (builder builder) (lhs value) (rhs value) (name :string))
(defcfun* "LLVMBuildNSWAdd" value
  (builder builder) (lhs value) (rhs value) (name :string))
(defcfun* "LLVMBuildNUWAdd" value
  (builder builder) (lhs value) (rhs value) (name :string))
(defcfun* "LLVMBuildFAdd" value
  (builder builder) (lhs value) (rhs value) (name :string))
(defcfun* "LLVMBuildSub" value
  (builder builder) (lhs value) (rhs value) (name :string))
(defcfun* "LLVMBuildNSWSub" value
  (builder builder) (lhs value) (rhs value) (name :string))
(defcfun* "LLVMBuildNUWSub" value
  (builder builder) (lhs value) (rhs value) (name :string))
(defcfun* "LLVMBuildFSub" value
  (builder builder) (lhs value) (rhs value) (name :string))
(defcfun* "LLVMBuildMul" value
  (builder builder) (lhs value) (rhs value) (name :string))
(defcfun* "LLVMBuildNSWMul" value
  (builder builder) (lhs value) (rhs value) (name :string))
(defcfun* "LLVMBuildNUWMul" value
  (builder builder) (lhs value) (rhs value) (name :string))
(defcfun* "LLVMBuildFMul" value
  (builder builder) (lhs value) (rhs value) (name :string))
(defcfun* "LLVMBuildUDiv" value
  (builder builder) (lhs value) (rhs value) (name :string))
(defcfun* "LLVMBuildSDiv" value
  (builder builder) (lhs value) (rhs value) (name :string))
(defcfun* "LLVMBuildExactSDiv" value
  (builder builder) (lhs value) (rhs value) (name :string))
(defcfun* "LLVMBuildFDiv" value
  (builder builder) (lhs value) (rhs value) (name :string))
(defcfun* "LLVMBuildURem" value
  (builder builder) (lhs value) (rhs value) (name :string))
(defcfun* "LLVMBuildSRem" value
  (builder builder) (lhs value) (rhs value) (name :string))
(defcfun* "LLVMBuildFRem" value
  (builder builder) (lhs value) (rhs value) (name :string))
(defcfun* "LLVMBuildShl" value
  (builder builder) (lhs value) (rhs value) (name :string))
(defcfun* "LLVMBuildLShr" value
  (builder builder) (lhs value) (rhs value) (name :string))
(defcfun* "LLVMBuildAShr" value
  (builder builder) (lhs value) (rhs value) (name :string))
(defcfun* "LLVMBuildAnd" value
  (builder builder) (lhs value) (rhs value) (name :string))
(defcfun* "LLVMBuildOr" value
  (builder builder) (lhs value) (rhs value) (name :string))
(defcfun* "LLVMBuildXor" value
  (builder builder) (lhs value) (rhs value) (name :string))
(defcfun* "LLVMBuildNeg" value (builder builder) (v value) (name :string))
(defcfun* "LLVMBuildFNeg" value (builder builder) (v value) (name :string))
(defcfun* "LLVMBuildNot" value (builder builder) (v value) (name :string))

(defcfun* "LLVMBuildMalloc" value (builder builder) (ty type) (name :string))
(defcfun* "LLVMBuildArrayMalloc" value
  (builder builder) (ty type) (val value) (name :string))
(defcfun* "LLVMBuildAlloca" value (builder builder) (ty type) (name :string))
(defcfun* "LLVMBuildArrayAlloca" value
  (builder builder) (ty type) (val value) (name :string))
(defcfun* "LLVMBuildFree" value (builder builder) (pointer-val value))
(defcfun* "LLVMBuildLoad" value
  (builder builder) (pointer-val value) (name :string))
(defcfun* "LLVMBuildStore" value (builder builder) (val value) (ptr value))
(defcfun (%build-gep "LLVMBuildGEP") value
  (b builder)
  (pointer value) (indices (carray value)) (num-indices :unsigned-int)
  (name :string))
(defun build-gep (b pointer indices name)
  (%build-gep b pointer indices (length indices) name))
(defcfun (%build-in-bounds-gep "LLVMBuildInBoundsGEP") value
  (b builder)
  (pointer value) (indices (carray value)) (num-indices :unsigned-int)
  (name :string))
(defun build-in-bounds-gep (b pointer indices name)
  (%build-in-bounds-gep b pointer indices (length indices) name))
(defcfun* "LLVMBuildStructGEP" value
  (b builder) (pointer value) (idx :unsigned-int) (name :string))
(defcfun* "LLVMBuildGlobalString" value
  (b builder) (str :string) (name :string))
(defcfun (build-global-string-pointer "LLVMBuildGlobalStringPtr") value
  (b builder) (str :string) (name :string))

(defcfun* "LLVMBuildTrunc" value
  (builder builder) (val value) (dest-ty type) (name :string))
(defcfun* "LLVMBuildZExt" value
  (builder builder) (val value) (dest-ty type) (name :string))
(defcfun* "LLVMBuildSExt" value
  (builder builder) (val value) (dest-ty type) (name :string))
(defcfun* "LLVMBuildFPToUI" value
  (builder builder) (val value) (dest-ty type) (name :string))
(defcfun* "LLVMBuildFPToSI" value
  (builder builder) (val value) (dest-ty type) (name :string))
(defcfun* "LLVMBuildUIToFP" value
  (builder builder) (val value) (dest-ty type) (name :string))
(defcfun* "LLVMBuildSIToFP" value
  (builder builder) (val value) (dest-ty type) (name :string))
(defcfun* "LLVMBuildFPTrunc" value
  (builder builder) (val value) (dest-ty type) (name :string))
(defcfun* "LLVMBuildFPExt" value
  (builder builder) (val value) (dest-ty type) (name :string))
(defcfun (build-pointer-to-int "LLVMBuildPtrToInt") value
  (builder builder) (val value) (dest-ty type) (name :string))
(defcfun (build-int-to-pointer "LLVMBuildIntToPtr") value
  (builder builder) (val value) (dest-ty type) (name :string))
(defcfun* "LLVMBuildBitCast" value
  (builder builder) (val value) (dest-ty type) (name :string))
(defcfun* "LLVMBuildZExtOrBitCast" value
  (builder builder) (val value) (dest-ty type) (name :string))
(defcfun* "LLVMBuildSExtOrBitCast" value
  (builder builder) (val value) (dest-ty type) (name :string))
(defcfun* "LLVMBuildTruncOrBitCast" value
  (builder builder) (val value) (dest-ty type) (name :string))
(defcfun* "LLVMBuildPointerCast" value
  (builder builder) (val value) (dest-ty type) (name :string))
(defcfun* "LLVMBuildIntCast" value
  (builder builder) (val value) (dest-ty type) (name :string))
(defcfun* "LLVMBuildFPCast" value
  (builder builder) (val value) (dest-ty type) (name :string))

(defcfun* "LLVMBuildICmp" value
  (builder builder) (op int-predicate) (lhs value) (rhs value) (name :string))
(defcfun* "LLVMBuildFCmp" value
  (builder builder) (op real-predicate) (lhs value) (rhs value) (name :string))

(defcfun* "LLVMBuildPhi" value (builder builder) (ty type) (name :string))
(defcfun (%build-call "LLVMBuildCall") value
  (builder builder)
  (fn value) (args (carray value)) (num-args :unsigned-int)
  (name :string))
(defun build-call (builder fn args name)
  (%build-call builder fn args (length args) name))
(defcfun* "LLVMBuildSelect" value
  (builder builder) (if value) (then value) (else value) (name :string))
(defcfun* "LLVMBuildVAArg" value
  (builder builder) (list value) (ty type) (name :string))
(defcfun* "LLVMBuildExtractElement" value
  (builder builder) (vec-val value) (index value) (name :string))
(defcfun* "LLVMBuildInsertElement" value
  (builder builder)
  (vec-val value) (elt-val value) (index value)
  (name :string))
(defcfun* "LLVMBuildShuffleVector" value
  (builder builder) (v1 value) (v2 value) (mask value) (name :string))
(defcfun* "LLVMBuildExtractValue" value
  (builder builder) (agg-val value) (index :unsigned-int) (name :string))
(defcfun* "LLVMBuildInsertValue" value
  (builder builder)
  (agg-val value) (elt-val value) (index :unsigned-int)
  (name :string))

(defcfun (build-nullp "LLVMBuildIsNull") value
  (builder builder) (val value) (name :string))
(defcfun (build-not-null-p "LLVMBuildIsNotNull") value
  (builder builder) (val value) (name :string))
(defcfun (build-pointer-diff "LLVMBuildPtrDiff") value
  (builder builder) (lhs value) (rhs value) (name :string))
