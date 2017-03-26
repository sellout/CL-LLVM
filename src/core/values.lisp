(in-package :llvm)

(defcfun* "LLVMTypeOf" type (val value))
(defcfun (value-name "LLVMGetValueName") :string (val value))
(defcfun* "LLVMSetValueName" :void (val value) (name :string))
(defun (setf value-name) (name val)
  (set-value-name val name)
  name)
(defcfun (%dump-value "LLVMDumpValue") :void (val value))
(defun dump-value (val)
  (finish-output *error-output*)
  (%dump-value val))
(defcfun* "LLVMPrintValueToString" :string (m value))

(defcfun* "LLVMPrintValueToString" :string (val value))

(defcfun* "LLVMGetOperand" value (val value) (index :unsigned-int))
(defcfun* "LLVMSetOperand" value (user value) (index :unsigned-int) (val value))
(defcfun* "LLVMGetNumOperands" value (val value))

(defcfun* "LLVMConstNull" value (ty type))
(defcfun* "LLVMConstAllOnes" value (ty type))
(defcfun (undef "LLVMGetUndef") value (ty type))
(defcfun (constantp "LLVMIsConstant") :boolean (val value))
(defcfun (nullp "LLVMIsNull") :boolean (val value))
(defcfun (undefp "LLVMIsUndef") :boolean (val value))
(defcfun* "LLVMConstPointerNull" value (ty type))

(defcfun (%const-int "LLVMConstInt") value
  (int-ty type) (n :unsigned-long-long) (sign-extend :boolean))
(defcfun* "LLVMConstIntOfString" value
  (int-ty type) (text :string) (radix :uint8))
;; NOTE: This is only available in SVN as of revision 119989, and releases after
;;       1.8.
(defcfun* "LLVMConstIntOfArbitraryPrecision" value
  (int-ty type) (num-words :unsigned-int) (words (carray :uint64)))
(defun const-int (int-ty value &optional radix)
  (if (typep value 'string)
      (const-int-of-string int-ty value radix)
      (let* ((+max-primitive-width+ 64)
             (bitmask (1- (expt 2 +max-primitive-width+)))
             (width (width int-ty)))
        (if (<= width +max-primitive-width+)
            (%const-int int-ty (logand value bitmask) nil)
            (let* ((length (ceiling width +max-primitive-width+))
                   (words (loop for num = value
                             then (ash num (- +max-primitive-width+))
                             while (< 0 num)
                             collect (logand num bitmask))))
              (const-int-of-arbitrary-precision int-ty length words))))))
(defcfun (%const-real "LLVMConstReal") value (real-ty type) (n real-double))
(defcfun* "LLVMConstRealOfString" value (real-ty type) (text :string))
(defun const-real (real-ty value)
  (if (typep value 'string)
      (const-real-of-string real-ty value)
      (%const-real real-ty value)))
(defcfun (z-ext-value "LLVMConstIntGetZExtValue") :unsigned-long-long
  (constant-val value))
(defcfun (s-ext-value "LLVMConstIntGetSExtValue") :long-long
  (constant-val value))

(defcfun* "LLVMConstStringInContext" value
  (c context)
  (str :string) (length :unsigned-int)
  (dont-null-terminate :boolean))
;;; FIXME: is it right to hardcode dont-null-terminate here?
(defun const-string (str dont-null-terminate &key (context (global-context)))
  (const-string-in-context context str (length str) dont-null-terminate))
(defcfun* "LLVMConstStructInContext" value
  (c context)
  (constant-vals (carray value)) (count :unsigned-int)
  (packed :boolean))
(defun const-struct (constant-vals packed &key (context (global-context)))
  (const-struct-in-context context constant-vals (length constant-vals) packed))
(defcfun (%const-array "LLVMConstArray") value
  (element-ty type) (constant-vals (carray value)) (length :unsigned-int))
(defun const-array (element-ty constant-vals)
  (%const-array element-ty constant-vals (length constant-vals)))
(defcfun (%const-vector "LLVMConstVector") value
  (scalar-constant-vals (carray value)) (size :unsigned-int))
(defun const-vector (scalar-constant-vals)
  (%const-vector scalar-constant-vals (length scalar-constant-vals)))

(defcfun (const-opcode "LLVMGetConstOpcode") opcode (constant-val value))
(defcfun* "LLVMAlignOf" value (ty type))
(defcfun* "LLVMSizeOf" value (ty type))
(defcfun* "LLVMConstNeg" value (constant-val value))
(defcfun* "LLVMConstFNeg" value (constant-val value))
(defcfun* "LLVMConstNot" value (constant-val value))
(defcfun* "LLVMConstAdd" value (lhs-constant value) (rhs-constant value))
(defcfun* "LLVMConstNSWAdd" value (lhs-constant value) (rhs-constant value))
(defcfun* "LLVMConstFAdd" value (lhs-constant value) (rhs-constant value))
(defcfun* "LLVMConstSub" value (lhs-constant value) (rhs-constant value))
(defcfun* "LLVMConstFSub" value (lhs-constant value) (rhs-constant value))
(defcfun* "LLVMConstMul" value (lhs-constant value) (rhs-constant value))
(defcfun* "LLVMConstFMul" value (lhs-constant value) (rhs-constant value))
(defcfun* "LLVMConstUDiv" value (lhs-constant value) (rhs-constant value))
(defcfun* "LLVMConstSDiv" value (lhs-constant value) (rhs-constant value))
(defcfun* "LLVMConstExactSDiv" value (lhs-constant value) (rhs-constant value))
(defcfun* "LLVMConstFDiv" value (lhs-constant value) (rhs-constant value))
(defcfun* "LLVMConstURem" value (lhs-constant value) (rhs-constant value))
(defcfun* "LLVMConstSRem" value (lhs-constant value) (rhs-constant value))
(defcfun* "LLVMConstFRem" value (lhs-constant value) (rhs-constant value))
(defcfun* "LLVMConstAnd" value (lhs-constant value) (rhs-constant value))
(defcfun* "LLVMConstOr" value (lhs-constant value) (rhs-constant value))
(defcfun* "LLVMConstXor" value (lhs-constant value) (rhs-constant value))
(defcfun* "LLVMConstICmp" value
  (predicate int-predicate) (lhs-constant value) (rhs-constant value))
(defcfun* "LLVMConstFCmp" value
  (predicate real-predicate) (lhs-constant value) (rhs-constant value))
(defcfun* "LLVMConstShl" value (lhs-constant value) (rhs-constant value))
(defcfun* "LLVMConstLShr" value (lhs-constant value) (rhs-constant value))
(defcfun* "LLVMConstAShr" value (lhs-constant value) (rhs-constant value))
(defcfun (%const-gep "LLVMConstGEP") value
  (constant-val value)
  (constant-indices (carray value)) (num-indices :unsigned-int))
(defun const-gep (constant-val constant-indices)
  (%const-gep constant-val constant-indices (length constant-indices)))
(defcfun (%const-in-bounds-gep "LLVMConstInBoundsGEP") value
  (constant-val value)
  (constant-indices (carray value)) (num-indices :unsigned-int))
(defun const-in-bounds-gep (constant-val constant-indices)
  (%const-in-bounds-gep constant-val
                        constant-indices (length constant-indices)))
(defcfun* "LLVMConstTrunc" value (constant-val value) (to-type type))
(defcfun* "LLVMConstSExt" value (constant-val value) (to-type type))
(defcfun* "LLVMConstZExt" value (constant-val value) (to-type type))
(defcfun* "LLVMConstFPTrunc" value (constant-val value) (to-type type))
(defcfun* "LLVMConstFPExt" value (constant-val value) (to-type type))
(defcfun* "LLVMConstUIToFP" value (constant-val value) (to-type type))
(defcfun* "LLVMConstSIToFP" value (constant-val value) (to-type type))
(defcfun* "LLVMConstFPToUI" value (constant-val value) (to-type type))
(defcfun* "LLVMConstFPToSI" value (constant-val value) (to-type type))
(defcfun (const-pointer-to-int "LLVMConstPtrToInt") value
  (constant-val value) (to-type type))
(defcfun (const-int-to-pointer "LLVMConstIntToPtr") value
  (constant-val value) (to-type type))
(defcfun* "LLVMConstBitCast" value (constant-val value) (to-type type))
(defcfun* "LLVMConstZExtOrBitCast" value (constant-val value) (to-type type))
(defcfun* "LLVMConstSExtOrBitCast" value (constant-val value) (to-type type))
(defcfun* "LLVMConstTruncOrBitCast" value (constant-val value) (to-type type))
(defcfun* "LLVMConstPointerCast" value (constant-val value) (to-type type))
(defcfun* "LLVMConstIntCast" value
  (constant-val value) (to-type type) (is-signed :boolean))
(defcfun* "LLVMConstFPCast" value (constant-val value) (to-type type))
(defcfun* "LLVMConstSelect" value
  (constant-condition value) (constant-if-true value) (constant-if-false value))
(defcfun* "LLVMConstExtractElement" value
  (vector-constant value) (index-constant value))
(defcfun* "LLVMConstInsertElement" value
  (vector-constant value) (element-value-constant value) (index-constant value))
(defcfun* "LLVMConstShuffleVector" value
  (vector-a-constant value) (vector-b-constant value) (mask-constant value))
(defcfun (%const-extract-value "LLVMConstExtractValue") value
  (agg-constant value)
  (idx-list (carray :unsigned-int)) (num-idx :unsigned-int))
(defun const-extract-value (agg-constant idx-list)
  (%const-extract-value agg-constant idx-list (length idx-list)))
(defcfun (%const-insert-value "LLVMConstInsertValue") value
  (agg-constant value) (element-value-constant value)
  (idx-list (carray :unsigned-int)) (num-idx :unsigned-int))
(defun const-insert-value (agg-constant element-value-constant idx-list)
  (%const-insert-value agg-constant element-value-constant
                       idx-list (length idx-list)))
(defcfun* "LLVMConstInlineAsm" value
  (ty type) (asm-string :string) (constraints :string)
  (has-side-effects :boolean))
(defcfun* "LLVMBlockAddress" value (f value) (bb basic-block))
(defcfun (global-parent "LLVMGetGlobalParent") module (global value))
(defcfun (declarationp "LLVMIsDeclaration") :boolean (global value))
(defcfun (linkage "LLVMGetLinkage") linkage (global value))
(defcfun* "LLVMSetLinkage" :void (global value) (linkage linkage))
(defun (setf linkage) (linkage global)
  (set-linkage global linkage)
  linkage)
(defcfun (section "LLVMGetSection") :string (global value))
(defcfun* "LLVMSetSection" :void (global value) (section :string))
(defun (setf section) (section global)
  (set-section global section)
  section)
(defcfun (visibility "LLVMGetVisibility") visibility (global value))
(defcfun* "LLVMSetVisibility" :void (global value) (viz visibility))
(defun (setf visibility) (viz global)
  (set-visibility global viz)
  viz)
(defcfun (alignment "LLVMGetAlignment") :unsigned-int (global value))
(defcfun* "LLVMSetAlignment" :void (global value) (bytes :unsigned-int))
(defun (setf alignment) (bytes global)
  (set-alignment global bytes)
  bytes)

(defcfun* "LLVMAddGlobal" value (m module) (ty type) (name :string))
(defcfun (named-global "LLVMGetNamedGlobal") value (m module) (name :string))
(defcfun (first-global "LLVMGetFirstGlobal") value (m module))
(defcfun (last-global "LLVMGetLastGlobal") value (m module))
(defcfun (next-global "LLVMGetNextGlobal") value (global-var value))
(defcfun (previous-global "LLVMGetPreviousGlobal") value (global-var value))
(defcfun* "LLVMDeleteGlobal" :void (global-var value))
(defcfun (initializer "LLVMGetInitializer") value (global-var value))
(defcfun* "LLVMSetInitializer" :void (global-var value) (constant-val value))
(defun (setf initializer) (constant-val global-var)
  (set-initializer global-var constant-val)
  constant-val)
(defcfun (thread-local-p "LLVMIsThreadLocal") :boolean (global-var value))
(defcfun* "LLVMSetThreadLocal" :void
  (global-var value) (is-thread-local :boolean))
(defun (setf thread-local-p) (is-thread-local global-var)
  (set-thread-local global-var is-thread-local)
  is-thread-local)
(defcfun (global-constant-p "LLVMIsGlobalConstant") :boolean (global-var value))
(defcfun* "LLVMSetGlobalConstant" :void
  (global-var value) (is-constant :boolean))
(defun (setf global-constant-p) (is-constant global-var)
  (set-global-constant global-var is-constant)
  is-constant)

(defcfun* "LLVMAddAlias" value
  (m module) (ty type) (aliasee value) (name :string))

(defcfun* "LLVMAddFunction" value (m module) (name :string) (function-ty type))
(defcfun (named-function "LLVMGetNamedFunction") value
  (m module) (name :string))
(defcfun (first-function "LLVMGetFirstFunction") value (m module))
(defcfun (last-function "LLVMGetLastFunction") value (m module))
(defcfun (next-function "LLVMGetNextFunction") value (fn value))
(defcfun (previous-function "LLVMGetPreviousFunction") value (fn value))
(defcfun* "LLVMDeleteFunction" :void (fn value))
(defcfun (intrinsic-id "LLVMGetIntrinsicID") :unsigned-int (fn value))
(defcfun (function-calling-convention "LLVMGetFunctionCallConv") calling-convention
  (fn value))
(defcfun* "LLVMSetFunctionCallConv" :void (fn value) (cc calling-convention))
(defun (setf function-calling-convention) (cc fn)
  (set-function-call-conv fn cc)
  cc)
(defcfun (gc "LLVMGetGC") :string (fn value))
(defcfun* "LLVMSetGC" :void (fn value) (name :string))
(defun (setf gc) (name fn)
  (set-gc fn name)
  name)
(defcfun* "LLVMAddFunctionAttr" :void (fn value) (pa attribute))
(defun add-function-attributes (fn &rest attributes)
  (add-function-attr fn attributes))
(defcfun* "LLVMGetFunctionAttr" attribute
  (fn value))
(defun get-function-attribute (function)
  (get-function-attr function))
(defcfun* "LLVMRemoveFunctionAttr" :void (fn value) (pa attribute))
(defun remove-function-attributes (fn &rest attributes)
  (remove-function-attribute fn attributes))

(defcfun* "LLVMCountParams" :unsigned-int (fn value))
(defcfun* "LLVMGetParams" :void (fn value) (params (:pointer value)))
(defun params (fn)
  (with-pointer-to-list (pointer value (count-params fn))
    (get-params fn pointer)))
(defcfun* "LLVMGetParam" value (fn value) (index :unsigned-int))
(defcfun (param-parent "LLVMGetParamParent") value (inst value))
(defcfun (first-param "LLVMGetFirstParam") value (fn value))
(defcfun (last-param "LLVMGetLastParam") value (fn value))
(defcfun (next-param "LLVMGetNextParam") value (arg value))
(defcfun (previous-param "LLVMGetPreviousParam") value (arg value))
(defcfun* "LLVMAddAttribute" :void (arg value) (pa attribute))
(defun add-attributes (arg &rest attributes)
  (add-attribute arg attributes))
(defcfun* "LLVMRemoveAttribute" :void (arg value) (pa attribute))
(defun remove-attributes (arg &rest attributes)
  (remove-attribute arg attributes))
(defcfun* "LLVMSetParamAlignment" :void (arg value) (align :unsigned-int))
(defun (setf param-alignment) (align arg)
  (set-param-alignment arg align)
  align)

(defcfun* "LLVMBasicBlockAsValue" value (bb basic-block))
(defcfun (value-is-basic-block-p "LLVMValueIsBasicBlock") :boolean (val value))
(defcfun* "LLVMValueAsBasicBlock" basic-block (val value))
(defcfun (basic-block-parent "LLVMGetBasicBlockParent") value (bb basic-block))
(defcfun* "LLVMCountBasicBlocks" :unsigned-int (fn value))
(defcfun* "LLVMGetBasicBlocks" :void
  (fn value) (basic-blocks (:pointer basic-block)))
(defun basic-blocks (fn)
  (with-pointer-to-list (pointer basic-block (count-basic-blocks fn))
    (get-basic-blocks fn pointer)))
(defcfun (first-basic-block "LLVMGetFirstBasicBlock") basic-block (fn value))
(defcfun (last-basic-block "LLVMGetLastBasicBlock") basic-block (fn value))
(defcfun (next-basic-block "LLVMGetNextBasicBlock") basic-block
  (bb basic-block))
(defcfun (previous-basic-block "LLVMGetPreviousBasicBlock") basic-block
  (bb basic-block))
(defcfun (entry-basic-block "LLVMGetEntryBasicBlock") basic-block (fn value))

(defcfun* "LLVMAppendBasicBlockInContext" basic-block
  (c context) (fn value) (name :string))
(defun append-basic-block (fn name &key (context (global-context)))
  (append-basic-block-in-context context fn name))
(defcfun* "LLVMInsertBasicBlockInContext" basic-block
  (c context) (insert-block basic-block) (name :string))
(defun insert-basic-block (insert-block name &key (context (global-context)))
  (insert-basic-block-in-context context insert-block name))

(defcfun* "LLVMDeleteBasicBlock" :void (bb basic-block))
(defcfun* "LLVMMoveBasicBlockBefore" :void
  (block basic-block) (move-pos basic-block))
(defcfun* "LLVMMoveBasicBlockAfter" :void
  (block basic-block) (move-pos basic-block))

(defcfun (instruction-parent "LLVMGetInstructionParent") basic-block
  (inst value))
(defcfun (first-instruction "LLVMGetFirstInstruction") value (bb basic-block))
(defcfun (last-instruction "LLVMGetLastInstruction") value (bb basic-block))
(defcfun (next-instruction "LLVMGetNextInstruction") value (inst value))
(defcfun (previous-instruction "LLVMGetPreviousInstruction") value (inst value))

(defcfun* "LLVMSetInstructionCallConv" :void
  (instr value) (cc calling-convention))
(defun (setf instruction-calling-convention) (cc instr)
  (set-instruction-call-conv instr cc)
  cc)
(defcfun (instruction-calling-convention "LLVMGetInstructionCallingConvention")
         calling-convention
  (instr value))
(defcfun* "LLVMAddInstrAttribute" :void
  (instr value) (index :unsigned-int) (attribute attribute))
(defun add-instruction-attributes (instr index &rest attributes)
  (add-instr-attribute instr index attributes))
(defcfun* "LLVMRemoveInstrAttribute" :void
  (instr value) (index :unsigned-int) (attribute attribute))
(defun remove-instruction-attributes (instr index &rest attributes)
  (remove-instr-attribute instr index attributes))
(defcfun* "LLVMSetInstrParamAlignment" :void
  (instr value) (index :unsigned-int) (align :unsigned-int))
(defun (setf instruction-param-alignment) (align instr index)
  (set-instr-param-alignment instr index align)
  align)

(defcfun (tail-call-p "LLVMIsTailCall") :boolean (call-inst value))
(defcfun* "LLVMSetTailCall" :void (call-inst value) (is-tail-call :boolean))
(defun (setf tail-call-p) (is-tail-call call-inst)
  (set-tail-call call-inst is-tail-call)
  is-tail-call)

(defcfun (%add-incoming "LLVMAddIncoming") :void
  (phi-node value)
  (incoming-values (carray value)) (incoming-blocks (carray basic-block))
  (count :unsigned-int))
(defun add-incoming (phi-node incoming-values incoming-blocks)
  (%add-incoming phi-node
                 incoming-values incoming-blocks (length incoming-values)))
(defcfun* "LLVMCountIncoming" :unsigned-int (phi-node value))
(defcfun (incoming-value "LLVMGetIncomingValue") value
  (phi-node value) (index :unsigned-int))
(defcfun (incoming-basic-block "LLVMGetIncomingBasicBlock") basic-block
  (phi-node value) (index :unsigned-int))
