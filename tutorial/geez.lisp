(defpackage #:llvm
  (:use #:cl
	#:cffi))
(in-package :llvm)

(defctype uint8_t :uint8)
(defctype uint32_t :uint32)
(defctype uint64_t :uint64)
(defctype size_t :unsigned-int)
(defctype off_t :int)

(defctype |LLVMDiagnosticHandler| :pointer)
(defctype |LLVMYieldCallback| :pointer)
(defctype |LLVMFatalErrorHandler| :pointer)

(defctype |LLVMOrcSymbolResolverFn| :pointer)
(defctype |LLVMOrcLazyCompileCallbackFn| :pointer)

;;;;Types

;;;include llvm/Support/DataTypes.h
(defctype |LLVMBool| :boolean)
(defctype |LLVMMemoryBufferRef| :pointer)
(defctype |LLVMContextRef| :pointer)
(defctype |LLVMModuleRef| :pointer)
(defctype |LLVMTypeRef| :pointer)
(defctype |LLVMValueRef| :pointer)
(defctype |LLVMBasicBlockRef| :pointer)
(defctype |LLVMBuilderRef| :pointer)
(defctype |LLVMModuleProviderRef| :pointer)
(defctype |LLVMPassManagerRef| :pointer)
(defctype |LLVMPassRegistryRef| :pointer)
(defctype |LLVMUseRef| :pointer)
(defctype |LLVMDiagnosticInfoRef| :pointer)

;;;;Core

;;;include llvm-c/ErrorHandling.h
;;;include llvm-c/Types.h
(defcenum |LLVMAttribute|
  (|LLVMZExtAttribute| 1)
  (|LLVMSExtAttribute| 2)
  (|LLVMNoReturnAttribute| 4)
  (|LLVMInRegAttribute| 8)
  (|LLVMStructRetAttribute| 16)
  (|LLVMNoUnwindAttribute| 32)
  (|LLVMNoAliasAttribute| 64)
  (|LLVMByValAttribute| 128)
  (|LLVMNestAttribute| 256)
  (|LLVMReadNoneAttribute| 512)
  (|LLVMReadOnlyAttribute| 1024)
  (|LLVMNoInlineAttribute| 2048)
  (|LLVMAlwaysInlineAttribute| 4096)
  (|LLVMOptimizeForSizeAttribute| 8192)
  (|LLVMStackProtectAttribute| 16384)
  (|LLVMStackProtectReqAttribute| 32768)
  (|LLVMAlignment| 2031616)
  (|LLVMNoCaptureAttribute| 2097152)
  (|LLVMNoRedZoneAttribute| 4194304)
  (|LLVMNoImplicitFloatAttribute| 8388608)
  (|LLVMNakedAttribute| 16777216)
  (|LLVMInlineHintAttribute| 33554432)
  (|LLVMStackAlignment| 469762048)
  (|LLVMReturnsTwice| 536870912)
  (|LLVMUWTable| 1073741824)
  (|LLVMNonLazyBind| 2147483648))
(defcenum |LLVMOpcode|
  (|LLVMRet| 1)
  (|LLVMBr| 2)
  (|LLVMSwitch| 3)
  (|LLVMIndirectBr| 4)
  (|LLVMInvoke| 5)
  (|LLVMUnreachable| 7)
  (|LLVMAdd| 8)
  (|LLVMFAdd| 9)
  (|LLVMSub| 10)
  (|LLVMFSub| 11)
  (|LLVMMul| 12)
  (|LLVMFMul| 13)
  (|LLVMUDiv| 14)
  (|LLVMSDiv| 15)
  (|LLVMFDiv| 16)
  (|LLVMURem| 17)
  (|LLVMSRem| 18)
  (|LLVMFRem| 19)
  (|LLVMShl| 20)
  (|LLVMLShr| 21)
  (|LLVMAShr| 22)
  (|LLVMAnd| 23)
  (|LLVMOr| 24)
  (|LLVMXor| 25)
  (|LLVMAlloca| 26)
  (|LLVMLoad| 27)
  (|LLVMStore| 28)
  (|LLVMGetElementPtr| 29)
  (|LLVMTrunc| 30)
  (|LLVMZExt| 31)
  (|LLVMSExt| 32)
  (|LLVMFPToUI| 33)
  (|LLVMFPToSI| 34)
  (|LLVMUIToFP| 35)
  (|LLVMSIToFP| 36)
  (|LLVMFPTrunc| 37)
  (|LLVMFPExt| 38)
  (|LLVMPtrToInt| 39)
  (|LLVMIntToPtr| 40)
  (|LLVMBitCast| 41)
  (|LLVMAddrSpaceCast| 60)
  (|LLVMICmp| 42)
  (|LLVMFCmp| 43)
  (|llvmphi| 44)
  (|LLVMCall| 45)
  (|LLVMSelect| 46)
  (|LLVMUserOp1| 47)
  (|LLVMUserOp2| 48)
  (|LLVMVAArg| 49)
  (|LLVMExtractElement| 50)
  (|LLVMInsertElement| 51)
  (|LLVMShuffleVector| 52)
  (|LLVMExtractValue| 53)
  (|LLVMInsertValue| 54)
  (|LLVMFence| 55)
  (|LLVMAtomicCmpXchg| 56)
  (|LLVMAtomicRMW| 57)
  (|LLVMResume| 58)
  (|LLVMLandingPad| 59)
  (|LLVMCleanupRet| 61)
  (|LLVMCatchRet| 62)
  (|LLVMCatchPad| 63)
  (|LLVMCleanupPad| 64)
  (|LLVMCatchSwitch| 65))
(defcenum |LLVMTypeKind|
  |LLVMVoidTypeKind|
  |LLVMHalfTypeKind|
  |LLVMFloatTypeKind|
  |LLVMDoubleTypeKind|
  |LLVMX86_FP80TypeKind|
  |LLVMFP128TypeKind|
  |LLVMPPC_FP128TypeKind|
  |LLVMLabelTypeKind|
  |LLVMIntegerTypeKind|
  |LLVMFunctionTypeKind|
  |LLVMStructTypeKind|
  |LLVMArrayTypeKind|
  |LLVMPointerTypeKind|
  |LLVMVectorTypeKind|
  |LLVMMetadataTypeKind|
  |LLVMX86_MMXTypeKind|
  |LLVMTokenTypeKind|)
(defcenum |LLVMLinkage|
  |LLVMExternalLinkage|
  |LLVMAvailableExternallyLinkage|
  |LLVMLinkOnceAnyLinkage|
  |LLVMLinkOnceODRLinkage|
  |LLVMLinkOnceODRAutoHideLinkage|
  |LLVMWeakAnyLinkage|
  |LLVMWeakODRLinkage|
  |LLVMAppendingLinkage|
  |LLVMInternalLinkage|
  |LLVMPrivateLinkage|
  |LLVMDLLImportLinkage|
  |LLVMDLLExportLinkage|
  |LLVMExternalWeakLinkage|
  |LLVMGhostLinkage|
  |LLVMCommonLinkage|
  |LLVMLinkerPrivateLinkage|
  |LLVMLinkerPrivateWeakLinkage|)
(defcenum |LLVMVisibility|
  |LLVMDefaultVisibility|
  |LLVMHiddenVisibility|
  |LLVMProtectedVisibility|)
(defcenum |LLVMDLLStorageClass|
  (|LLVMDefaultStorageClass| 0)
  (|LLVMDLLImportStorageClass| 1)
  (|LLVMDLLExportStorageClass| 2))
(defcenum |LLVMCallConv|
  (|LLVMCCallConv| 0)
  (|LLVMFastCallConv| 8)
  (|LLVMColdCallConv| 9)
  (|LLVMWebKitJSCallConv| 12)
  (|LLVMAnyRegCallConv| 13)
  (|LLVMX86StdcallCallConv| 64)
  (|LLVMX86FastcallCallConv| 65))
(defcenum |LLVMIntPredicate|
  (|LLVMIntEQ| 32)
  |LLVMIntNE|
  |LLVMIntUGT|
  |LLVMIntUGE|
  |LLVMIntULT|
  |LLVMIntULE|
  |LLVMIntSGT|
  |LLVMIntSGE|
  |LLVMIntSLT|
  |LLVMIntSLE|)
(defcenum |LLVMRealPredicate|
  |LLVMRealPredicateFalse|
  |LLVMRealOEQ|
  |LLVMRealOGT|
  |LLVMRealOGE|
  |LLVMRealOLT|
  |LLVMRealOLE|
  |LLVMRealONE|
  |LLVMRealORD|
  |LLVMRealUNO|
  |LLVMRealUEQ|
  |LLVMRealUGT|
  |LLVMRealUGE|
  |LLVMRealULT|
  |LLVMRealULE|
  |LLVMRealUNE|
  |LLVMRealPredicateTrue|)
(defcenum |LLVMLandingPadClauseTy|
  |LLVMLandingPadCatch|
  |LLVMLandingPadFilter|)
(defcenum |LLVMThreadLocalMode|
  (|LLVMNotThreadLocal| 0)
  |LLVMGeneralDynamicTLSModel|
  |LLVMLocalDynamicTLSModel|
  |LLVMInitialExecTLSModel|
  |LLVMLocalExecTLSModel|)
(defcenum |LLVMAtomicOrdering|
  (|LLVMAtomicOrderingNotAtomic| 0)
  (|LLVMAtomicOrderingUnordered| 1)
  (|LLVMAtomicOrderingMonotonic| 2)
  (|LLVMAtomicOrderingAcquire| 4)
  (|LLVMAtomicOrderingRelease| 5)
  (|LLVMAtomicOrderingAcquireRelease| 6)
  (|LLVMAtomicOrderingSequentiallyConsistent| 7))
(defcenum |LLVMAtomicRMWBinOp|
  |LLVMAtomicRMWBinOpXchg|
  |LLVMAtomicRMWBinOpAdd|
  |LLVMAtomicRMWBinOpSub|
  |LLVMAtomicRMWBinOpAnd|
  |LLVMAtomicRMWBinOpNand|
  |LLVMAtomicRMWBinOpOr|
  |LLVMAtomicRMWBinOpXor|
  |LLVMAtomicRMWBinOpMax|
  |LLVMAtomicRMWBinOpMin|
  |LLVMAtomicRMWBinOpUMax|
  |LLVMAtomicRMWBinOpUMin|)
(defcenum |LLVMDiagnosticSeverity|
  |LLVMDSError|
  |LLVMDSWarning|
  |LLVMDSRemark|
  |LLVMDSNote|)
(defcfun (-initialize-core "LLVMInitializeCore") :void
  (|r| |LLVMPassRegistryRef|))
(defcfun (-shutdown "LLVMShutdown") :void)
(defcfun (-create-message "LLVMCreateMessage") (:pointer :char)
  (|Message| (:pointer :char)))
(defcfun (-dispose-message "LLVMDisposeMessage") :void
  (|Message| (:pointer :char)))
(defcfun (-context-create "LLVMContextCreate") |LLVMContextRef|)
(defcfun (-get-global-context "LLVMGetGlobalContext") |LLVMContextRef|)
(defcfun (-context-set-diagnostic-handler
          "LLVMContextSetDiagnosticHandler") :void
  (|c| |LLVMContextRef|)
  (|Handler| |LLVMDiagnosticHandler|)
  (|DiagnosticContext| (:pointer :void)))
(defcfun (-context-set-yield-callback "LLVMContextSetYieldCallback") :void
  (|c| |LLVMContextRef|)
  (|Callback| |LLVMYieldCallback|)
  (|OpaqueHandle| (:pointer :void)))
(defcfun (-context-dispose "LLVMContextDispose") :void
  (|c| |LLVMContextRef|))
(defcfun (-get-diag-info-description
          "LLVMGetDiagInfoDescription") (:pointer :char)
  (|di| |LLVMDiagnosticInfoRef|))
(defcfun (-get-diag-info-severity
          "LLVMGetDiagInfoSeverity") |LLVMDiagnosticSeverity|
  (|di| |LLVMDiagnosticInfoRef|))
(defcfun (-get-m-d-kind-i-d-in-context
          "LLVMGetMDKindIDInContext") :unsigned-int
  (|c| |LLVMContextRef|)
  (|Name| (:pointer :char))
  (|SLen| :unsigned-int))
(defcfun (-get-m-d-kind-i-d "LLVMGetMDKindID") :unsigned-int
  (|Name| (:pointer :char))
  (|SLen| :unsigned-int))
(defcfun (-module-create-with-name
          "LLVMModuleCreateWithName") |LLVMModuleRef|
  (|ModuleID| (:pointer :char)))
(defcfun (-module-create-with-name-in-context
          "LLVMModuleCreateWithNameInContext") |LLVMModuleRef|
  (|ModuleID| (:pointer :char))
  (|c| |LLVMContextRef|))
(defcfun (-clone-module "LLVMCloneModule") |LLVMModuleRef|
  (|m| |LLVMModuleRef|))
(defcfun (-dispose-module "LLVMDisposeModule") :void
  (|m| |LLVMModuleRef|))
(defcfun (-get-data-layout "LLVMGetDataLayout") (:pointer :char)
  (|m| |LLVMModuleRef|))
(defcfun (-set-data-layout "LLVMSetDataLayout") :void
  (|m| |LLVMModuleRef|)
  (|Triple| (:pointer :char)))
(defcfun (-get-target "LLVMGetTarget") (:pointer :char)
  (|m| |LLVMModuleRef|))
(defcfun (-set-target "LLVMSetTarget") :void
  (|m| |LLVMModuleRef|)
  (|Triple| (:pointer :char)))
(defcfun (-dump-module "LLVMDumpModule") :void
  (|m| |LLVMModuleRef|))
(defcfun (-print-module-to-file "LLVMPrintModuleToFile") |LLVMBool|
  (|m| |LLVMModuleRef|)
  (|Filename| (:pointer :char))
  (|ErrorMessage| (:pointer (:pointer :char))))
(defcfun (-print-module-to-string "LLVMPrintModuleToString") (:pointer
                                                                   :char)
  (|m| |LLVMModuleRef|))
(defcfun (-set-module-inline-asm "LLVMSetModuleInlineAsm") :void
  (|m| |LLVMModuleRef|)
  (|Asm| (:pointer :char)))
(defcfun (-get-module-context "LLVMGetModuleContext") |LLVMContextRef|
  (|m| |LLVMModuleRef|))
(defcfun (-get-type-by-name "LLVMGetTypeByName") |LLVMTypeRef|
  (|m| |LLVMModuleRef|)
  (|Name| (:pointer :char)))
(defcfun (-get-named-metadata-num-operands
          "LLVMGetNamedMetadataNumOperands") :unsigned-int
  (|m| |LLVMModuleRef|)
  (name (:pointer :char)))
(defcfun (-get-named-metadata-operands
          "LLVMGetNamedMetadataOperands") :void
  (|m| |LLVMModuleRef|)
  (name (:pointer :char))
  (|Dest| (:pointer |LLVMValueRef|)))
(defcfun (-add-named-metadata-operand "LLVMAddNamedMetadataOperand") :void
  (|m| |LLVMModuleRef|)
  (name (:pointer :char))
  (|Val| |LLVMValueRef|))
(defcfun (-add-function "LLVMAddFunction") |LLVMValueRef|
  (|m| |LLVMModuleRef|)
  (|Name| (:pointer :char))
  (|FunctionTy| |LLVMTypeRef|))
(defcfun (-get-named-function "LLVMGetNamedFunction") |LLVMValueRef|
  (|m| |LLVMModuleRef|)
  (|Name| (:pointer :char)))
(defcfun (-get-first-function "LLVMGetFirstFunction") |LLVMValueRef|
  (|m| |LLVMModuleRef|))
(defcfun (-get-last-function "LLVMGetLastFunction") |LLVMValueRef|
  (|m| |LLVMModuleRef|))
(defcfun (-get-next-function "LLVMGetNextFunction") |LLVMValueRef|
  (|Fn| |LLVMValueRef|))
(defcfun (-get-previous-function "LLVMGetPreviousFunction") |LLVMValueRef|
  (|Fn| |LLVMValueRef|))
(defcfun (-get-type-kind "LLVMGetTypeKind") |LLVMTypeKind|
  (|Ty| |LLVMTypeRef|))
(defcfun (-type-is-sized "LLVMTypeIsSized") |LLVMBool|
  (|Ty| |LLVMTypeRef|))
(defcfun (-get-type-context "LLVMGetTypeContext") |LLVMContextRef|
  (|Ty| |LLVMTypeRef|))
(defcfun (-dump-type "LLVMDumpType") :void
  (|Val| |LLVMTypeRef|))
(defcfun (-print-type-to-string "LLVMPrintTypeToString") (:pointer :char)
  (|Val| |LLVMTypeRef|))
(defcfun (-int1-type-in-context "LLVMInt1TypeInContext") |LLVMTypeRef|
  (|c| |LLVMContextRef|))
(defcfun (-int8-type-in-context "LLVMInt8TypeInContext") |LLVMTypeRef|
  (|c| |LLVMContextRef|))
(defcfun (-int16-type-in-context "LLVMInt16TypeInContext") |LLVMTypeRef|
  (|c| |LLVMContextRef|))
(defcfun (-int32-type-in-context "LLVMInt32TypeInContext") |LLVMTypeRef|
  (|c| |LLVMContextRef|))
(defcfun (-int64-type-in-context "LLVMInt64TypeInContext") |LLVMTypeRef|
  (|c| |LLVMContextRef|))
(defcfun (-int128-type-in-context "LLVMInt128TypeInContext") |LLVMTypeRef|
  (|c| |LLVMContextRef|))
(defcfun (-int-type-in-context "LLVMIntTypeInContext") |LLVMTypeRef|
  (|c| |LLVMContextRef|)
  (|NumBits| :unsigned-int))
(defcfun (-int1-type "LLVMInt1Type") |LLVMTypeRef|)
(defcfun (-int8-type "LLVMInt8Type") |LLVMTypeRef|)
(defcfun (-int16-type "LLVMInt16Type") |LLVMTypeRef|)
(defcfun (-int32-type "LLVMInt32Type") |LLVMTypeRef|)
(defcfun (-int64-type "LLVMInt64Type") |LLVMTypeRef|)
(defcfun (-int128-type "LLVMInt128Type") |LLVMTypeRef|)
(defcfun (-int-type "LLVMIntType") |LLVMTypeRef|
  (|NumBits| :unsigned-int))
(defcfun (-get-int-type-width "LLVMGetIntTypeWidth") :unsigned-int
  (|IntegerTy| |LLVMTypeRef|))
(defcfun (-half-type-in-context "LLVMHalfTypeInContext") |LLVMTypeRef|
  (|c| |LLVMContextRef|))
(defcfun (-float-type-in-context "LLVMFloatTypeInContext") |LLVMTypeRef|
  (|c| |LLVMContextRef|))
(defcfun (-double-type-in-context "LLVMDoubleTypeInContext") |LLVMTypeRef|
  (|c| |LLVMContextRef|))
(defcfun (-x86-f-p80-type-in-context
          "LLVMX86FP80TypeInContext") |LLVMTypeRef|
  (|c| |LLVMContextRef|))
(defcfun (-f-p128-type-in-context "LLVMFP128TypeInContext") |LLVMTypeRef|
  (|c| |LLVMContextRef|))
(defcfun (-p-p-c-f-p128-type-in-context
          "LLVMPPCFP128TypeInContext") |LLVMTypeRef|
  (|c| |LLVMContextRef|))
(defcfun (-half-type "LLVMHalfType") |LLVMTypeRef|)
(defcfun (-float-type "LLVMFloatType") |LLVMTypeRef|)
(defcfun (-double-type "LLVMDoubleType") |LLVMTypeRef|)
(defcfun (-x86-f-p80-type "LLVMX86FP80Type") |LLVMTypeRef|)
(defcfun (-f-p128-type "LLVMFP128Type") |LLVMTypeRef|)
(defcfun (-p-p-c-f-p128-type "LLVMPPCFP128Type") |LLVMTypeRef|)
(defcfun (-function-type "LLVMFunctionType") |LLVMTypeRef|
  (|ReturnType| |LLVMTypeRef|)
  (|ParamTypes| (:pointer |LLVMTypeRef|))
  (|ParamCount| :unsigned-int)
  (|IsVarArg| |LLVMBool|))
(defcfun (-is-function-var-arg "LLVMIsFunctionVarArg") |LLVMBool|
  (|FunctionTy| |LLVMTypeRef|))
(defcfun (-get-return-type "LLVMGetReturnType") |LLVMTypeRef|
  (|FunctionTy| |LLVMTypeRef|))
(defcfun (-count-param-types "LLVMCountParamTypes") :unsigned-int
  (|FunctionTy| |LLVMTypeRef|))
(defcfun (-get-param-types "LLVMGetParamTypes") :void
  (|FunctionTy| |LLVMTypeRef|)
  (|Dest| (:pointer |LLVMTypeRef|)))
(defcfun (-struct-type-in-context "LLVMStructTypeInContext") |LLVMTypeRef|
  (|c| |LLVMContextRef|)
  (|ElementTypes| (:pointer |LLVMTypeRef|))
  (|ElementCount| :unsigned-int)
  (|Packed| |LLVMBool|))
(defcfun (-struct-type "LLVMStructType") |LLVMTypeRef|
  (|ElementTypes| (:pointer |LLVMTypeRef|))
  (|ElementCount| :unsigned-int)
  (|Packed| |LLVMBool|))
(defcfun (-struct-create-named "LLVMStructCreateNamed") |LLVMTypeRef|
  (|c| |LLVMContextRef|)
  (|Name| (:pointer :char)))
(defcfun (-get-struct-name "LLVMGetStructName") (:pointer :char)
  (|Ty| |LLVMTypeRef|))
(defcfun (-struct-set-body "LLVMStructSetBody") :void
  (|StructTy| |LLVMTypeRef|)
  (|ElementTypes| (:pointer |LLVMTypeRef|))
  (|ElementCount| :unsigned-int)
  (|Packed| |LLVMBool|))
(defcfun (-count-struct-element-types
          "LLVMCountStructElementTypes") :unsigned-int
  (|StructTy| |LLVMTypeRef|))
(defcfun (-get-struct-element-types "LLVMGetStructElementTypes") :void
  (|StructTy| |LLVMTypeRef|)
  (|Dest| (:pointer |LLVMTypeRef|)))
(defcfun (-struct-get-type-at-index
          "LLVMStructGetTypeAtIndex") |LLVMTypeRef|
  (|StructTy| |LLVMTypeRef|)
  (i :unsigned-int))
(defcfun (-is-packed-struct "LLVMIsPackedStruct") |LLVMBool|
  (|StructTy| |LLVMTypeRef|))
(defcfun (-is-opaque-struct "LLVMIsOpaqueStruct") |LLVMBool|
  (|StructTy| |LLVMTypeRef|))
(defcfun (-get-element-type "LLVMGetElementType") |LLVMTypeRef|
  (|Ty| |LLVMTypeRef|))
(defcfun (-array-type "LLVMArrayType") |LLVMTypeRef|
  (|ElementType| |LLVMTypeRef|)
  (|ElementCount| :unsigned-int))
(defcfun (-get-array-length "LLVMGetArrayLength") :unsigned-int
  (|ArrayTy| |LLVMTypeRef|))
(defcfun (-pointer-type "LLVMPointerType") |LLVMTypeRef|
  (|ElementType| |LLVMTypeRef|)
  (|AddressSpace| :unsigned-int))
(defcfun (-get-pointer-address-space
          "LLVMGetPointerAddressSpace") :unsigned-int
  (|PointerTy| |LLVMTypeRef|))
(defcfun (-vector-type "LLVMVectorType") |LLVMTypeRef|
  (|ElementType| |LLVMTypeRef|)
  (|ElementCount| :unsigned-int))
(defcfun (-get-vector-size "LLVMGetVectorSize") :unsigned-int
  (|VectorTy| |LLVMTypeRef|))
(defcfun (-void-type-in-context "LLVMVoidTypeInContext") |LLVMTypeRef|
  (|c| |LLVMContextRef|))
(defcfun (-label-type-in-context "LLVMLabelTypeInContext") |LLVMTypeRef|
  (|c| |LLVMContextRef|))
(defcfun (-x86-m-m-x-type-in-context
          "LLVMX86MMXTypeInContext") |LLVMTypeRef|
  (|c| |LLVMContextRef|))
(defcfun (-void-type "LLVMVoidType") |LLVMTypeRef|)
(defcfun (-label-type "LLVMLabelType") |LLVMTypeRef|)
(defcfun (-x86-m-m-x-type "LLVMX86MMXType") |LLVMTypeRef|)
(defcfun (-type-of "LLVMTypeOf") |LLVMTypeRef|
  (|Val| |LLVMValueRef|))
(defcfun (-get-value-name "LLVMGetValueName") (:pointer :char)
  (|Val| |LLVMValueRef|))
(defcfun (-set-value-name "LLVMSetValueName") :void
  (|Val| |LLVMValueRef|)
  (|Name| (:pointer :char)))
(defcfun (-dump-value "LLVMDumpValue") :void
  (|Val| |LLVMValueRef|))
(defcfun (-print-value-to-string "LLVMPrintValueToString") (:pointer :char)
  (|Val| |LLVMValueRef|))
(defcfun (-replace-all-uses-with "LLVMReplaceAllUsesWith") :void
  (|OldVal| |LLVMValueRef|)
  (|NewVal| |LLVMValueRef|))
(defcfun (-is-constant "LLVMIsConstant") |LLVMBool|
  (|Val| |LLVMValueRef|))
(defcfun (-is-undef "LLVMIsUndef") |LLVMBool|
  (|Val| |LLVMValueRef|))
(defcfun (-is-a-m-d-node "LLVMIsAMDNode") |LLVMValueRef|
  (|Val| |LLVMValueRef|))
(defcfun (-is-a-m-d-string "LLVMIsAMDString") |LLVMValueRef|
  (|Val| |LLVMValueRef|))
(defcfun (-get-first-use "LLVMGetFirstUse") |LLVMUseRef|
  (|Val| |LLVMValueRef|))
(defcfun (-get-next-use "LLVMGetNextUse") |LLVMUseRef|
  (|u| |LLVMUseRef|))
(defcfun (-get-user "LLVMGetUser") |LLVMValueRef|
  (|u| |LLVMUseRef|))
(defcfun (-get-used-value "LLVMGetUsedValue") |LLVMValueRef|
  (|u| |LLVMUseRef|))
(defcfun (-get-operand "LLVMGetOperand") |LLVMValueRef|
  (|Val| |LLVMValueRef|)
  (|Index| :unsigned-int))
(defcfun (-get-operand-use "LLVMGetOperandUse") |LLVMUseRef|
  (|Val| |LLVMValueRef|)
  (|Index| :unsigned-int))
(defcfun (-set-operand "LLVMSetOperand") :void
  (|User| |LLVMValueRef|)
  (|Index| :unsigned-int)
  (|Val| |LLVMValueRef|))
(defcfun (-get-num-operands "LLVMGetNumOperands") :int
  (|Val| |LLVMValueRef|))
(defcfun (-const-null "LLVMConstNull") |LLVMValueRef|
  (|Ty| |LLVMTypeRef|))
(defcfun (-const-all-ones "LLVMConstAllOnes") |LLVMValueRef|
  (|Ty| |LLVMTypeRef|))
(defcfun (-get-undef "LLVMGetUndef") |LLVMValueRef|
  (|Ty| |LLVMTypeRef|))
(defcfun (-is-null "LLVMIsNull") |LLVMBool|
  (|Val| |LLVMValueRef|))
(defcfun (-const-pointer-null "LLVMConstPointerNull") |LLVMValueRef|
  (|Ty| |LLVMTypeRef|))
(defcfun (-const-int "LLVMConstInt") |LLVMValueRef|
  (|IntTy| |LLVMTypeRef|)
  (|n| :unsigned-long-long)
  (|SignExtend| |LLVMBool|))
#+nil
(defcfun (-const-int-of-arbitrary-precision
          "LLVMConstIntOfArbitraryPrecision") |LLVMValueRef|
  (|IntTy| |LLVMTypeRef|)
  (|NumWords| :unsigned-int)
  ((:[] nil) :const))
(defcfun (-const-int-of-string "LLVMConstIntOfString") |LLVMValueRef|
  (|IntTy| |LLVMTypeRef|)
  (|Text| (:pointer :char))
  (|Radix| uint8_t))
(defcfun (-const-int-of-string-and-size
          "LLVMConstIntOfStringAndSize") |LLVMValueRef|
  (|IntTy| |LLVMTypeRef|)
  (|Text| (:pointer :char))
  (|SLen| :unsigned-int)
  (|Radix| uint8_t))
(defcfun (-const-real "LLVMConstReal") |LLVMValueRef|
  (|RealTy| |LLVMTypeRef|)
  (|n| :double))
(defcfun (-const-real-of-string "LLVMConstRealOfString") |LLVMValueRef|
  (|RealTy| |LLVMTypeRef|)
  (|Text| (:pointer :char)))
(defcfun (-const-real-of-string-and-size
          "LLVMConstRealOfStringAndSize") |LLVMValueRef|
  (|RealTy| |LLVMTypeRef|)
  (|Text| (:pointer :char))
  (|SLen| :unsigned-int))
(defcfun (-const-int-get-z-ext-value
          "LLVMConstIntGetZExtValue") :unsigned-long-long
  (|ConstantVal| |LLVMValueRef|))
(defcfun (-const-int-get-s-ext-value "LLVMConstIntGetSExtValue") :long-long
  (|ConstantVal| |LLVMValueRef|))
(defcfun (-const-real-get-double "LLVMConstRealGetDouble") :double
  (|ConstantVal| |LLVMValueRef|)
  (|losesInfo| (:pointer |LLVMBool|)))
(defcfun (-const-string-in-context
          "LLVMConstStringInContext") |LLVMValueRef|
  (|c| |LLVMContextRef|)
  (|Str| (:pointer :char))
  (|Length| :unsigned-int)
  (|DontNullTerminate| |LLVMBool|))
(defcfun (-const-string "LLVMConstString") |LLVMValueRef|
  (|Str| (:pointer :char))
  (|Length| :unsigned-int)
  (|DontNullTerminate| |LLVMBool|))
(defcfun (-is-constant-string "LLVMIsConstantString") |LLVMBool|
  (c |LLVMValueRef|))
(defcfun (-get-as-string "LLVMGetAsString") (:pointer :char)
  (c |LLVMValueRef|)
  (out (:pointer size_t)))
(defcfun (-const-struct-in-context
          "LLVMConstStructInContext") |LLVMValueRef|
  (|c| |LLVMContextRef|)
  (|ConstantVals| (:pointer |LLVMValueRef|))
  (|Count| :unsigned-int)
  (|Packed| |LLVMBool|))
(defcfun (-const-struct "LLVMConstStruct") |LLVMValueRef|
  (|ConstantVals| (:pointer |LLVMValueRef|))
  (|Count| :unsigned-int)
  (|Packed| |LLVMBool|))
(defcfun (-const-array "LLVMConstArray") |LLVMValueRef|
  (|ElementTy| |LLVMTypeRef|)
  (|ConstantVals| (:pointer |LLVMValueRef|))
  (|Length| :unsigned-int))
(defcfun (-const-named-struct "LLVMConstNamedStruct") |LLVMValueRef|
  (|StructTy| |LLVMTypeRef|)
  (|ConstantVals| (:pointer |LLVMValueRef|))
  (|Count| :unsigned-int))
(defcfun (-get-element-as-constant
          "LLVMGetElementAsConstant") |LLVMValueRef|
  (c |LLVMValueRef|)
  (idx :unsigned-int))
(defcfun (-const-vector "LLVMConstVector") |LLVMValueRef|
  (|ScalarConstantVals| (:pointer |LLVMValueRef|))
  (|Size| :unsigned-int))
(defcfun (-get-const-opcode "LLVMGetConstOpcode") |LLVMOpcode|
  (|ConstantVal| |LLVMValueRef|))
(defcfun (-align-of "LLVMAlignOf") |LLVMValueRef|
  (|Ty| |LLVMTypeRef|))
(defcfun (-size-of "LLVMSizeOf") |LLVMValueRef|
  (|Ty| |LLVMTypeRef|))
(defcfun (-const-neg "LLVMConstNeg") |LLVMValueRef|
  (|ConstantVal| |LLVMValueRef|))
(defcfun (-const-n-s-w-neg "LLVMConstNSWNeg") |LLVMValueRef|
  (|ConstantVal| |LLVMValueRef|))
(defcfun (-const-n-u-w-neg "LLVMConstNUWNeg") |LLVMValueRef|
  (|ConstantVal| |LLVMValueRef|))
(defcfun (-const-f-neg "LLVMConstFNeg") |LLVMValueRef|
  (|ConstantVal| |LLVMValueRef|))
(defcfun (-const-not "LLVMConstNot") |LLVMValueRef|
  (|ConstantVal| |LLVMValueRef|))
(defcfun (-const-add "LLVMConstAdd") |LLVMValueRef|
  (|LHSConstant| |LLVMValueRef|)
  (|RHSConstant| |LLVMValueRef|))
(defcfun (-const-n-s-w-add "LLVMConstNSWAdd") |LLVMValueRef|
  (|LHSConstant| |LLVMValueRef|)
  (|RHSConstant| |LLVMValueRef|))
(defcfun (-const-n-u-w-add "LLVMConstNUWAdd") |LLVMValueRef|
  (|LHSConstant| |LLVMValueRef|)
  (|RHSConstant| |LLVMValueRef|))
(defcfun (-const-f-add "LLVMConstFAdd") |LLVMValueRef|
  (|LHSConstant| |LLVMValueRef|)
  (|RHSConstant| |LLVMValueRef|))
(defcfun (-const-sub "LLVMConstSub") |LLVMValueRef|
  (|LHSConstant| |LLVMValueRef|)
  (|RHSConstant| |LLVMValueRef|))
(defcfun (-const-n-s-w-sub "LLVMConstNSWSub") |LLVMValueRef|
  (|LHSConstant| |LLVMValueRef|)
  (|RHSConstant| |LLVMValueRef|))
(defcfun (-const-n-u-w-sub "LLVMConstNUWSub") |LLVMValueRef|
  (|LHSConstant| |LLVMValueRef|)
  (|RHSConstant| |LLVMValueRef|))
(defcfun (-const-f-sub "LLVMConstFSub") |LLVMValueRef|
  (|LHSConstant| |LLVMValueRef|)
  (|RHSConstant| |LLVMValueRef|))
(defcfun (-const-mul "LLVMConstMul") |LLVMValueRef|
  (|LHSConstant| |LLVMValueRef|)
  (|RHSConstant| |LLVMValueRef|))
(defcfun (-const-n-s-w-mul "LLVMConstNSWMul") |LLVMValueRef|
  (|LHSConstant| |LLVMValueRef|)
  (|RHSConstant| |LLVMValueRef|))
(defcfun (-const-n-u-w-mul "LLVMConstNUWMul") |LLVMValueRef|
  (|LHSConstant| |LLVMValueRef|)
  (|RHSConstant| |LLVMValueRef|))
(defcfun (-const-f-mul "LLVMConstFMul") |LLVMValueRef|
  (|LHSConstant| |LLVMValueRef|)
  (|RHSConstant| |LLVMValueRef|))
(defcfun (-const-u-div "LLVMConstUDiv") |LLVMValueRef|
  (|LHSConstant| |LLVMValueRef|)
  (|RHSConstant| |LLVMValueRef|))
(defcfun (-const-s-div "LLVMConstSDiv") |LLVMValueRef|
  (|LHSConstant| |LLVMValueRef|)
  (|RHSConstant| |LLVMValueRef|))
(defcfun (-const-exact-s-div "LLVMConstExactSDiv") |LLVMValueRef|
  (|LHSConstant| |LLVMValueRef|)
  (|RHSConstant| |LLVMValueRef|))
(defcfun (-const-f-div "LLVMConstFDiv") |LLVMValueRef|
  (|LHSConstant| |LLVMValueRef|)
  (|RHSConstant| |LLVMValueRef|))
(defcfun (-const-u-rem "LLVMConstURem") |LLVMValueRef|
  (|LHSConstant| |LLVMValueRef|)
  (|RHSConstant| |LLVMValueRef|))
(defcfun (-const-s-rem "LLVMConstSRem") |LLVMValueRef|
  (|LHSConstant| |LLVMValueRef|)
  (|RHSConstant| |LLVMValueRef|))
(defcfun (-const-f-rem "LLVMConstFRem") |LLVMValueRef|
  (|LHSConstant| |LLVMValueRef|)
  (|RHSConstant| |LLVMValueRef|))
(defcfun (-const-and "LLVMConstAnd") |LLVMValueRef|
  (|LHSConstant| |LLVMValueRef|)
  (|RHSConstant| |LLVMValueRef|))
(defcfun (-const-or "LLVMConstOr") |LLVMValueRef|
  (|LHSConstant| |LLVMValueRef|)
  (|RHSConstant| |LLVMValueRef|))
(defcfun (-const-xor "LLVMConstXor") |LLVMValueRef|
  (|LHSConstant| |LLVMValueRef|)
  (|RHSConstant| |LLVMValueRef|))
(defcfun (-const-i-cmp "LLVMConstICmp") |LLVMValueRef|
  (|Predicate| |LLVMIntPredicate|)
  (|LHSConstant| |LLVMValueRef|)
  (|RHSConstant| |LLVMValueRef|))
(defcfun (-const-f-cmp "LLVMConstFCmp") |LLVMValueRef|
  (|Predicate| |LLVMRealPredicate|)
  (|LHSConstant| |LLVMValueRef|)
  (|RHSConstant| |LLVMValueRef|))
(defcfun (-const-shl "LLVMConstShl") |LLVMValueRef|
  (|LHSConstant| |LLVMValueRef|)
  (|RHSConstant| |LLVMValueRef|))
(defcfun (-const-l-shr "LLVMConstLShr") |LLVMValueRef|
  (|LHSConstant| |LLVMValueRef|)
  (|RHSConstant| |LLVMValueRef|))
(defcfun (-const-a-shr "LLVMConstAShr") |LLVMValueRef|
  (|LHSConstant| |LLVMValueRef|)
  (|RHSConstant| |LLVMValueRef|))
(defcfun (-const-g-e-p "LLVMConstGEP") |LLVMValueRef|
  (|ConstantVal| |LLVMValueRef|)
  (|ConstantIndices| (:pointer |LLVMValueRef|))
  (|NumIndices| :unsigned-int))
(defcfun (-const-in-bounds-g-e-p "LLVMConstInBoundsGEP") |LLVMValueRef|
  (|ConstantVal| |LLVMValueRef|)
  (|ConstantIndices| (:pointer |LLVMValueRef|))
  (|NumIndices| :unsigned-int))
(defcfun (-const-trunc "LLVMConstTrunc") |LLVMValueRef|
  (|ConstantVal| |LLVMValueRef|)
  (|ToType| |LLVMTypeRef|))
(defcfun (-const-s-ext "LLVMConstSExt") |LLVMValueRef|
  (|ConstantVal| |LLVMValueRef|)
  (|ToType| |LLVMTypeRef|))
(defcfun (-const-z-ext "LLVMConstZExt") |LLVMValueRef|
  (|ConstantVal| |LLVMValueRef|)
  (|ToType| |LLVMTypeRef|))
(defcfun (-const-f-p-trunc "LLVMConstFPTrunc") |LLVMValueRef|
  (|ConstantVal| |LLVMValueRef|)
  (|ToType| |LLVMTypeRef|))
(defcfun (-const-f-p-ext "LLVMConstFPExt") |LLVMValueRef|
  (|ConstantVal| |LLVMValueRef|)
  (|ToType| |LLVMTypeRef|))
(defcfun (-const-u-i-to-f-p "LLVMConstUIToFP") |LLVMValueRef|
  (|ConstantVal| |LLVMValueRef|)
  (|ToType| |LLVMTypeRef|))
(defcfun (-const-s-i-to-f-p "LLVMConstSIToFP") |LLVMValueRef|
  (|ConstantVal| |LLVMValueRef|)
  (|ToType| |LLVMTypeRef|))
(defcfun (-const-f-p-to-u-i "LLVMConstFPToUI") |LLVMValueRef|
  (|ConstantVal| |LLVMValueRef|)
  (|ToType| |LLVMTypeRef|))
(defcfun (-const-f-p-to-s-i "LLVMConstFPToSI") |LLVMValueRef|
  (|ConstantVal| |LLVMValueRef|)
  (|ToType| |LLVMTypeRef|))
(defcfun (-const-ptr-to-int "LLVMConstPtrToInt") |LLVMValueRef|
  (|ConstantVal| |LLVMValueRef|)
  (|ToType| |LLVMTypeRef|))
(defcfun (-const-int-to-ptr "LLVMConstIntToPtr") |LLVMValueRef|
  (|ConstantVal| |LLVMValueRef|)
  (|ToType| |LLVMTypeRef|))
(defcfun (-const-bit-cast "LLVMConstBitCast") |LLVMValueRef|
  (|ConstantVal| |LLVMValueRef|)
  (|ToType| |LLVMTypeRef|))
(defcfun (-const-addr-space-cast "LLVMConstAddrSpaceCast") |LLVMValueRef|
  (|ConstantVal| |LLVMValueRef|)
  (|ToType| |LLVMTypeRef|))
(defcfun (-const-z-ext-or-bit-cast "LLVMConstZExtOrBitCast") |LLVMValueRef|
  (|ConstantVal| |LLVMValueRef|)
  (|ToType| |LLVMTypeRef|))
(defcfun (-const-s-ext-or-bit-cast "LLVMConstSExtOrBitCast") |LLVMValueRef|
  (|ConstantVal| |LLVMValueRef|)
  (|ToType| |LLVMTypeRef|))
(defcfun (-const-trunc-or-bit-cast
          "LLVMConstTruncOrBitCast") |LLVMValueRef|
  (|ConstantVal| |LLVMValueRef|)
  (|ToType| |LLVMTypeRef|))
(defcfun (-const-pointer-cast "LLVMConstPointerCast") |LLVMValueRef|
  (|ConstantVal| |LLVMValueRef|)
  (|ToType| |LLVMTypeRef|))
(defcfun (-const-int-cast "LLVMConstIntCast") |LLVMValueRef|
  (|ConstantVal| |LLVMValueRef|)
  (|ToType| |LLVMTypeRef|)
  (|isSigned| |LLVMBool|))
(defcfun (-const-f-p-cast "LLVMConstFPCast") |LLVMValueRef|
  (|ConstantVal| |LLVMValueRef|)
  (|ToType| |LLVMTypeRef|))
(defcfun (-const-select "LLVMConstSelect") |LLVMValueRef|
  (|ConstantCondition| |LLVMValueRef|)
  (|ConstantIfTrue| |LLVMValueRef|)
  (|ConstantIfFalse| |LLVMValueRef|))
(defcfun (-const-extract-element "LLVMConstExtractElement") |LLVMValueRef|
  (|VectorConstant| |LLVMValueRef|)
  (|IndexConstant| |LLVMValueRef|))
(defcfun (-const-insert-element "LLVMConstInsertElement") |LLVMValueRef|
  (|VectorConstant| |LLVMValueRef|)
  (|ElementValueConstant| |LLVMValueRef|)
  (|IndexConstant| |LLVMValueRef|))
(defcfun (-const-shuffle-vector "LLVMConstShuffleVector") |LLVMValueRef|
  (|VectorAConstant| |LLVMValueRef|)
  (|VectorBConstant| |LLVMValueRef|)
  (|MaskConstant| |LLVMValueRef|))
(defcfun (-const-extract-value "LLVMConstExtractValue") |LLVMValueRef|
  (|AggConstant| |LLVMValueRef|)
  (|IdxList| (:pointer :unsigned-int))
  (|NumIdx| :unsigned-int))
(defcfun (-const-insert-value "LLVMConstInsertValue") |LLVMValueRef|
  (|AggConstant| |LLVMValueRef|)
  (|ElementValueConstant| |LLVMValueRef|)
  (|IdxList| (:pointer :unsigned-int))
  (|NumIdx| :unsigned-int))
(defcfun (-const-inline-asm "LLVMConstInlineAsm") |LLVMValueRef|
  (|Ty| |LLVMTypeRef|)
  (|AsmString| (:pointer :char))
  (|Constraints| (:pointer :char))
  (|HasSideEffects| |LLVMBool|)
  (|IsAlignStack| |LLVMBool|))
(defcfun (-block-address "LLVMBlockAddress") |LLVMValueRef|
  (|f| |LLVMValueRef|)
  (|bb| |LLVMBasicBlockRef|))
(defcfun (-get-global-parent "LLVMGetGlobalParent") |LLVMModuleRef|
  (|Global| |LLVMValueRef|))
(defcfun (-is-declaration "LLVMIsDeclaration") |LLVMBool|
  (|Global| |LLVMValueRef|))
(defcfun (-get-linkage "LLVMGetLinkage") |LLVMLinkage|
  (|Global| |LLVMValueRef|))
(defcfun (-set-linkage "LLVMSetLinkage") :void
  (|Global| |LLVMValueRef|)
  (|Linkage| |LLVMLinkage|))
(defcfun (-get-section "LLVMGetSection") (:pointer :char)
  (|Global| |LLVMValueRef|))
(defcfun (-set-section "LLVMSetSection") :void
  (|Global| |LLVMValueRef|)
  (|Section| (:pointer :char)))
(defcfun (-get-visibility "LLVMGetVisibility") |LLVMVisibility|
  (|Global| |LLVMValueRef|))
(defcfun (-set-visibility "LLVMSetVisibility") :void
  (|Global| |LLVMValueRef|)
  (|Viz| |LLVMVisibility|))
(defcfun (-get-d-l-l-storage-class
          "LLVMGetDLLStorageClass") |LLVMDLLStorageClass|
  (|Global| |LLVMValueRef|))
(defcfun (-set-d-l-l-storage-class "LLVMSetDLLStorageClass") :void
  (|Global| |LLVMValueRef|)
  (|Class| |LLVMDLLStorageClass|))
(defcfun (-has-unnamed-addr "LLVMHasUnnamedAddr") |LLVMBool|
  (|Global| |LLVMValueRef|))
(defcfun (-set-unnamed-addr "LLVMSetUnnamedAddr") :void
  (|Global| |LLVMValueRef|)
  (|HasUnnamedAddr| |LLVMBool|))
(defcfun (-get-alignment "LLVMGetAlignment") :unsigned-int
  (|v| |LLVMValueRef|))
(defcfun (-set-alignment "LLVMSetAlignment") :void
  (|v| |LLVMValueRef|)
  (|Bytes| :unsigned-int))
(defcfun (-add-global "LLVMAddGlobal") |LLVMValueRef|
  (|m| |LLVMModuleRef|)
  (|Ty| |LLVMTypeRef|)
  (|Name| (:pointer :char)))
(defcfun (-add-global-in-address-space
          "LLVMAddGlobalInAddressSpace") |LLVMValueRef|
  (|m| |LLVMModuleRef|)
  (|Ty| |LLVMTypeRef|)
  (|Name| (:pointer :char))
  (|AddressSpace| :unsigned-int))
(defcfun (-get-named-global "LLVMGetNamedGlobal") |LLVMValueRef|
  (|m| |LLVMModuleRef|)
  (|Name| (:pointer :char)))
(defcfun (-get-first-global "LLVMGetFirstGlobal") |LLVMValueRef|
  (|m| |LLVMModuleRef|))
(defcfun (-get-last-global "LLVMGetLastGlobal") |LLVMValueRef|
  (|m| |LLVMModuleRef|))
(defcfun (-get-next-global "LLVMGetNextGlobal") |LLVMValueRef|
  (|GlobalVar| |LLVMValueRef|))
(defcfun (-get-previous-global "LLVMGetPreviousGlobal") |LLVMValueRef|
  (|GlobalVar| |LLVMValueRef|))
(defcfun (-delete-global "LLVMDeleteGlobal") :void
  (|GlobalVar| |LLVMValueRef|))
(defcfun (-get-initializer "LLVMGetInitializer") |LLVMValueRef|
  (|GlobalVar| |LLVMValueRef|))
(defcfun (-set-initializer "LLVMSetInitializer") :void
  (|GlobalVar| |LLVMValueRef|)
  (|ConstantVal| |LLVMValueRef|))
(defcfun (-is-thread-local "LLVMIsThreadLocal") |LLVMBool|
  (|GlobalVar| |LLVMValueRef|))
(defcfun (-set-thread-local "LLVMSetThreadLocal") :void
  (|GlobalVar| |LLVMValueRef|)
  (|IsThreadLocal| |LLVMBool|))
(defcfun (-is-global-constant "LLVMIsGlobalConstant") |LLVMBool|
  (|GlobalVar| |LLVMValueRef|))
(defcfun (-set-global-constant "LLVMSetGlobalConstant") :void
  (|GlobalVar| |LLVMValueRef|)
  (|IsConstant| |LLVMBool|))
(defcfun (-get-thread-local-mode
          "LLVMGetThreadLocalMode") |LLVMThreadLocalMode|
  (|GlobalVar| |LLVMValueRef|))
(defcfun (-set-thread-local-mode "LLVMSetThreadLocalMode") :void
  (|GlobalVar| |LLVMValueRef|)
  (|Mode| |LLVMThreadLocalMode|))
(defcfun (-is-externally-initialized
          "LLVMIsExternallyInitialized") |LLVMBool|
  (|GlobalVar| |LLVMValueRef|))
(defcfun (-set-externally-initialized "LLVMSetExternallyInitialized") :void
  (|GlobalVar| |LLVMValueRef|)
  (|IsExtInit| |LLVMBool|))
(defcfun (-add-alias "LLVMAddAlias") |LLVMValueRef|
  (|m| |LLVMModuleRef|)
  (|Ty| |LLVMTypeRef|)
  (|Aliasee| |LLVMValueRef|)
  (|Name| (:pointer :char)))
(defcfun (-delete-function "LLVMDeleteFunction") :void
  (|Fn| |LLVMValueRef|))
(defcfun (-get-personality-fn "LLVMGetPersonalityFn") |LLVMValueRef|
  (|Fn| |LLVMValueRef|))
(defcfun (-set-personality-fn "LLVMSetPersonalityFn") :void
  (|Fn| |LLVMValueRef|)
  (|PersonalityFn| |LLVMValueRef|))
(defcfun (-get-intrinsic-i-d "LLVMGetIntrinsicID") :unsigned-int
  (|Fn| |LLVMValueRef|))
(defcfun (-get-function-call-conv "LLVMGetFunctionCallConv") :unsigned-int
  (|Fn| |LLVMValueRef|))
(defcfun (-set-function-call-conv "LLVMSetFunctionCallConv") :void
  (|Fn| |LLVMValueRef|)
  (|cc| :unsigned-int))
(defcfun (-get-g-c "LLVMGetGC") (:pointer :char)
  (|Fn| |LLVMValueRef|))
(defcfun (-set-g-c "LLVMSetGC") :void
  (|Fn| |LLVMValueRef|)
  (|Name| (:pointer :char)))
(defcfun (-add-function-attr "LLVMAddFunctionAttr") :void
  (|Fn| |LLVMValueRef|)
  (|pa| |LLVMAttribute|))
(defcfun (-add-target-dependent-function-attr
          "LLVMAddTargetDependentFunctionAttr") :void
  (|Fn| |LLVMValueRef|)
  (|a| (:pointer :char))
  (|v| (:pointer :char)))
(defcfun (-get-function-attr "LLVMGetFunctionAttr") |LLVMAttribute|
  (|Fn| |LLVMValueRef|))
(defcfun (-remove-function-attr "LLVMRemoveFunctionAttr") :void
  (|Fn| |LLVMValueRef|)
  (|pa| |LLVMAttribute|))
(defcfun (-count-params "LLVMCountParams") :unsigned-int
  (|Fn| |LLVMValueRef|))
(defcfun (-get-params "LLVMGetParams") :void
  (|Fn| |LLVMValueRef|)
  (|Params| (:pointer |LLVMValueRef|)))
(defcfun (-get-param "LLVMGetParam") |LLVMValueRef|
  (|Fn| |LLVMValueRef|)
  (|Index| :unsigned-int))
(defcfun (-get-param-parent "LLVMGetParamParent") |LLVMValueRef|
  (|Inst| |LLVMValueRef|))
(defcfun (-get-first-param "LLVMGetFirstParam") |LLVMValueRef|
  (|Fn| |LLVMValueRef|))
(defcfun (-get-last-param "LLVMGetLastParam") |LLVMValueRef|
  (|Fn| |LLVMValueRef|))
(defcfun (-get-next-param "LLVMGetNextParam") |LLVMValueRef|
  (|Arg| |LLVMValueRef|))
(defcfun (-get-previous-param "LLVMGetPreviousParam") |LLVMValueRef|
  (|Arg| |LLVMValueRef|))
(defcfun (-add-attribute "LLVMAddAttribute") :void
  (|Arg| |LLVMValueRef|)
  (|pa| |LLVMAttribute|))
(defcfun (-remove-attribute "LLVMRemoveAttribute") :void
  (|Arg| |LLVMValueRef|)
  (|pa| |LLVMAttribute|))
(defcfun (-get-attribute "LLVMGetAttribute") |LLVMAttribute|
  (|Arg| |LLVMValueRef|))
(defcfun (-set-param-alignment "LLVMSetParamAlignment") :void
  (|Arg| |LLVMValueRef|)
  (align :unsigned-int))
(defcfun (-m-d-string-in-context "LLVMMDStringInContext") |LLVMValueRef|
  (|c| |LLVMContextRef|)
  (|Str| (:pointer :char))
  (|SLen| :unsigned-int))
(defcfun (-m-d-string "LLVMMDString") |LLVMValueRef|
  (|Str| (:pointer :char))
  (|SLen| :unsigned-int))
(defcfun (-m-d-node-in-context "LLVMMDNodeInContext") |LLVMValueRef|
  (|c| |LLVMContextRef|)
  (|Vals| (:pointer |LLVMValueRef|))
  (|Count| :unsigned-int))
(defcfun (-m-d-node "LLVMMDNode") |LLVMValueRef|
  (|Vals| (:pointer |LLVMValueRef|))
  (|Count| :unsigned-int))
(defcfun (-get-m-d-string "LLVMGetMDString") (:pointer :char)
  (|v| |LLVMValueRef|)
  (|Len| (:pointer :unsigned-int)))
(defcfun (-get-m-d-node-num-operands
          "LLVMGetMDNodeNumOperands") :unsigned-int
  (|v| |LLVMValueRef|))
(defcfun (-get-m-d-node-operands "LLVMGetMDNodeOperands") :void
  (|v| |LLVMValueRef|)
  (|Dest| (:pointer |LLVMValueRef|)))
(defcfun (-basic-block-as-value "LLVMBasicBlockAsValue") |LLVMValueRef|
  (|bb| |LLVMBasicBlockRef|))
(defcfun (-value-is-basic-block "LLVMValueIsBasicBlock") |LLVMBool|
  (|Val| |LLVMValueRef|))
(defcfun (-value-as-basic-block
          "LLVMValueAsBasicBlock") |LLVMBasicBlockRef|
  (|Val| |LLVMValueRef|))
(defcfun (-get-basic-block-parent "LLVMGetBasicBlockParent") |LLVMValueRef|
  (|bb| |LLVMBasicBlockRef|))
(defcfun (-get-basic-block-terminator
          "LLVMGetBasicBlockTerminator") |LLVMValueRef|
  (|bb| |LLVMBasicBlockRef|))
(defcfun (-count-basic-blocks "LLVMCountBasicBlocks") :unsigned-int
  (|Fn| |LLVMValueRef|))
(defcfun (-get-basic-blocks "LLVMGetBasicBlocks") :void
  (|Fn| |LLVMValueRef|)
  (|BasicBlocks| (:pointer |LLVMBasicBlockRef|)))
(defcfun (-get-first-basic-block
          "LLVMGetFirstBasicBlock") |LLVMBasicBlockRef|
  (|Fn| |LLVMValueRef|))
(defcfun (-get-last-basic-block
          "LLVMGetLastBasicBlock") |LLVMBasicBlockRef|
  (|Fn| |LLVMValueRef|))
(defcfun (-get-next-basic-block
          "LLVMGetNextBasicBlock") |LLVMBasicBlockRef|
  (|bb| |LLVMBasicBlockRef|))
(defcfun (-get-previous-basic-block
          "LLVMGetPreviousBasicBlock") |LLVMBasicBlockRef|
  (|bb| |LLVMBasicBlockRef|))
(defcfun (-get-entry-basic-block
          "LLVMGetEntryBasicBlock") |LLVMBasicBlockRef|
  (|Fn| |LLVMValueRef|))
(defcfun (-append-basic-block-in-context
          "LLVMAppendBasicBlockInContext") |LLVMBasicBlockRef|
  (|c| |LLVMContextRef|)
  (|Fn| |LLVMValueRef|)
  (|Name| (:pointer :char)))
(defcfun (-append-basic-block "LLVMAppendBasicBlock") |LLVMBasicBlockRef|
  (|Fn| |LLVMValueRef|)
  (|Name| (:pointer :char)))
(defcfun (-insert-basic-block-in-context
          "LLVMInsertBasicBlockInContext") |LLVMBasicBlockRef|
  (|c| |LLVMContextRef|)
  (|bb| |LLVMBasicBlockRef|)
  (|Name| (:pointer :char)))
(defcfun (-insert-basic-block "LLVMInsertBasicBlock") |LLVMBasicBlockRef|
  (|InsertBeforeBB| |LLVMBasicBlockRef|)
  (|Name| (:pointer :char)))
(defcfun (-delete-basic-block "LLVMDeleteBasicBlock") :void
  (|bb| |LLVMBasicBlockRef|))
(defcfun (-remove-basic-block-from-parent
          "LLVMRemoveBasicBlockFromParent") :void
  (|bb| |LLVMBasicBlockRef|))
(defcfun (-move-basic-block-before "LLVMMoveBasicBlockBefore") :void
  (|bb| |LLVMBasicBlockRef|)
  (|MovePos| |LLVMBasicBlockRef|))
(defcfun (-move-basic-block-after "LLVMMoveBasicBlockAfter") :void
  (|bb| |LLVMBasicBlockRef|)
  (|MovePos| |LLVMBasicBlockRef|))
(defcfun (-get-first-instruction "LLVMGetFirstInstruction") |LLVMValueRef|
  (|bb| |LLVMBasicBlockRef|))
(defcfun (-get-last-instruction "LLVMGetLastInstruction") |LLVMValueRef|
  (|bb| |LLVMBasicBlockRef|))
(defcfun (-has-metadata "LLVMHasMetadata") :int
  (|Val| |LLVMValueRef|))
(defcfun (-get-metadata "LLVMGetMetadata") |LLVMValueRef|
  (|Val| |LLVMValueRef|)
  (|KindID| :unsigned-int))
(defcfun (-set-metadata "LLVMSetMetadata") :void
  (|Val| |LLVMValueRef|)
  (|KindID| :unsigned-int)
  (|Node| |LLVMValueRef|))
(defcfun (-get-instruction-parent
          "LLVMGetInstructionParent") |LLVMBasicBlockRef|
  (|Inst| |LLVMValueRef|))
(defcfun (-get-next-instruction "LLVMGetNextInstruction") |LLVMValueRef|
  (|Inst| |LLVMValueRef|))
(defcfun (-get-previous-instruction
          "LLVMGetPreviousInstruction") |LLVMValueRef|
  (|Inst| |LLVMValueRef|))
(defcfun (-instruction-erase-from-parent
          "LLVMInstructionEraseFromParent") :void
  (|Inst| |LLVMValueRef|))
(defcfun (-get-instruction-opcode "LLVMGetInstructionOpcode") |LLVMOpcode|
  (|Inst| |LLVMValueRef|))
(defcfun (-get-i-cmp-predicate "LLVMGetICmpPredicate") |LLVMIntPredicate|
  (|Inst| |LLVMValueRef|))
(defcfun (-get-f-cmp-predicate "LLVMGetFCmpPredicate") |LLVMRealPredicate|
  (|Inst| |LLVMValueRef|))
(defcfun (-instruction-clone "LLVMInstructionClone") |LLVMValueRef|
  (|Inst| |LLVMValueRef|))
(defcfun (-set-instruction-call-conv "LLVMSetInstructionCallConv") :void
  (|Instr| |LLVMValueRef|)
  (|cc| :unsigned-int))
(defcfun (-get-instruction-call-conv
          "LLVMGetInstructionCallConv") :unsigned-int
  (|Instr| |LLVMValueRef|))
(defcfun (-add-instr-attribute "LLVMAddInstrAttribute") :void
  (|Instr| |LLVMValueRef|)
  (index :unsigned-int)
  (|LLVMAttribute| |LLVMAttribute|))
(defcfun (-remove-instr-attribute "LLVMRemoveInstrAttribute") :void
  (|Instr| |LLVMValueRef|)
  (index :unsigned-int)
  (|LLVMAttribute| |LLVMAttribute|))
(defcfun (-set-instr-param-alignment "LLVMSetInstrParamAlignment") :void
  (|Instr| |LLVMValueRef|)
  (index :unsigned-int)
  (align :unsigned-int))
(defcfun (-is-tail-call "LLVMIsTailCall") |LLVMBool|
  (|CallInst| |LLVMValueRef|))
(defcfun (-set-tail-call "LLVMSetTailCall") :void
  (|CallInst| |LLVMValueRef|)
  (|IsTailCall| |LLVMBool|))
(defcfun (-get-num-successors "LLVMGetNumSuccessors") :unsigned-int
  (|Term| |LLVMValueRef|))
(defcfun (-get-successor "LLVMGetSuccessor") |LLVMBasicBlockRef|
  (|Term| |LLVMValueRef|)
  (i :unsigned-int))
(defcfun (-set-successor "LLVMSetSuccessor") :void
  (|Term| |LLVMValueRef|)
  (i :unsigned-int)
  (block |LLVMBasicBlockRef|))
(defcfun (-is-conditional "LLVMIsConditional") |LLVMBool|
  (|Branch| |LLVMValueRef|))
(defcfun (-get-condition "LLVMGetCondition") |LLVMValueRef|
  (|Branch| |LLVMValueRef|))
(defcfun (-set-condition "LLVMSetCondition") :void
  (|Branch| |LLVMValueRef|)
  (|Cond| |LLVMValueRef|))
(defcfun (-get-switch-default-dest
          "LLVMGetSwitchDefaultDest") |LLVMBasicBlockRef|
  (|SwitchInstr| |LLVMValueRef|))
(defcfun (-add-incoming "LLVMAddIncoming") :void
  (|PhiNode| |LLVMValueRef|)
  (|IncomingValues| (:pointer |LLVMValueRef|))
  (|IncomingBlocks| (:pointer |LLVMBasicBlockRef|))
  (|Count| :unsigned-int))
(defcfun (-count-incoming "LLVMCountIncoming") :unsigned-int
  (|PhiNode| |LLVMValueRef|))
(defcfun (-get-incoming-value "LLVMGetIncomingValue") |LLVMValueRef|
  (|PhiNode| |LLVMValueRef|)
  (|Index| :unsigned-int))
(defcfun (-get-incoming-block "LLVMGetIncomingBlock") |LLVMBasicBlockRef|
  (|PhiNode| |LLVMValueRef|)
  (|Index| :unsigned-int))
(defcfun (-create-builder-in-context
          "LLVMCreateBuilderInContext") |LLVMBuilderRef|
  (|c| |LLVMContextRef|))
(defcfun (-create-builder "LLVMCreateBuilder") |LLVMBuilderRef|)
(defcfun (-position-builder "LLVMPositionBuilder") :void
  (|Builder| |LLVMBuilderRef|)
  (|Block| |LLVMBasicBlockRef|)
  (|Instr| |LLVMValueRef|))
(defcfun (-position-builder-before "LLVMPositionBuilderBefore") :void
  (|Builder| |LLVMBuilderRef|)
  (|Instr| |LLVMValueRef|))
(defcfun (-position-builder-at-end "LLVMPositionBuilderAtEnd") :void
  (|Builder| |LLVMBuilderRef|)
  (|Block| |LLVMBasicBlockRef|))
(defcfun (-get-insert-block "LLVMGetInsertBlock") |LLVMBasicBlockRef|
  (|Builder| |LLVMBuilderRef|))
(defcfun (-clear-insertion-position "LLVMClearInsertionPosition") :void
  (|Builder| |LLVMBuilderRef|))
(defcfun (-insert-into-builder "LLVMInsertIntoBuilder") :void
  (|Builder| |LLVMBuilderRef|)
  (|Instr| |LLVMValueRef|))
(defcfun (-insert-into-builder-with-name
          "LLVMInsertIntoBuilderWithName") :void
  (|Builder| |LLVMBuilderRef|)
  (|Instr| |LLVMValueRef|)
  (|Name| (:pointer :char)))
(defcfun (-dispose-builder "LLVMDisposeBuilder") :void
  (|Builder| |LLVMBuilderRef|))
(defcfun (-set-current-debug-location "LLVMSetCurrentDebugLocation") :void
  (|Builder| |LLVMBuilderRef|)
  (|l| |LLVMValueRef|))
(defcfun (-get-current-debug-location
          "LLVMGetCurrentDebugLocation") |LLVMValueRef|
  (|Builder| |LLVMBuilderRef|))
(defcfun (-set-inst-debug-location "LLVMSetInstDebugLocation") :void
  (|Builder| |LLVMBuilderRef|)
  (|Inst| |LLVMValueRef|))
(defcfun (-build-ret-void "LLVMBuildRetVoid") |LLVMValueRef|
  (|LLVMBuilderRef| |LLVMBuilderRef|))
(defcfun (-build-ret "LLVMBuildRet") |LLVMValueRef|
  (|LLVMBuilderRef| |LLVMBuilderRef|)
  (|v| |LLVMValueRef|))
(defcfun (-build-aggregate-ret "LLVMBuildAggregateRet") |LLVMValueRef|
  (|LLVMBuilderRef| |LLVMBuilderRef|)
  (|RetVals| (:pointer |LLVMValueRef|))
  (|n| :unsigned-int))
(defcfun (-build-br "LLVMBuildBr") |LLVMValueRef|
  (|LLVMBuilderRef| |LLVMBuilderRef|)
  (|Dest| |LLVMBasicBlockRef|))
(defcfun (-build-cond-br "LLVMBuildCondBr") |LLVMValueRef|
  (|LLVMBuilderRef| |LLVMBuilderRef|)
  (|If| |LLVMValueRef|)
  (|Then| |LLVMBasicBlockRef|)
  (|Else| |LLVMBasicBlockRef|))
(defcfun (-build-switch "LLVMBuildSwitch") |LLVMValueRef|
  (|LLVMBuilderRef| |LLVMBuilderRef|)
  (|v| |LLVMValueRef|)
  (|Else| |LLVMBasicBlockRef|)
  (|NumCases| :unsigned-int))
(defcfun (-build-indirect-br "LLVMBuildIndirectBr") |LLVMValueRef|
  (|b| |LLVMBuilderRef|)
  (|Addr| |LLVMValueRef|)
  (|NumDests| :unsigned-int))
(defcfun (-build-invoke "LLVMBuildInvoke") |LLVMValueRef|
  (|LLVMBuilderRef| |LLVMBuilderRef|)
  (|Fn| |LLVMValueRef|)
  (|Args| (:pointer |LLVMValueRef|))
  (|NumArgs| :unsigned-int)
  (|Then| |LLVMBasicBlockRef|)
  (|Catch| |LLVMBasicBlockRef|)
  (|Name| (:pointer :char)))
(defcfun (-build-landing-pad "LLVMBuildLandingPad") |LLVMValueRef|
  (|b| |LLVMBuilderRef|)
  (|Ty| |LLVMTypeRef|)
  (|PersFn| |LLVMValueRef|)
  (|NumClauses| :unsigned-int)
  (|Name| (:pointer :char)))
(defcfun (-build-resume "LLVMBuildResume") |LLVMValueRef|
  (|b| |LLVMBuilderRef|)
  (|Exn| |LLVMValueRef|))
(defcfun (-build-unreachable "LLVMBuildUnreachable") |LLVMValueRef|
  (|LLVMBuilderRef| |LLVMBuilderRef|))
(defcfun (-add-case "LLVMAddCase") :void
  (|Switch| |LLVMValueRef|)
  (|OnVal| |LLVMValueRef|)
  (|Dest| |LLVMBasicBlockRef|))
(defcfun (-add-destination "LLVMAddDestination") :void
  (|IndirectBr| |LLVMValueRef|)
  (|Dest| |LLVMBasicBlockRef|))
(defcfun (-add-clause "LLVMAddClause") :void
  (|LandingPad| |LLVMValueRef|)
  (|ClauseVal| |LLVMValueRef|))
(defcfun (-set-cleanup "LLVMSetCleanup") :void
  (|LandingPad| |LLVMValueRef|)
  (|Val| |LLVMBool|))
(defcfun (-build-add "LLVMBuildAdd") |LLVMValueRef|
  (|LLVMBuilderRef| |LLVMBuilderRef|)
  (|lhs| |LLVMValueRef|)
  (|rhs| |LLVMValueRef|)
  (|Name| (:pointer :char)))
(defcfun (-build-n-s-w-add "LLVMBuildNSWAdd") |LLVMValueRef|
  (|LLVMBuilderRef| |LLVMBuilderRef|)
  (|lhs| |LLVMValueRef|)
  (|rhs| |LLVMValueRef|)
  (|Name| (:pointer :char)))
(defcfun (-build-n-u-w-add "LLVMBuildNUWAdd") |LLVMValueRef|
  (|LLVMBuilderRef| |LLVMBuilderRef|)
  (|lhs| |LLVMValueRef|)
  (|rhs| |LLVMValueRef|)
  (|Name| (:pointer :char)))
(defcfun (-build-f-add "LLVMBuildFAdd") |LLVMValueRef|
  (|LLVMBuilderRef| |LLVMBuilderRef|)
  (|lhs| |LLVMValueRef|)
  (|rhs| |LLVMValueRef|)
  (|Name| (:pointer :char)))
(defcfun (-build-sub "LLVMBuildSub") |LLVMValueRef|
  (|LLVMBuilderRef| |LLVMBuilderRef|)
  (|lhs| |LLVMValueRef|)
  (|rhs| |LLVMValueRef|)
  (|Name| (:pointer :char)))
(defcfun (-build-n-s-w-sub "LLVMBuildNSWSub") |LLVMValueRef|
  (|LLVMBuilderRef| |LLVMBuilderRef|)
  (|lhs| |LLVMValueRef|)
  (|rhs| |LLVMValueRef|)
  (|Name| (:pointer :char)))
(defcfun (-build-n-u-w-sub "LLVMBuildNUWSub") |LLVMValueRef|
  (|LLVMBuilderRef| |LLVMBuilderRef|)
  (|lhs| |LLVMValueRef|)
  (|rhs| |LLVMValueRef|)
  (|Name| (:pointer :char)))
(defcfun (-build-f-sub "LLVMBuildFSub") |LLVMValueRef|
  (|LLVMBuilderRef| |LLVMBuilderRef|)
  (|lhs| |LLVMValueRef|)
  (|rhs| |LLVMValueRef|)
  (|Name| (:pointer :char)))
(defcfun (-build-mul "LLVMBuildMul") |LLVMValueRef|
  (|LLVMBuilderRef| |LLVMBuilderRef|)
  (|lhs| |LLVMValueRef|)
  (|rhs| |LLVMValueRef|)
  (|Name| (:pointer :char)))
(defcfun (-build-n-s-w-mul "LLVMBuildNSWMul") |LLVMValueRef|
  (|LLVMBuilderRef| |LLVMBuilderRef|)
  (|lhs| |LLVMValueRef|)
  (|rhs| |LLVMValueRef|)
  (|Name| (:pointer :char)))
(defcfun (-build-n-u-w-mul "LLVMBuildNUWMul") |LLVMValueRef|
  (|LLVMBuilderRef| |LLVMBuilderRef|)
  (|lhs| |LLVMValueRef|)
  (|rhs| |LLVMValueRef|)
  (|Name| (:pointer :char)))
(defcfun (-build-f-mul "LLVMBuildFMul") |LLVMValueRef|
  (|LLVMBuilderRef| |LLVMBuilderRef|)
  (|lhs| |LLVMValueRef|)
  (|rhs| |LLVMValueRef|)
  (|Name| (:pointer :char)))
(defcfun (-build-u-div "LLVMBuildUDiv") |LLVMValueRef|
  (|LLVMBuilderRef| |LLVMBuilderRef|)
  (|lhs| |LLVMValueRef|)
  (|rhs| |LLVMValueRef|)
  (|Name| (:pointer :char)))
(defcfun (-build-s-div "LLVMBuildSDiv") |LLVMValueRef|
  (|LLVMBuilderRef| |LLVMBuilderRef|)
  (|lhs| |LLVMValueRef|)
  (|rhs| |LLVMValueRef|)
  (|Name| (:pointer :char)))
(defcfun (-build-exact-s-div "LLVMBuildExactSDiv") |LLVMValueRef|
  (|LLVMBuilderRef| |LLVMBuilderRef|)
  (|lhs| |LLVMValueRef|)
  (|rhs| |LLVMValueRef|)
  (|Name| (:pointer :char)))
(defcfun (-build-f-div "LLVMBuildFDiv") |LLVMValueRef|
  (|LLVMBuilderRef| |LLVMBuilderRef|)
  (|lhs| |LLVMValueRef|)
  (|rhs| |LLVMValueRef|)
  (|Name| (:pointer :char)))
(defcfun (-build-u-rem "LLVMBuildURem") |LLVMValueRef|
  (|LLVMBuilderRef| |LLVMBuilderRef|)
  (|lhs| |LLVMValueRef|)
  (|rhs| |LLVMValueRef|)
  (|Name| (:pointer :char)))
(defcfun (-build-s-rem "LLVMBuildSRem") |LLVMValueRef|
  (|LLVMBuilderRef| |LLVMBuilderRef|)
  (|lhs| |LLVMValueRef|)
  (|rhs| |LLVMValueRef|)
  (|Name| (:pointer :char)))
(defcfun (-build-f-rem "LLVMBuildFRem") |LLVMValueRef|
  (|LLVMBuilderRef| |LLVMBuilderRef|)
  (|lhs| |LLVMValueRef|)
  (|rhs| |LLVMValueRef|)
  (|Name| (:pointer :char)))
(defcfun (-build-shl "LLVMBuildShl") |LLVMValueRef|
  (|LLVMBuilderRef| |LLVMBuilderRef|)
  (|lhs| |LLVMValueRef|)
  (|rhs| |LLVMValueRef|)
  (|Name| (:pointer :char)))
(defcfun (-build-l-shr "LLVMBuildLShr") |LLVMValueRef|
  (|LLVMBuilderRef| |LLVMBuilderRef|)
  (|lhs| |LLVMValueRef|)
  (|rhs| |LLVMValueRef|)
  (|Name| (:pointer :char)))
(defcfun (-build-a-shr "LLVMBuildAShr") |LLVMValueRef|
  (|LLVMBuilderRef| |LLVMBuilderRef|)
  (|lhs| |LLVMValueRef|)
  (|rhs| |LLVMValueRef|)
  (|Name| (:pointer :char)))
(defcfun (-build-and "LLVMBuildAnd") |LLVMValueRef|
  (|LLVMBuilderRef| |LLVMBuilderRef|)
  (|lhs| |LLVMValueRef|)
  (|rhs| |LLVMValueRef|)
  (|Name| (:pointer :char)))
(defcfun (-build-or "LLVMBuildOr") |LLVMValueRef|
  (|LLVMBuilderRef| |LLVMBuilderRef|)
  (|lhs| |LLVMValueRef|)
  (|rhs| |LLVMValueRef|)
  (|Name| (:pointer :char)))
(defcfun (-build-xor "LLVMBuildXor") |LLVMValueRef|
  (|LLVMBuilderRef| |LLVMBuilderRef|)
  (|lhs| |LLVMValueRef|)
  (|rhs| |LLVMValueRef|)
  (|Name| (:pointer :char)))
(defcfun (-build-bin-op "LLVMBuildBinOp") |LLVMValueRef|
  (|b| |LLVMBuilderRef|)
  (|Op| |LLVMOpcode|)
  (|lhs| |LLVMValueRef|)
  (|rhs| |LLVMValueRef|)
  (|Name| (:pointer :char)))
(defcfun (-build-neg "LLVMBuildNeg") |LLVMValueRef|
  (|LLVMBuilderRef| |LLVMBuilderRef|)
  (|v| |LLVMValueRef|)
  (|Name| (:pointer :char)))
(defcfun (-build-n-s-w-neg "LLVMBuildNSWNeg") |LLVMValueRef|
  (|b| |LLVMBuilderRef|)
  (|v| |LLVMValueRef|)
  (|Name| (:pointer :char)))
(defcfun (-build-n-u-w-neg "LLVMBuildNUWNeg") |LLVMValueRef|
  (|b| |LLVMBuilderRef|)
  (|v| |LLVMValueRef|)
  (|Name| (:pointer :char)))
(defcfun (-build-f-neg "LLVMBuildFNeg") |LLVMValueRef|
  (|LLVMBuilderRef| |LLVMBuilderRef|)
  (|v| |LLVMValueRef|)
  (|Name| (:pointer :char)))
(defcfun (-build-not "LLVMBuildNot") |LLVMValueRef|
  (|LLVMBuilderRef| |LLVMBuilderRef|)
  (|v| |LLVMValueRef|)
  (|Name| (:pointer :char)))
(defcfun (-build-malloc "LLVMBuildMalloc") |LLVMValueRef|
  (|LLVMBuilderRef| |LLVMBuilderRef|)
  (|Ty| |LLVMTypeRef|)
  (|Name| (:pointer :char)))
(defcfun (-build-array-malloc "LLVMBuildArrayMalloc") |LLVMValueRef|
  (|LLVMBuilderRef| |LLVMBuilderRef|)
  (|Ty| |LLVMTypeRef|)
  (|Val| |LLVMValueRef|)
  (|Name| (:pointer :char)))
(defcfun (-build-alloca "LLVMBuildAlloca") |LLVMValueRef|
  (|LLVMBuilderRef| |LLVMBuilderRef|)
  (|Ty| |LLVMTypeRef|)
  (|Name| (:pointer :char)))
(defcfun (-build-array-alloca "LLVMBuildArrayAlloca") |LLVMValueRef|
  (|LLVMBuilderRef| |LLVMBuilderRef|)
  (|Ty| |LLVMTypeRef|)
  (|Val| |LLVMValueRef|)
  (|Name| (:pointer :char)))
(defcfun (-build-free "LLVMBuildFree") |LLVMValueRef|
  (|LLVMBuilderRef| |LLVMBuilderRef|)
  (|PointerVal| |LLVMValueRef|))
(defcfun (-build-load "LLVMBuildLoad") |LLVMValueRef|
  (|LLVMBuilderRef| |LLVMBuilderRef|)
  (|PointerVal| |LLVMValueRef|)
  (|Name| (:pointer :char)))
(defcfun (-build-store "LLVMBuildStore") |LLVMValueRef|
  (|LLVMBuilderRef| |LLVMBuilderRef|)
  (|Val| |LLVMValueRef|)
  (|Ptr| |LLVMValueRef|))
(defcfun (-build-g-e-p "LLVMBuildGEP") |LLVMValueRef|
  (|b| |LLVMBuilderRef|)
  (|Pointer| |LLVMValueRef|)
  (|Indices| (:pointer |LLVMValueRef|))
  (|NumIndices| :unsigned-int)
  (|Name| (:pointer :char)))
(defcfun (-build-in-bounds-g-e-p "LLVMBuildInBoundsGEP") |LLVMValueRef|
  (|b| |LLVMBuilderRef|)
  (|Pointer| |LLVMValueRef|)
  (|Indices| (:pointer |LLVMValueRef|))
  (|NumIndices| :unsigned-int)
  (|Name| (:pointer :char)))
(defcfun (-build-struct-g-e-p "LLVMBuildStructGEP") |LLVMValueRef|
  (|b| |LLVMBuilderRef|)
  (|Pointer| |LLVMValueRef|)
  (|Idx| :unsigned-int)
  (|Name| (:pointer :char)))
(defcfun (-build-global-string "LLVMBuildGlobalString") |LLVMValueRef|
  (|b| |LLVMBuilderRef|)
  (|Str| (:pointer :char))
  (|Name| (:pointer :char)))
(defcfun (-build-global-string-ptr
          "LLVMBuildGlobalStringPtr") |LLVMValueRef|
  (|b| |LLVMBuilderRef|)
  (|Str| (:pointer :char))
  (|Name| (:pointer :char)))
(defcfun (-get-volatile "LLVMGetVolatile") |LLVMBool|
  (|MemoryAccessInst| |LLVMValueRef|))
(defcfun (-set-volatile "LLVMSetVolatile") :void
  (|MemoryAccessInst| |LLVMValueRef|)
  (|IsVolatile| |LLVMBool|))
(defcfun (-get-ordering "LLVMGetOrdering") |LLVMAtomicOrdering|
  (|MemoryAccessInst| |LLVMValueRef|))
(defcfun (-set-ordering "LLVMSetOrdering") :void
  (|MemoryAccessInst| |LLVMValueRef|)
  (|Ordering| |LLVMAtomicOrdering|))
(defcfun (-build-trunc "LLVMBuildTrunc") |LLVMValueRef|
  (|LLVMBuilderRef| |LLVMBuilderRef|)
  (|Val| |LLVMValueRef|)
  (|DestTy| |LLVMTypeRef|)
  (|Name| (:pointer :char)))
(defcfun (-build-z-ext "LLVMBuildZExt") |LLVMValueRef|
  (|LLVMBuilderRef| |LLVMBuilderRef|)
  (|Val| |LLVMValueRef|)
  (|DestTy| |LLVMTypeRef|)
  (|Name| (:pointer :char)))
(defcfun (-build-s-ext "LLVMBuildSExt") |LLVMValueRef|
  (|LLVMBuilderRef| |LLVMBuilderRef|)
  (|Val| |LLVMValueRef|)
  (|DestTy| |LLVMTypeRef|)
  (|Name| (:pointer :char)))
(defcfun (-build-f-p-to-u-i "LLVMBuildFPToUI") |LLVMValueRef|
  (|LLVMBuilderRef| |LLVMBuilderRef|)
  (|Val| |LLVMValueRef|)
  (|DestTy| |LLVMTypeRef|)
  (|Name| (:pointer :char)))
(defcfun (-build-f-p-to-s-i "LLVMBuildFPToSI") |LLVMValueRef|
  (|LLVMBuilderRef| |LLVMBuilderRef|)
  (|Val| |LLVMValueRef|)
  (|DestTy| |LLVMTypeRef|)
  (|Name| (:pointer :char)))
(defcfun (-build-u-i-to-f-p "LLVMBuildUIToFP") |LLVMValueRef|
  (|LLVMBuilderRef| |LLVMBuilderRef|)
  (|Val| |LLVMValueRef|)
  (|DestTy| |LLVMTypeRef|)
  (|Name| (:pointer :char)))
(defcfun (-build-s-i-to-f-p "LLVMBuildSIToFP") |LLVMValueRef|
  (|LLVMBuilderRef| |LLVMBuilderRef|)
  (|Val| |LLVMValueRef|)
  (|DestTy| |LLVMTypeRef|)
  (|Name| (:pointer :char)))
(defcfun (-build-f-p-trunc "LLVMBuildFPTrunc") |LLVMValueRef|
  (|LLVMBuilderRef| |LLVMBuilderRef|)
  (|Val| |LLVMValueRef|)
  (|DestTy| |LLVMTypeRef|)
  (|Name| (:pointer :char)))
(defcfun (-build-f-p-ext "LLVMBuildFPExt") |LLVMValueRef|
  (|LLVMBuilderRef| |LLVMBuilderRef|)
  (|Val| |LLVMValueRef|)
  (|DestTy| |LLVMTypeRef|)
  (|Name| (:pointer :char)))
(defcfun (-build-ptr-to-int "LLVMBuildPtrToInt") |LLVMValueRef|
  (|LLVMBuilderRef| |LLVMBuilderRef|)
  (|Val| |LLVMValueRef|)
  (|DestTy| |LLVMTypeRef|)
  (|Name| (:pointer :char)))
(defcfun (-build-int-to-ptr "LLVMBuildIntToPtr") |LLVMValueRef|
  (|LLVMBuilderRef| |LLVMBuilderRef|)
  (|Val| |LLVMValueRef|)
  (|DestTy| |LLVMTypeRef|)
  (|Name| (:pointer :char)))
(defcfun (-build-bit-cast "LLVMBuildBitCast") |LLVMValueRef|
  (|LLVMBuilderRef| |LLVMBuilderRef|)
  (|Val| |LLVMValueRef|)
  (|DestTy| |LLVMTypeRef|)
  (|Name| (:pointer :char)))
(defcfun (-build-addr-space-cast "LLVMBuildAddrSpaceCast") |LLVMValueRef|
  (|LLVMBuilderRef| |LLVMBuilderRef|)
  (|Val| |LLVMValueRef|)
  (|DestTy| |LLVMTypeRef|)
  (|Name| (:pointer :char)))
(defcfun (-build-z-ext-or-bit-cast "LLVMBuildZExtOrBitCast") |LLVMValueRef|
  (|LLVMBuilderRef| |LLVMBuilderRef|)
  (|Val| |LLVMValueRef|)
  (|DestTy| |LLVMTypeRef|)
  (|Name| (:pointer :char)))
(defcfun (-build-s-ext-or-bit-cast "LLVMBuildSExtOrBitCast") |LLVMValueRef|
  (|LLVMBuilderRef| |LLVMBuilderRef|)
  (|Val| |LLVMValueRef|)
  (|DestTy| |LLVMTypeRef|)
  (|Name| (:pointer :char)))
(defcfun (-build-trunc-or-bit-cast
          "LLVMBuildTruncOrBitCast") |LLVMValueRef|
  (|LLVMBuilderRef| |LLVMBuilderRef|)
  (|Val| |LLVMValueRef|)
  (|DestTy| |LLVMTypeRef|)
  (|Name| (:pointer :char)))
(defcfun (-build-cast "LLVMBuildCast") |LLVMValueRef|
  (|b| |LLVMBuilderRef|)
  (|Op| |LLVMOpcode|)
  (|Val| |LLVMValueRef|)
  (|DestTy| |LLVMTypeRef|)
  (|Name| (:pointer :char)))
(defcfun (-build-pointer-cast "LLVMBuildPointerCast") |LLVMValueRef|
  (|LLVMBuilderRef| |LLVMBuilderRef|)
  (|Val| |LLVMValueRef|)
  (|DestTy| |LLVMTypeRef|)
  (|Name| (:pointer :char)))
(defcfun (-build-int-cast "LLVMBuildIntCast") |LLVMValueRef|
  (|LLVMBuilderRef| |LLVMBuilderRef|)
  (|Val| |LLVMValueRef|)
  (|DestTy| |LLVMTypeRef|)
  (|Name| (:pointer :char)))
(defcfun (-build-f-p-cast "LLVMBuildFPCast") |LLVMValueRef|
  (|LLVMBuilderRef| |LLVMBuilderRef|)
  (|Val| |LLVMValueRef|)
  (|DestTy| |LLVMTypeRef|)
  (|Name| (:pointer :char)))
(defcfun (-build-i-cmp "LLVMBuildICmp") |LLVMValueRef|
  (|LLVMBuilderRef| |LLVMBuilderRef|)
  (|Op| |LLVMIntPredicate|)
  (|lhs| |LLVMValueRef|)
  (|rhs| |LLVMValueRef|)
  (|Name| (:pointer :char)))
(defcfun (-build-f-cmp "LLVMBuildFCmp") |LLVMValueRef|
  (|LLVMBuilderRef| |LLVMBuilderRef|)
  (|Op| |LLVMRealPredicate|)
  (|lhs| |LLVMValueRef|)
  (|rhs| |LLVMValueRef|)
  (|Name| (:pointer :char)))
(defcfun (-build-phi "LLVMBuildPhi") |LLVMValueRef|
  (|LLVMBuilderRef| |LLVMBuilderRef|)
  (|Ty| |LLVMTypeRef|)
  (|Name| (:pointer :char)))
(defcfun (-build-call "LLVMBuildCall") |LLVMValueRef|
  (|LLVMBuilderRef| |LLVMBuilderRef|)
  (|Fn| |LLVMValueRef|)
  (|Args| (:pointer |LLVMValueRef|))
  (|NumArgs| :unsigned-int)
  (|Name| (:pointer :char)))
(defcfun (-build-select "LLVMBuildSelect") |LLVMValueRef|
  (|LLVMBuilderRef| |LLVMBuilderRef|)
  (|If| |LLVMValueRef|)
  (|Then| |LLVMValueRef|)
  (|Else| |LLVMValueRef|)
  (|Name| (:pointer :char)))
(defcfun (-build-v-a-arg "LLVMBuildVAArg") |LLVMValueRef|
  (|LLVMBuilderRef| |LLVMBuilderRef|)
  (|List| |LLVMValueRef|)
  (|Ty| |LLVMTypeRef|)
  (|Name| (:pointer :char)))
(defcfun (-build-extract-element "LLVMBuildExtractElement") |LLVMValueRef|
  (|LLVMBuilderRef| |LLVMBuilderRef|)
  (|VecVal| |LLVMValueRef|)
  (|Index| |LLVMValueRef|)
  (|Name| (:pointer :char)))
(defcfun (-build-insert-element "LLVMBuildInsertElement") |LLVMValueRef|
  (|LLVMBuilderRef| |LLVMBuilderRef|)
  (|VecVal| |LLVMValueRef|)
  (|EltVal| |LLVMValueRef|)
  (|Index| |LLVMValueRef|)
  (|Name| (:pointer :char)))
(defcfun (-build-shuffle-vector "LLVMBuildShuffleVector") |LLVMValueRef|
  (|LLVMBuilderRef| |LLVMBuilderRef|)
  (|v1| |LLVMValueRef|)
  (|v2| |LLVMValueRef|)
  (|Mask| |LLVMValueRef|)
  (|Name| (:pointer :char)))
(defcfun (-build-extract-value "LLVMBuildExtractValue") |LLVMValueRef|
  (|LLVMBuilderRef| |LLVMBuilderRef|)
  (|AggVal| |LLVMValueRef|)
  (|Index| :unsigned-int)
  (|Name| (:pointer :char)))
(defcfun (-build-insert-value "LLVMBuildInsertValue") |LLVMValueRef|
  (|LLVMBuilderRef| |LLVMBuilderRef|)
  (|AggVal| |LLVMValueRef|)
  (|EltVal| |LLVMValueRef|)
  (|Index| :unsigned-int)
  (|Name| (:pointer :char)))
(defcfun (-build-is-null "LLVMBuildIsNull") |LLVMValueRef|
  (|LLVMBuilderRef| |LLVMBuilderRef|)
  (|Val| |LLVMValueRef|)
  (|Name| (:pointer :char)))
(defcfun (-build-is-not-null "LLVMBuildIsNotNull") |LLVMValueRef|
  (|LLVMBuilderRef| |LLVMBuilderRef|)
  (|Val| |LLVMValueRef|)
  (|Name| (:pointer :char)))
(defcfun (-build-ptr-diff "LLVMBuildPtrDiff") |LLVMValueRef|
  (|LLVMBuilderRef| |LLVMBuilderRef|)
  (|lhs| |LLVMValueRef|)
  (|rhs| |LLVMValueRef|)
  (|Name| (:pointer :char)))
(defcfun (-build-fence "LLVMBuildFence") |LLVMValueRef|
  (|b| |LLVMBuilderRef|)
  (ordering |LLVMAtomicOrdering|)
  (|singleThread| |LLVMBool|)
  (|Name| (:pointer :char)))
(defcfun (-build-atomic-r-m-w "LLVMBuildAtomicRMW") |LLVMValueRef|
  (|b| |LLVMBuilderRef|)
  (op |LLVMAtomicRMWBinOp|)
  (|ptr| |LLVMValueRef|)
  (|Val| |LLVMValueRef|)
  (ordering |LLVMAtomicOrdering|)
  (|singleThread| |LLVMBool|))
(defcfun (-create-module-provider-for-existing-module
          "LLVMCreateModuleProviderForExistingModule") |LLVMModuleProviderRef|
  (|m| |LLVMModuleRef|))
(defcfun (-dispose-module-provider "LLVMDisposeModuleProvider") :void
  (|m| |LLVMModuleProviderRef|))
(defcfun (-create-memory-buffer-with-contents-of-file
          "LLVMCreateMemoryBufferWithContentsOfFile") |LLVMBool|
  (|Path| (:pointer :char))
  (|OutMemBuf| (:pointer |LLVMMemoryBufferRef|))
  (|OutMessage| (:pointer (:pointer :char))))
(defcfun (-create-memory-buffer-with-s-t-d-i-n
          "LLVMCreateMemoryBufferWithSTDIN") |LLVMBool|
  (|OutMemBuf| (:pointer |LLVMMemoryBufferRef|))
  (|OutMessage| (:pointer (:pointer :char))))
(defcfun (-create-memory-buffer-with-memory-range
          "LLVMCreateMemoryBufferWithMemoryRange") |LLVMMemoryBufferRef|
  (|InputData| (:pointer :char))
  (|InputDataLength| size_t)
  (|BufferName| (:pointer :char))
  (|RequiresNullTerminator| |LLVMBool|))
(defcfun (-create-memory-buffer-with-memory-range-copy
          "LLVMCreateMemoryBufferWithMemoryRangeCopy") |LLVMMemoryBufferRef|
  (|InputData| (:pointer :char))
  (|InputDataLength| size_t)
  (|BufferName| (:pointer :char)))
(defcfun (-get-buffer-start "LLVMGetBufferStart") (:pointer :char)
  (|MemBuf| |LLVMMemoryBufferRef|))
(defcfun (-get-buffer-size "LLVMGetBufferSize") size_t
  (|MemBuf| |LLVMMemoryBufferRef|))
(defcfun (-dispose-memory-buffer "LLVMDisposeMemoryBuffer") :void
  (|MemBuf| |LLVMMemoryBufferRef|))
(defcfun (-get-global-pass-registry
          "LLVMGetGlobalPassRegistry") |LLVMPassRegistryRef|)
(defcfun (-create-pass-manager
          "LLVMCreatePassManager") |LLVMPassManagerRef|)
(defcfun (-create-function-pass-manager-for-module
          "LLVMCreateFunctionPassManagerForModule") |LLVMPassManagerRef|
  (|m| |LLVMModuleRef|))
(defcfun (-create-function-pass-manager
          "LLVMCreateFunctionPassManager") |LLVMPassManagerRef|
  (|mp| |LLVMModuleProviderRef|))
(defcfun (-run-pass-manager "LLVMRunPassManager") |LLVMBool|
  (|pm| |LLVMPassManagerRef|)
  (|m| |LLVMModuleRef|))
(defcfun (-initialize-function-pass-manager
          "LLVMInitializeFunctionPassManager") |LLVMBool|
  (|fpm| |LLVMPassManagerRef|))
(defcfun (-run-function-pass-manager
          "LLVMRunFunctionPassManager") |LLVMBool|
  (|fpm| |LLVMPassManagerRef|)
  (|f| |LLVMValueRef|))
(defcfun (-finalize-function-pass-manager
          "LLVMFinalizeFunctionPassManager") |LLVMBool|
  (|fpm| |LLVMPassManagerRef|))
(defcfun (-dispose-pass-manager "LLVMDisposePassManager") :void
  (|pm| |LLVMPassManagerRef|))
(defcfun (-start-multithreaded "LLVMStartMultithreaded") |LLVMBool|)
(defcfun (-stop-multithreaded "LLVMStopMultithreaded") :void)
(defcfun (-is-multithreaded "LLVMIsMultithreaded") |LLVMBool|)

;;;;Target

;;;include llvm-c/Types.h
;;;include llvm/Config/llvm-config.h
(defcenum |LLVMByteOrdering|
  |LLVMBigEndian|
  |LLVMLittleEndian|)
(defctype |LLVMTargetDataRef| :pointer)
(defctype |LLVMTargetLibraryInfoRef| :pointer)
;;;include llvm/Config/Targets.def
;;;include llvm/Config/Targets.def
;;;include llvm/Config/Targets.def
;;;include llvm/Config/AsmPrinters.def
;;;include llvm/Config/AsmParsers.def
;;;include llvm/Config/Disassemblers.def
(defcfun (-initialize-all-target-infos
          "LLVMInitializeAllTargetInfos") :void)
;;;include llvm/Config/Targets.def
(defcfun (-initialize-all-targets "LLVMInitializeAllTargets") :void)
;;;include llvm/Config/Targets.def
(defcfun (-initialize-all-target-m-cs "LLVMInitializeAllTargetMCs") :void)
;;;include llvm/Config/Targets.def
(defcfun (-initialize-all-asm-printers
          "LLVMInitializeAllAsmPrinters") :void)
;;;include llvm/Config/AsmPrinters.def
(defcfun (-initialize-all-asm-parsers "LLVMInitializeAllAsmParsers") :void)
;;;include llvm/Config/AsmParsers.def
(defcfun (-initialize-all-disassemblers
          "LLVMInitializeAllDisassemblers") :void)
;;;include llvm/Config/Disassemblers.def
(defcfun (-initialize-native-target
          "LLVMInitializeNativeTarget") |LLVMBool|)
(defcfun (-initialize-native-asm-parser
          "LLVMInitializeNativeAsmParser") |LLVMBool|)
(defcfun (-initialize-native-asm-printer
          "LLVMInitializeNativeAsmPrinter") |LLVMBool|)
(defcfun (-initialize-native-disassembler
          "LLVMInitializeNativeDisassembler") |LLVMBool|)
(defcfun (-create-target-data "LLVMCreateTargetData") |LLVMTargetDataRef|
  (|StringRep| (:pointer :char)))
(defcfun (-add-target-data "LLVMAddTargetData") :void
  (|td| |LLVMTargetDataRef|)
  (|pm| |LLVMPassManagerRef|))
(defcfun (-add-target-library-info "LLVMAddTargetLibraryInfo") :void
  (|tli| |LLVMTargetLibraryInfoRef|)
  (|pm| |LLVMPassManagerRef|))
(defcfun (-copy-string-rep-of-target-data
          "LLVMCopyStringRepOfTargetData") (:pointer :char)
  (|td| |LLVMTargetDataRef|))
(defcfun (-byte-order "LLVMByteOrder") |LLVMByteOrdering|
  (|td| |LLVMTargetDataRef|))
(defcfun (-pointer-size "LLVMPointerSize") :unsigned-int
  (|td| |LLVMTargetDataRef|))
(defcfun (-pointer-size-for-a-s "LLVMPointerSizeForAS") :unsigned-int
  (|td| |LLVMTargetDataRef|)
  (|as| :unsigned-int))
(defcfun (-int-ptr-type "LLVMIntPtrType") |LLVMTypeRef|
  (|td| |LLVMTargetDataRef|))
(defcfun (-int-ptr-type-for-a-s "LLVMIntPtrTypeForAS") |LLVMTypeRef|
  (|td| |LLVMTargetDataRef|)
  (|as| :unsigned-int))
(defcfun (-int-ptr-type-in-context "LLVMIntPtrTypeInContext") |LLVMTypeRef|
  (|c| |LLVMContextRef|)
  (|td| |LLVMTargetDataRef|))
(defcfun (-int-ptr-type-for-a-s-in-context
          "LLVMIntPtrTypeForASInContext") |LLVMTypeRef|
  (|c| |LLVMContextRef|)
  (|td| |LLVMTargetDataRef|)
  (|as| :unsigned-int))
(defcfun (-size-of-type-in-bits "LLVMSizeOfTypeInBits") :unsigned-long-long
  (|td| |LLVMTargetDataRef|)
  (|Ty| |LLVMTypeRef|))
(defcfun (-store-size-of-type "LLVMStoreSizeOfType") :unsigned-long-long
  (|td| |LLVMTargetDataRef|)
  (|Ty| |LLVMTypeRef|))
(defcfun (-a-b-i-size-of-type "LLVMABISizeOfType") :unsigned-long-long
  (|td| |LLVMTargetDataRef|)
  (|Ty| |LLVMTypeRef|))
(defcfun (-a-b-i-alignment-of-type "LLVMABIAlignmentOfType") :unsigned-int
  (|td| |LLVMTargetDataRef|)
  (|Ty| |LLVMTypeRef|))
(defcfun (-call-frame-alignment-of-type
          "LLVMCallFrameAlignmentOfType") :unsigned-int
  (|td| |LLVMTargetDataRef|)
  (|Ty| |LLVMTypeRef|))
(defcfun (-preferred-alignment-of-type
          "LLVMPreferredAlignmentOfType") :unsigned-int
  (|td| |LLVMTargetDataRef|)
  (|Ty| |LLVMTypeRef|))
(defcfun (-preferred-alignment-of-global
          "LLVMPreferredAlignmentOfGlobal") :unsigned-int
  (|td| |LLVMTargetDataRef|)
  (|GlobalVar| |LLVMValueRef|))
(defcfun (-element-at-offset "LLVMElementAtOffset") :unsigned-int
  (|td| |LLVMTargetDataRef|)
  (|StructTy| |LLVMTypeRef|)
  (|Offset| :unsigned-long-long))
(defcfun (-offset-of-element "LLVMOffsetOfElement") :unsigned-long-long
  (|td| |LLVMTargetDataRef|)
  (|StructTy| |LLVMTypeRef|)
  (|Element| :unsigned-int))
(defcfun (-dispose-target-data "LLVMDisposeTargetData") :void
  (|td| |LLVMTargetDataRef|))

;;;;TargetMachine

;;;include llvm-c/Types.h
;;;include llvm-c/Target.h
(defctype |LLVMTargetMachineRef| :pointer)
(defctype |LLVMTargetRef| :pointer)
(defcenum |LLVMCodeGenOptLevel|
  |LLVMCodeGenLevelNone|
  |LLVMCodeGenLevelLess|
  |LLVMCodeGenLevelDefault|
  |LLVMCodeGenLevelAggressive|)
(defcenum |LLVMRelocMode|
  |LLVMRelocDefault|
  |LLVMRelocStatic|
  |LLVMRelocPIC|
  |LLVMRelocDynamicNoPic|)
(defcenum |LLVMCodeModel|
  |LLVMCodeModelDefault|
  |LLVMCodeModelJITDefault|
  |LLVMCodeModelSmall|
  |LLVMCodeModelKernel|
  |LLVMCodeModelMedium|
  |LLVMCodeModelLarge|)
(defcenum |LLVMCodeGenFileType|
  |LLVMAssemblyFile|
  |LLVMObjectFile|)
(defcfun (-get-first-target "LLVMGetFirstTarget") |LLVMTargetRef|)
(defcfun (-get-next-target "LLVMGetNextTarget") |LLVMTargetRef|
  (|t| |LLVMTargetRef|))
(defcfun (-get-target-from-name "LLVMGetTargetFromName") |LLVMTargetRef|
  (|Name| (:pointer :char)))
(defcfun (-get-target-from-triple "LLVMGetTargetFromTriple") |LLVMBool|
  (|Triple| (:pointer :char))
  (|t| (:pointer |LLVMTargetRef|))
  (|ErrorMessage| (:pointer (:pointer :char))))
(defcfun (-get-target-name "LLVMGetTargetName") (:pointer :char)
  (|t| |LLVMTargetRef|))
(defcfun (-get-target-description "LLVMGetTargetDescription") (:pointer
                                                                    :char)
  (|t| |LLVMTargetRef|))
(defcfun (-target-has-j-i-t "LLVMTargetHasJIT") |LLVMBool|
  (|t| |LLVMTargetRef|))
(defcfun (-target-has-target-machine
          "LLVMTargetHasTargetMachine") |LLVMBool|
  (|t| |LLVMTargetRef|))
(defcfun (-target-has-asm-backend "LLVMTargetHasAsmBackend") |LLVMBool|
  (|t| |LLVMTargetRef|))
(defcfun (-create-target-machine
          "LLVMCreateTargetMachine") |LLVMTargetMachineRef|
  (|t| |LLVMTargetRef|)
  (|Triple| (:pointer :char))
  (|cpu| (:pointer :char))
  (|Features| (:pointer :char))
  (|Level| |LLVMCodeGenOptLevel|)
  (|Reloc| |LLVMRelocMode|)
  (|CodeModel| |LLVMCodeModel|))
(defcfun (-dispose-target-machine "LLVMDisposeTargetMachine") :void
  (|t| |LLVMTargetMachineRef|))
(defcfun (-get-target-machine-target
          "LLVMGetTargetMachineTarget") |LLVMTargetRef|
  (|t| |LLVMTargetMachineRef|))
(defcfun (-get-target-machine-triple
          "LLVMGetTargetMachineTriple") (:pointer :char)
  (|t| |LLVMTargetMachineRef|))
(defcfun (-get-target-machine-c-p-u "LLVMGetTargetMachineCPU") (:pointer
                                                                     :char)
  (|t| |LLVMTargetMachineRef|))
(defcfun (-get-target-machine-feature-string
          "LLVMGetTargetMachineFeatureString") (:pointer :char)
  (|t| |LLVMTargetMachineRef|))
(defcfun (-get-target-machine-data
          "LLVMGetTargetMachineData") |LLVMTargetDataRef|
  (|t| |LLVMTargetMachineRef|))
(defcfun (-set-target-machine-asm-verbosity
          "LLVMSetTargetMachineAsmVerbosity") :void
  (|t| |LLVMTargetMachineRef|)
  (|VerboseAsm| |LLVMBool|))
(defcfun (-target-machine-emit-to-file
          "LLVMTargetMachineEmitToFile") |LLVMBool|
  (|t| |LLVMTargetMachineRef|)
  (|m| |LLVMModuleRef|)
  (|Filename| (:pointer :char))
  (codegen |LLVMCodeGenFileType|)
  (|ErrorMessage| (:pointer (:pointer :char))))
(defcfun (-target-machine-emit-to-memory-buffer
          "LLVMTargetMachineEmitToMemoryBuffer") |LLVMBool|
  (|t| |LLVMTargetMachineRef|)
  (|m| |LLVMModuleRef|)
  (codegen |LLVMCodeGenFileType|)
  (|ErrorMessage| (:pointer (:pointer :char)))
  (|OutMemBuf| (:pointer |LLVMMemoryBufferRef|)))
(defcfun (-get-default-target-triple
          "LLVMGetDefaultTargetTriple") (:pointer :char))
(defcfun (-add-analysis-passes "LLVMAddAnalysisPasses") :void
  (|t| |LLVMTargetMachineRef|)
  (|pm| |LLVMPassManagerRef|))


;;;;Analysis

;;;include llvm-c/Types.h
(defcenum |LLVMVerifierFailureAction|
  |LLVMAbortProcessAction|
  |LLVMPrintMessageAction|
  |LLVMReturnStatusAction|)
(defcfun (-verify-module "LLVMVerifyModule") |LLVMBool|
  (|m| |LLVMModuleRef|)
  (|Action| |LLVMVerifierFailureAction|)
  (|OutMessage| (:pointer (:pointer :char))))
(defcfun (-verify-function "LLVMVerifyFunction") |LLVMBool|
  (|Fn| |LLVMValueRef|)
  (|Action| |LLVMVerifierFailureAction|))
(defcfun (-view-function-c-f-g "LLVMViewFunctionCFG") :void
  (|Fn| |LLVMValueRef|))
(defcfun (-view-function-c-f-g-only "LLVMViewFunctionCFGOnly") :void
  (|Fn| |LLVMValueRef|))

;;;;BitReader

;;;include llvm-c/Types.h
(defcfun (-parse-bitcode "LLVMParseBitcode") |LLVMBool|
  (|MemBuf| |LLVMMemoryBufferRef|)
  (|OutModule| (:pointer |LLVMModuleRef|))
  (|OutMessage| (:pointer (:pointer :char))))
(defcfun (-parse-bitcode2 "LLVMParseBitcode2") |LLVMBool|
  (|MemBuf| |LLVMMemoryBufferRef|)
  (|OutModule| (:pointer |LLVMModuleRef|)))
(defcfun (-parse-bitcode-in-context "LLVMParseBitcodeInContext") |LLVMBool|
  (|ContextRef| |LLVMContextRef|)
  (|MemBuf| |LLVMMemoryBufferRef|)
  (|OutModule| (:pointer |LLVMModuleRef|))
  (|OutMessage| (:pointer (:pointer :char))))
(defcfun (-parse-bitcode-in-context2
          "LLVMParseBitcodeInContext2") |LLVMBool|
  (|ContextRef| |LLVMContextRef|)
  (|MemBuf| |LLVMMemoryBufferRef|)
  (|OutModule| (:pointer |LLVMModuleRef|)))
(defcfun (-get-bitcode-module-in-context
          "LLVMGetBitcodeModuleInContext") |LLVMBool|
  (|ContextRef| |LLVMContextRef|)
  (|MemBuf| |LLVMMemoryBufferRef|)
  (|OutM| (:pointer |LLVMModuleRef|))
  (|OutMessage| (:pointer (:pointer :char))))
(defcfun (-get-bitcode-module-in-context2
          "LLVMGetBitcodeModuleInContext2") |LLVMBool|
  (|ContextRef| |LLVMContextRef|)
  (|MemBuf| |LLVMMemoryBufferRef|)
  (|OutM| (:pointer |LLVMModuleRef|)))
(defcfun (-get-bitcode-module "LLVMGetBitcodeModule") |LLVMBool|
  (|MemBuf| |LLVMMemoryBufferRef|)
  (|OutM| (:pointer |LLVMModuleRef|))
  (|OutMessage| (:pointer (:pointer :char))))
(defcfun (-get-bitcode-module2 "LLVMGetBitcodeModule2") |LLVMBool|
  (|MemBuf| |LLVMMemoryBufferRef|)
  (|OutM| (:pointer |LLVMModuleRef|)))

;;;;BitWriter

;;;include llvm-c/Types.h
(defcfun (-write-bitcode-to-file "LLVMWriteBitcodeToFile") :int
  (|m| |LLVMModuleRef|)
  (|Path| (:pointer :char)))
(defcfun (-write-bitcode-to-f-d "LLVMWriteBitcodeToFD") :int
  (|m| |LLVMModuleRef|)
  (|fd| :int)
  (|ShouldClose| :int)
  (|Unbuffered| :int))
(defcfun (-write-bitcode-to-file-handle
          "LLVMWriteBitcodeToFileHandle") :int
  (|m| |LLVMModuleRef|)
  (|Handle| :int))
(defcfun (-write-bitcode-to-memory-buffer
          "LLVMWriteBitcodeToMemoryBuffer") |LLVMMemoryBufferRef|
  (|m| |LLVMModuleRef|))

;;;;Disassembler

;;;include llvm/Support/DataTypes.h
;;;include stddef.h
(defctype |LLVMDisasmContextRef| :pointer)
#+nil
(defcstruct ((:struct) |LLVMOpInfoSymbol1|)
  ((uint64_t (|Value|)) ((:pointer :char) (|Name|)) (uint64_t (|Present|))))
#+nil
(defcstruct ((:struct) |LLVMOpInfo1|)
 ((uint64_t (|VariantKind|)) (uint64_t (|Value|))
  (((|LLVMOpInfoSymbol1| :struct) |AddSymbol|)
   (nil :struct |LLVMOpInfoSymbol1| |SubtractSymbol|))))
#+nil
(defctype :* :char)
#+nil
(defcfun (-create-disasm "LLVMCreateDisasm") |LLVMDisasmContextRef|
  (|TripleName| (:pointer :char))
  (|DisInfo| (:pointer :void))
  (|TagType| :int)
  (|GetOpInfo| |LLVMOpInfoCallback|)
  (|SymbolLookUp| |LLVMSymbolLookupCallback|))
#+nil
(defcfun (-create-disasm-c-p-u
          "LLVMCreateDisasmCPU") |LLVMDisasmContextRef|
  (|Triple| (:pointer :char))
  (|cpu| (:pointer :char))
  (|DisInfo| (:pointer :void))
  (|TagType| :int)
  (|GetOpInfo| |LLVMOpInfoCallback|)
  (|SymbolLookUp| |LLVMSymbolLookupCallback|))
#+nil
(defcfun (-create-disasm-c-p-u-features
          "LLVMCreateDisasmCPUFeatures") |LLVMDisasmContextRef|
  (|Triple| (:pointer :char))
  (|cpu| (:pointer :char))
  (|Features| (:pointer :char))
  (|DisInfo| (:pointer :void))
  (|TagType| :int)
  (|GetOpInfo| |LLVMOpInfoCallback|)
  (|SymbolLookUp| |LLVMSymbolLookupCallback|))
(defcfun (-set-disasm-options "LLVMSetDisasmOptions") :int
  (|dc| |LLVMDisasmContextRef|)
  (|Options| uint64_t))
(defcfun (-disasm-dispose "LLVMDisasmDispose") :void
  (|dc| |LLVMDisasmContextRef|))
(defcfun (-disasm-instruction "LLVMDisasmInstruction") size_t
  (|dc| |LLVMDisasmContextRef|)
  (|Bytes| (:pointer uint8_t))
  (|BytesSize| uint64_t)
  (|pc| uint64_t)
  (|OutString| (:pointer :char))
  (|OutStringSize| size_t))

;;;;ErrorHandling

;;;include llvm-c/Types.h
(defcfun (-install-fatal-error-handler
          "LLVMInstallFatalErrorHandler") :void
  (|Handler| |LLVMFatalErrorHandler|))
(defcfun (-reset-fatal-error-handler "LLVMResetFatalErrorHandler") :void)
(defcfun (-enable-pretty-stack-trace "LLVMEnablePrettyStackTrace") :void)

;;;;ExecutionEngine

;;;include llvm-c/Types.h
;;;include llvm-c/Target.h
;;;include llvm-c/TargetMachine.h
(defcfun (-link-in-m-c-j-i-t "LLVMLinkInMCJIT") :void)
(defcfun (-link-in-interpreter "LLVMLinkInInterpreter") :void)
(defctype |LLVMGenericValueRef| :pointer)
(defctype |LLVMExecutionEngineRef| :pointer)
(defctype |LLVMMCJITMemoryManagerRef| :pointer)
#+nil
(defcstruct ((:struct) |LLVMMCJITCompilerOptions|)
 ((|LLVMMCJITMemoryManagerRef| (|mcjmm|)) (|LLVMBool| (|EnableFastISel|))
  (|LLVMBool| (|NoFramePointerElim|)) (|LLVMCodeModel| (|CodeModel|))
  (:unsigned-int (|OptLevel|))))
(defcfun (-create-generic-value-of-int
          "LLVMCreateGenericValueOfInt") |LLVMGenericValueRef|
  (|Ty| |LLVMTypeRef|)
  (|n| :unsigned-long-long)
  (|IsSigned| |LLVMBool|))
(defcfun (-create-generic-value-of-pointer
          "LLVMCreateGenericValueOfPointer") |LLVMGenericValueRef|
  (|p| (:pointer :void)))
(defcfun (-create-generic-value-of-float
          "LLVMCreateGenericValueOfFloat") |LLVMGenericValueRef|
  (|Ty| |LLVMTypeRef|)
  (|n| :double))
(defcfun (-generic-value-int-width
          "LLVMGenericValueIntWidth") :unsigned-int
  (|GenValRef| |LLVMGenericValueRef|))
(defcfun (-generic-value-to-int
          "LLVMGenericValueToInt") :unsigned-long-long
  (|GenVal| |LLVMGenericValueRef|)
  (|IsSigned| |LLVMBool|))
(defcfun (-generic-value-to-pointer "LLVMGenericValueToPointer") (:pointer
                                                                       :void)
  (|GenVal| |LLVMGenericValueRef|))
(defcfun (-generic-value-to-float "LLVMGenericValueToFloat") :double
  (|TyRef| |LLVMTypeRef|)
  (|GenVal| |LLVMGenericValueRef|))
(defcfun (-dispose-generic-value "LLVMDisposeGenericValue") :void
  (|GenVal| |LLVMGenericValueRef|))
(defcfun (-create-execution-engine-for-module
          "LLVMCreateExecutionEngineForModule") |LLVMBool|
  (|OutEE| (:pointer |LLVMExecutionEngineRef|))
  (|m| |LLVMModuleRef|)
  (|OutError| (:pointer (:pointer :char))))
(defcfun (-create-interpreter-for-module
          "LLVMCreateInterpreterForModule") |LLVMBool|
  (|OutInterp| (:pointer |LLVMExecutionEngineRef|))
  (|m| |LLVMModuleRef|)
  (|OutError| (:pointer (:pointer :char))))
(defcfun (-create-j-i-t-compiler-for-module
          "LLVMCreateJITCompilerForModule") |LLVMBool|
  (|OutJIT| (:pointer |LLVMExecutionEngineRef|))
  (|m| |LLVMModuleRef|)
  (|OptLevel| :unsigned-int)
  (|OutError| (:pointer (:pointer :char))))
(defcfun (-initialize-m-c-j-i-t-compiler-options
          "LLVMInitializeMCJITCompilerOptions") :void
  (|Options| :pointer ;(:pointer :struct)
	     )
  (|SizeOfOptions| size_t))
(defcfun (-create-m-c-j-i-t-compiler-for-module
          "LLVMCreateMCJITCompilerForModule") |LLVMBool|
  (|OutJIT| (:pointer |LLVMExecutionEngineRef|))
  (|m| |LLVMModuleRef|)
  (|Options| :pointer ;;(:pointer :struct)
	     )
  (|SizeOfOptions| size_t)
  (|OutError| (:pointer (:pointer :char))))
(defcfun (-dispose-execution-engine "LLVMDisposeExecutionEngine") :void
  (|ee| |LLVMExecutionEngineRef|))
(defcfun (-run-static-constructors "LLVMRunStaticConstructors") :void
  (|ee| |LLVMExecutionEngineRef|))
(defcfun (-run-static-destructors "LLVMRunStaticDestructors") :void
  (|ee| |LLVMExecutionEngineRef|))
(defcfun (-run-function-as-main "LLVMRunFunctionAsMain") :int
  (|ee| |LLVMExecutionEngineRef|)
  (|f| |LLVMValueRef|)
  (|ArgC| :unsigned-int)
  (|ArgV| (:pointer (:pointer :char)))
  (|EnvP| (:pointer (:pointer :char))))
(defcfun (-run-function "LLVMRunFunction") |LLVMGenericValueRef|
  (|ee| |LLVMExecutionEngineRef|)
  (|f| |LLVMValueRef|)
  (|NumArgs| :unsigned-int)
  (|Args| (:pointer |LLVMGenericValueRef|)))
(defcfun (-free-machine-code-for-function
          "LLVMFreeMachineCodeForFunction") :void
  (|ee| |LLVMExecutionEngineRef|)
  (|f| |LLVMValueRef|))
(defcfun (-add-module "LLVMAddModule") :void
  (|ee| |LLVMExecutionEngineRef|)
  (|m| |LLVMModuleRef|))
(defcfun (-remove-module "LLVMRemoveModule") |LLVMBool|
  (|ee| |LLVMExecutionEngineRef|)
  (|m| |LLVMModuleRef|)
  (|OutMod| (:pointer |LLVMModuleRef|))
  (|OutError| (:pointer (:pointer :char))))
(defcfun (-find-function "LLVMFindFunction") |LLVMBool|
  (|ee| |LLVMExecutionEngineRef|)
  (|Name| (:pointer :char))
  (|OutFn| (:pointer |LLVMValueRef|)))
(defcfun (-recompile-and-relink-function
          "LLVMRecompileAndRelinkFunction") (:pointer :void)
  (|ee| |LLVMExecutionEngineRef|)
  (|Fn| |LLVMValueRef|))
(defcfun (-get-execution-engine-target-data
          "LLVMGetExecutionEngineTargetData") |LLVMTargetDataRef|
  (|ee| |LLVMExecutionEngineRef|))
(defcfun (-get-execution-engine-target-machine
          "LLVMGetExecutionEngineTargetMachine") |LLVMTargetMachineRef|
  (|ee| |LLVMExecutionEngineRef|))
(defcfun (-add-global-mapping "LLVMAddGlobalMapping") :void
  (|ee| |LLVMExecutionEngineRef|)
  (|Global| |LLVMValueRef|)
  (|Addr| (:pointer :void)))
(defcfun (-get-pointer-to-global "LLVMGetPointerToGlobal") (:pointer :void)
  (|ee| |LLVMExecutionEngineRef|)
  (|Global| |LLVMValueRef|))
(defcfun (-get-global-value-address "LLVMGetGlobalValueAddress") uint64_t
  (|ee| |LLVMExecutionEngineRef|)
  (|Name| (:pointer :char)))
(defcfun (-get-function-address "LLVMGetFunctionAddress") uint64_t
  (|ee| |LLVMExecutionEngineRef|)
  (|Name| (:pointer :char)))
#+nil
(defctype :* uint8_t)
#+nil
(defctype :* uint8_t)
#+nil
(defcfun (-create-simple-m-c-j-i-t-memory-manager
          "LLVMCreateSimpleMCJITMemoryManager") |LLVMMCJITMemoryManagerRef|
  (|Opaque| (:pointer :void))
  (|AllocateCodeSection| |LLVMMemoryManagerAllocateCodeSectionCallback|)
  (|AllocateDataSection| |LLVMMemoryManagerAllocateDataSectionCallback|)
  (|FinalizeMemory| |LLVMMemoryManagerFinalizeMemoryCallback|)
  (|Destroy| |LLVMMemoryManagerDestroyCallback|))
(defcfun (-dispose-m-c-j-i-t-memory-manager
          "LLVMDisposeMCJITMemoryManager") :void
  (|mm| |LLVMMCJITMemoryManagerRef|))

;;;;IRReader

;;;include llvm-c/Types.h
(defcfun (-parse-i-r-in-context "LLVMParseIRInContext") |LLVMBool|
  (|ContextRef| |LLVMContextRef|)
  (|MemBuf| |LLVMMemoryBufferRef|)
  (|OutM| (:pointer |LLVMModuleRef|))
  (|OutMessage| (:pointer (:pointer :char))))

;;;;Initialization

;;;include llvm-c/Types.h
(defcfun (-initialize-core "LLVMInitializeCore") :void
  (|r| |LLVMPassRegistryRef|))
(defcfun (-initialize-transform-utils "LLVMInitializeTransformUtils") :void
  (|r| |LLVMPassRegistryRef|))
(defcfun (-initialize-scalar-opts "LLVMInitializeScalarOpts") :void
  (|r| |LLVMPassRegistryRef|))
(defcfun (-initialize-obj-c-a-r-c-opts "LLVMInitializeObjCARCOpts") :void
  (|r| |LLVMPassRegistryRef|))
(defcfun (-initialize-vectorization "LLVMInitializeVectorization") :void
  (|r| |LLVMPassRegistryRef|))
(defcfun (-initialize-inst-combine "LLVMInitializeInstCombine") :void
  (|r| |LLVMPassRegistryRef|))
(defcfun (-initialize-i-p-o "LLVMInitializeIPO") :void
  (|r| |LLVMPassRegistryRef|))
(defcfun (-initialize-instrumentation
          "LLVMInitializeInstrumentation") :void
  (|r| |LLVMPassRegistryRef|))
(defcfun (-initialize-analysis "LLVMInitializeAnalysis") :void
  (|r| |LLVMPassRegistryRef|))
(defcfun (-initialize-i-p-a "LLVMInitializeIPA") :void
  (|r| |LLVMPassRegistryRef|))
(defcfun (-initialize-code-gen "LLVMInitializeCodeGen") :void
  (|r| |LLVMPassRegistryRef|))
(defcfun (-initialize-target "LLVMInitializeTarget") :void
  (|r| |LLVMPassRegistryRef|))

;;;;LinkTimeOptimizer

(defctype llvm_lto_t :pointer)
(defctype llvm_lto_status :int ;:enum
  )
#+nil
(defcfun (_-c-r-e-a-t-e_-o-p-t-i-m-i-z-e-r
          "LLVM_CREATE_OPTIMIZER") llvm_lto_t)
#+nil
(defcfun (_-d-e-s-t-r-o-y_-o-p-t-i-m-i-z-e-r
          "LLVM_DESTROY_OPTIMIZER") :void
  (lto llvm_lto_t))
#+nil
(defcfun (_-r-e-a-d_-o-b-j-e-c-t_-f-i-l-e
          "LLVM_READ_OBJECT_FILE") llvm_lto_status_t
  (lto llvm_lto_t)
  (input_filename (:pointer :char)))
#+nil
(defcfun (_-o-p-t-i-m-i-z-e_-m-o-d-u-l-e-s
          "LLVM_OPTIMIZE_MODULES") llvm_lto_status_t
  (lto llvm_lto_t)
  (output_filename (:pointer :char)))

;;;;Linker

;;;include llvm-c/Types.h
(defcenum |LLVMLinkerMode|
  (|LLVMLinkerDestroySource| 0)
  (|LLVMLinkerPreserveSource_Removed| 1))
(defcfun (-link-modules "LLVMLinkModules") |LLVMBool|
  (|Dest| |LLVMModuleRef|)
  (|Src| |LLVMModuleRef|)
  (|Unused| |LLVMLinkerMode|)
  (|OutMessage| (:pointer (:pointer :char))))
(defcfun (-link-modules2 "LLVMLinkModules2") |LLVMBool|
  (|Dest| |LLVMModuleRef|)
  (|Src| |LLVMModuleRef|))

;;;;Object

;;;include llvm-c/Types.h
;;;include llvm/Config/llvm-config.h
(defctype |LLVMObjectFileRef| :pointer)
(defctype |LLVMSectionIteratorRef| :pointer)
(defctype |LLVMSymbolIteratorRef| :pointer)
(defctype |LLVMRelocationIteratorRef| :pointer)
(defcfun (-create-object-file "LLVMCreateObjectFile") |LLVMObjectFileRef|
  (|MemBuf| |LLVMMemoryBufferRef|))
(defcfun (-dispose-object-file "LLVMDisposeObjectFile") :void
  (|ObjectFile| |LLVMObjectFileRef|))
(defcfun (-get-sections "LLVMGetSections") |LLVMSectionIteratorRef|
  (|ObjectFile| |LLVMObjectFileRef|))
(defcfun (-dispose-section-iterator "LLVMDisposeSectionIterator") :void
  (|si| |LLVMSectionIteratorRef|))
(defcfun (-is-section-iterator-at-end
          "LLVMIsSectionIteratorAtEnd") |LLVMBool|
  (|ObjectFile| |LLVMObjectFileRef|)
  (|si| |LLVMSectionIteratorRef|))
(defcfun (-move-to-next-section "LLVMMoveToNextSection") :void
  (|si| |LLVMSectionIteratorRef|))
(defcfun (-move-to-containing-section "LLVMMoveToContainingSection") :void
  (|Sect| |LLVMSectionIteratorRef|)
  (|Sym| |LLVMSymbolIteratorRef|))
(defcfun (-get-symbols "LLVMGetSymbols") |LLVMSymbolIteratorRef|
  (|ObjectFile| |LLVMObjectFileRef|))
(defcfun (-dispose-symbol-iterator "LLVMDisposeSymbolIterator") :void
  (|si| |LLVMSymbolIteratorRef|))
(defcfun (-is-symbol-iterator-at-end
          "LLVMIsSymbolIteratorAtEnd") |LLVMBool|
  (|ObjectFile| |LLVMObjectFileRef|)
  (|si| |LLVMSymbolIteratorRef|))
(defcfun (-move-to-next-symbol "LLVMMoveToNextSymbol") :void
  (|si| |LLVMSymbolIteratorRef|))
(defcfun (-get-section-name "LLVMGetSectionName") (:pointer :char)
  (|si| |LLVMSectionIteratorRef|))
(defcfun (-get-section-size "LLVMGetSectionSize") uint64_t
  (|si| |LLVMSectionIteratorRef|))
(defcfun (-get-section-contents "LLVMGetSectionContents") (:pointer :char)
  (|si| |LLVMSectionIteratorRef|))
(defcfun (-get-section-address "LLVMGetSectionAddress") uint64_t
  (|si| |LLVMSectionIteratorRef|))
(defcfun (-get-section-contains-symbol
          "LLVMGetSectionContainsSymbol") |LLVMBool|
  (|si| |LLVMSectionIteratorRef|)
  (|Sym| |LLVMSymbolIteratorRef|))
(defcfun (-get-relocations
          "LLVMGetRelocations") |LLVMRelocationIteratorRef|
  (|Section| |LLVMSectionIteratorRef|))
(defcfun (-dispose-relocation-iterator
          "LLVMDisposeRelocationIterator") :void
  (|ri| |LLVMRelocationIteratorRef|))
(defcfun (-is-relocation-iterator-at-end
          "LLVMIsRelocationIteratorAtEnd") |LLVMBool|
  (|Section| |LLVMSectionIteratorRef|)
  (|ri| |LLVMRelocationIteratorRef|))
(defcfun (-move-to-next-relocation "LLVMMoveToNextRelocation") :void
  (|ri| |LLVMRelocationIteratorRef|))
(defcfun (-get-symbol-name "LLVMGetSymbolName") (:pointer :char)
  (|si| |LLVMSymbolIteratorRef|))
(defcfun (-get-symbol-address "LLVMGetSymbolAddress") uint64_t
  (|si| |LLVMSymbolIteratorRef|))
(defcfun (-get-symbol-size "LLVMGetSymbolSize") uint64_t
  (|si| |LLVMSymbolIteratorRef|))
(defcfun (-get-relocation-offset "LLVMGetRelocationOffset") uint64_t
  (|ri| |LLVMRelocationIteratorRef|))
(defcfun (-get-relocation-symbol
          "LLVMGetRelocationSymbol") |LLVMSymbolIteratorRef|
  (|ri| |LLVMRelocationIteratorRef|))
(defcfun (-get-relocation-type "LLVMGetRelocationType") uint64_t
  (|ri| |LLVMRelocationIteratorRef|))
(defcfun (-get-relocation-type-name "LLVMGetRelocationTypeName") (:pointer
                                                                       :char)
  (|ri| |LLVMRelocationIteratorRef|))
(defcfun (-get-relocation-value-string
          "LLVMGetRelocationValueString") (:pointer :char)
  (|ri| |LLVMRelocationIteratorRef|))

;;;;OrcBindings

;;;include llvm-c/Object.h
;;;include llvm-c/Support.h
;;;include llvm-c/TargetMachine.h
(defctype |LLVMOrcJITStackRef| :pointer)
(defctype |LLVMOrcModuleHandle| uint32_t)
(defctype |LLVMOrcTargetAddress| uint64_t)
(defcfun (-orc-create-instance
          "LLVMOrcCreateInstance") |LLVMOrcJITStackRef|
  (|tm| |LLVMTargetMachineRef|))
(defcfun (-orc-get-mangled-symbol "LLVMOrcGetMangledSymbol") :void
  (|JITStack| |LLVMOrcJITStackRef|)
  (|MangledSymbol| (:pointer (:pointer :char)))
  (|Symbol| (:pointer :char)))
(defcfun (-orc-dispose-mangled-symbol "LLVMOrcDisposeMangledSymbol") :void
  (|MangledSymbol| (:pointer :char)))
(defcfun (-orc-create-lazy-compile-callback
          "LLVMOrcCreateLazyCompileCallback") |LLVMOrcTargetAddress|
  (|JITStack| |LLVMOrcJITStackRef|)
  (|Callback| |LLVMOrcLazyCompileCallbackFn|)
  (|CallbackCtx| (:pointer :void)))
(defcfun (-orc-create-indirect-stub "LLVMOrcCreateIndirectStub") :void
  (|JITStack| |LLVMOrcJITStackRef|)
  (|StubName| (:pointer :char))
  (|InitAddr| |LLVMOrcTargetAddress|))
(defcfun (-orc-set-indirect-stub-pointer
          "LLVMOrcSetIndirectStubPointer") :void
  (|JITStack| |LLVMOrcJITStackRef|)
  (|StubName| (:pointer :char))
  (|NewAddr| |LLVMOrcTargetAddress|))
(defcfun (-orc-add-eagerly-compiled-i-r
          "LLVMOrcAddEagerlyCompiledIR") |LLVMOrcModuleHandle|
  (|JITStack| |LLVMOrcJITStackRef|)
  (|Mod| |LLVMModuleRef|)
  (|SymbolResolver| |LLVMOrcSymbolResolverFn|)
  (|SymbolResolverCtx| (:pointer :void)))
(defcfun (-orc-add-lazily-compiled-i-r
          "LLVMOrcAddLazilyCompiledIR") |LLVMOrcModuleHandle|
  (|JITStack| |LLVMOrcJITStackRef|)
  (|Mod| |LLVMModuleRef|)
  (|SymbolResolver| |LLVMOrcSymbolResolverFn|)
  (|SymbolResolverCtx| (:pointer :void)))
(defcfun (-orc-add-object-file
          "LLVMOrcAddObjectFile") |LLVMOrcModuleHandle|
  (|JITStack| |LLVMOrcJITStackRef|)
  (|Obj| |LLVMObjectFileRef|)
  (|SymbolResolver| |LLVMOrcSymbolResolverFn|)
  (|SymbolResolverCtx| (:pointer :void)))
(defcfun (-orc-remove-module "LLVMOrcRemoveModule") :void
  (|JITStack| |LLVMOrcJITStackRef|)
  (|h| |LLVMOrcModuleHandle|))
(defcfun (-orc-get-symbol-address
          "LLVMOrcGetSymbolAddress") |LLVMOrcTargetAddress|
  (|JITStack| |LLVMOrcJITStackRef|)
  (|SymbolName| (:pointer :char)))
(defcfun (-orc-dispose-instance "LLVMOrcDisposeInstance") :void
  (|JITStack| |LLVMOrcJITStackRef|))

;;;;Support

;;;include llvm/Support/DataTypes.h
;;;include llvm-c/Types.h
(defcfun (-load-library-permanently
          "LLVMLoadLibraryPermanently") |LLVMBool|
  (|Filename| (:pointer :char)))
(defcfun (-parse-command-line-options "LLVMParseCommandLineOptions") :void
  (argc :int)
  (argv (:pointer (:pointer :char)))
  (|Overview| (:pointer :char)))
(defcfun (-search-for-address-of-symbol
          "LLVMSearchForAddressOfSymbol") (:pointer :void)
  (|symbolName| (:pointer :char)))
(defcfun (-add-symbol "LLVMAddSymbol") :void
  (|symbolName| (:pointer :char))
  (|symbolValue| (:pointer :void)))

;;;;lto

;;;include stddef.h
;;;include sys/types.h
;;;include stdbool.h
(defctype lto_bool_t :int)
(defcenum lto_symbol_attributes
  (|lto_symbol_alignment_mask| 31)
  (|lto_symbol_permissions_mask| 224)
  (|lto_symbol_permissions_code| 160)
  (|lto_symbol_permissions_data| 192)
  (|lto_symbol_permissions_rodata| 128)
  (|lto_symbol_definition_mask| 1792)
  (|lto_symbol_definition_regular| 256)
  (|lto_symbol_definition_tentative| 512)
  (|lto_symbol_definition_weak| 768)
  (|lto_symbol_definition_undefined| 1024)
  (|lto_symbol_definition_weakundef| 1280)
  (|lto_symbol_scope_mask| 14336)
  (|lto_symbol_scope_internal| 2048)
  (|lto_symbol_scope_hidden| 4096)
  (|lto_symbol_scope_protected| 8192)
  (|lto_symbol_scope_default| 6144)
  (|lto_symbol_scope_default_can_be_hidden| 10240)
  (|lto_symbol_comdat| 16384)
  (|lto_symbol_alias| 32768))
(defcenum lto_debug_model
  (|lto_debug_model_none| 0)
  (|lto_debug_model_dwarf| 1))
(defcenum lto_codegen_model
  (|lto_codegen_pic_model_static| 0)
  (|lto_codegen_pic_model_dynamic| 1)
  (|lto_codegen_pic_model_dynamic_no_pic| 2)
  (|lto_codegen_pic_model_default| 3))
(defctype lto_module_t :pointer)
(defctype lto_code_gen_t :pointer)
(defcfun (lto_get_version "LTO_GET_VERSION") (:pointer :char))
(defcfun (lto_get_error_message "LTO_GET_ERROR_MESSAGE") (:pointer :char))
(defcfun (lto_module_is_object_file "LTO_MODULE_IS_OBJECT_FILE") lto_bool_t
  (path (:pointer :char)))
(defcfun (lto_module_is_object_file_for_target
          "LTO_MODULE_IS_OBJECT_FILE_FOR_TARGET") lto_bool_t
  (path (:pointer :char))
  (target_triple_prefix (:pointer :char)))
(defcfun (lto_module_is_object_file_in_memory
          "LTO_MODULE_IS_OBJECT_FILE_IN_MEMORY") lto_bool_t
  (mem :pointer ;(:pointer :const)
       )
  (length size_t))
(defcfun (lto_module_is_object_file_in_memory_for_target
          "LTO_MODULE_IS_OBJECT_FILE_IN_MEMORY_FOR_TARGET") lto_bool_t
  (mem :pointer ;(:pointer :const)
       )
  (length size_t)
  (target_triple_prefix (:pointer :char)))
(defcfun (lto_module_create "LTO_MODULE_CREATE") lto_module_t
  (path :pointer ;(:pointer :char)
	))
(defcfun (lto_module_create_from_memory
          "LTO_MODULE_CREATE_FROM_MEMORY") lto_module_t
  (mem :pointer ;(:pointer :const)
       )
  (length size_t))
(defcfun (lto_module_create_from_memory_with_path
          "LTO_MODULE_CREATE_FROM_MEMORY_WITH_PATH") lto_module_t
  (mem :pointer;(:pointer :const)
       )
  (length size_t)
  (path (:pointer :char)))
(defcfun (lto_module_create_in_local_context
          "LTO_MODULE_CREATE_IN_LOCAL_CONTEXT") lto_module_t
  (mem :pointer ;(:pointer :const)
   )
  (length size_t)
  (path (:pointer :char)))
(defcfun (lto_module_create_in_codegen_context
          "LTO_MODULE_CREATE_IN_CODEGEN_CONTEXT") lto_module_t
  (mem :pointer ;(:pointer :const)
   )
  (length size_t)
  (path (:pointer :char))
  (cg lto_code_gen_t))
(defcfun (lto_module_create_from_fd "LTO_MODULE_CREATE_FROM_FD") lto_module_t
  (fd :int)
  (path (:pointer :char))
  (file_size size_t))
(defcfun (lto_module_create_from_fd_at_offset
          "LTO_MODULE_CREATE_FROM_FD_AT_OFFSET") lto_module_t
  (fd :int)
  (path (:pointer :char))
  (file_size size_t)
  (map_size size_t)
  (offset off_t))
(defcfun (lto_module_dispose "LTO_MODULE_DISPOSE") :void
  (mod lto_module_t))
(defcfun (lto_module_get_target_triple
          "LTO_MODULE_GET_TARGET_TRIPLE") (:pointer :char)
  (mod lto_module_t))
(defcfun (lto_module_set_target_triple "LTO_MODULE_SET_TARGET_TRIPLE") :void
  (mod lto_module_t)
  (triple (:pointer :char)))
(defcfun (lto_module_get_num_symbols "LTO_MODULE_GET_NUM_SYMBOLS") :unsigned-int
  (mod lto_module_t))
(defcfun (lto_module_get_symbol_name "LTO_MODULE_GET_SYMBOL_NAME") (:pointer
                                                                    :char)
  (mod lto_module_t)
  (index :unsigned-int))
(defcfun (lto_module_get_symbol_attribute
          "LTO_MODULE_GET_SYMBOL_ATTRIBUTE") lto_symbol_attributes
  (mod lto_module_t)
  (index :unsigned-int))
(defcfun (lto_module_get_linkeropts "LTO_MODULE_GET_LINKEROPTS") (:pointer
                                                                  :char)
  (mod lto_module_t))
(defcenum lto_codegen_diagnostic_severity_t
  (|lto_ds_error| 0)
  (|lto_ds_warning| 1)
  (|lto_ds_remark| 3)
  (|lto_ds_note| 2))
#+nil
(defcfun (lto_codegen_set_diagnostic_handler
          "LTO_CODEGEN_SET_DIAGNOSTIC_HANDLER") :void
  (lto_code_gen_t lto_code_gen_t)
  (lto_diagnostic_handler_t lto_diagnostic_handler_t)
  (:pointer)
  #+nil
  (:* (:pointer :void)))
(defcfun (lto_codegen_create "LTO_CODEGEN_CREATE") lto_code_gen_t)
(defcfun (lto_codegen_create_in_local_context
          "LTO_CODEGEN_CREATE_IN_LOCAL_CONTEXT") lto_code_gen_t)
(defcfun (lto_codegen_dispose "LTO_CODEGEN_DISPOSE") :void
  (lto_code_gen_t lto_code_gen_t))
(defcfun (lto_codegen_add_module "LTO_CODEGEN_ADD_MODULE") lto_bool_t
  (cg lto_code_gen_t)
  (mod lto_module_t))
(defcfun (lto_codegen_set_module "LTO_CODEGEN_SET_MODULE") :void
  (cg lto_code_gen_t)
  (mod lto_module_t))
(defcfun (lto_codegen_set_debug_model "LTO_CODEGEN_SET_DEBUG_MODEL") lto_bool_t
  (cg lto_code_gen_t)
  (lto_debug_model lto_debug_model))
(defcfun (lto_codegen_set_pic_model "LTO_CODEGEN_SET_PIC_MODEL") lto_bool_t
  (cg lto_code_gen_t)
  (lto_codegen_model lto_codegen_model))
(defcfun (lto_codegen_set_cpu "LTO_CODEGEN_SET_CPU") :void
  (cg lto_code_gen_t)
  (cpu (:pointer :char)))
(defcfun (lto_codegen_set_assembler_path "LTO_CODEGEN_SET_ASSEMBLER_PATH") :void
  (cg lto_code_gen_t)
  (path (:pointer :char)))
(defcfun (lto_codegen_set_assembler_args "LTO_CODEGEN_SET_ASSEMBLER_ARGS") :void
  (cg lto_code_gen_t)
  (args (:pointer (:pointer :char)))
  (nargs :int))
(defcfun (lto_codegen_add_must_preserve_symbol
          "LTO_CODEGEN_ADD_MUST_PRESERVE_SYMBOL") :void
  (cg lto_code_gen_t)
  (symbol (:pointer :char)))
(defcfun (lto_codegen_write_merged_modules
          "LTO_CODEGEN_WRITE_MERGED_MODULES") lto_bool_t
  (cg lto_code_gen_t)
  (path (:pointer :char)))
(defcfun (lto_codegen_compile "LTO_CODEGEN_COMPILE") (:pointer :void)
  (cg lto_code_gen_t)
  (length (:pointer size_t)))
(defcfun (lto_codegen_compile_to_file "LTO_CODEGEN_COMPILE_TO_FILE") lto_bool_t
  (cg lto_code_gen_t)
  (name (:pointer (:pointer :char))))
(defcfun (lto_codegen_optimize "LTO_CODEGEN_OPTIMIZE") lto_bool_t
  (cg lto_code_gen_t))
(defcfun (lto_codegen_compile_optimized
          "LTO_CODEGEN_COMPILE_OPTIMIZED") (:pointer :void)
  (cg lto_code_gen_t)
  (length (:pointer size_t)))
(defcfun (lto_api_version "LTO_API_VERSION") :unsigned-int)
(defcfun (lto_codegen_debug_options "LTO_CODEGEN_DEBUG_OPTIONS") :void
  (cg lto_code_gen_t)
  (huh?? :pointer)
  #+nil
  (:* (:pointer :char)))
(defcfun (lto_initialize_disassembler "LTO_INITIALIZE_DISASSEMBLER") :void)
(defcfun (lto_codegen_set_should_internalize
          "LTO_CODEGEN_SET_SHOULD_INTERNALIZE") :void
  (cg lto_code_gen_t)
  (|ShouldInternalize| lto_bool_t))
(defcfun (lto_codegen_set_should_embed_uselists
          "LTO_CODEGEN_SET_SHOULD_EMBED_USELISTS") :void
  (cg lto_code_gen_t)
  (|ShouldEmbedUselists| lto_bool_t))

;;;;IPO

;;;include llvm-c/Types.h
(defcfun (-add-argument-promotion-pass
          "LLVMAddArgumentPromotionPass") :void
  (|pm| |LLVMPassManagerRef|))
(defcfun (-add-constant-merge-pass "LLVMAddConstantMergePass") :void
  (|pm| |LLVMPassManagerRef|))
(defcfun (-add-dead-arg-elimination-pass
          "LLVMAddDeadArgEliminationPass") :void
  (|pm| |LLVMPassManagerRef|))
(defcfun (-add-function-attrs-pass "LLVMAddFunctionAttrsPass") :void
  (|pm| |LLVMPassManagerRef|))
(defcfun (-add-function-inlining-pass "LLVMAddFunctionInliningPass") :void
  (|pm| |LLVMPassManagerRef|))
(defcfun (-add-always-inliner-pass "LLVMAddAlwaysInlinerPass") :void
  (|pm| |LLVMPassManagerRef|))
(defcfun (-add-global-d-c-e-pass "LLVMAddGlobalDCEPass") :void
  (|pm| |LLVMPassManagerRef|))
(defcfun (-add-global-optimizer-pass "LLVMAddGlobalOptimizerPass") :void
  (|pm| |LLVMPassManagerRef|))
(defcfun (-add-i-p-constant-propagation-pass
          "LLVMAddIPConstantPropagationPass") :void
  (|pm| |LLVMPassManagerRef|))
(defcfun (-add-prune-e-h-pass "LLVMAddPruneEHPass") :void
  (|pm| |LLVMPassManagerRef|))
(defcfun (-add-i-p-s-c-c-p-pass "LLVMAddIPSCCPPass") :void
  (|pm| |LLVMPassManagerRef|))
(defcfun (-add-internalize-pass "LLVMAddInternalizePass") :void
  (|LLVMPassManagerRef| |LLVMPassManagerRef|)
  (|AllButMain| :unsigned-int))
(defcfun (-add-strip-dead-prototypes-pass
          "LLVMAddStripDeadPrototypesPass") :void
  (|pm| |LLVMPassManagerRef|))
(defcfun (-add-strip-symbols-pass "LLVMAddStripSymbolsPass") :void
  (|pm| |LLVMPassManagerRef|))

;;;;PassManagerBuilder

;;;include llvm-c/Types.h
(defctype |LLVMPassManagerBuilderRef| :pointer)
(defcfun (-pass-manager-builder-create
          "LLVMPassManagerBuilderCreate") |LLVMPassManagerBuilderRef|)
(defcfun (-pass-manager-builder-dispose
          "LLVMPassManagerBuilderDispose") :void
  (|pmb| |LLVMPassManagerBuilderRef|))
(defcfun (-pass-manager-builder-set-opt-level
          "LLVMPassManagerBuilderSetOptLevel") :void
  (|pmb| |LLVMPassManagerBuilderRef|)
  (|OptLevel| :unsigned-int))
(defcfun (-pass-manager-builder-set-size-level
          "LLVMPassManagerBuilderSetSizeLevel") :void
  (|pmb| |LLVMPassManagerBuilderRef|)
  (|SizeLevel| :unsigned-int))
(defcfun (-pass-manager-builder-set-disable-unit-at-a-time
          "LLVMPassManagerBuilderSetDisableUnitAtATime") :void
  (|pmb| |LLVMPassManagerBuilderRef|)
  (|Value| |LLVMBool|))
(defcfun (-pass-manager-builder-set-disable-unroll-loops
          "LLVMPassManagerBuilderSetDisableUnrollLoops") :void
  (|pmb| |LLVMPassManagerBuilderRef|)
  (|Value| |LLVMBool|))
(defcfun (-pass-manager-builder-set-disable-simplify-lib-calls
          "LLVMPassManagerBuilderSetDisableSimplifyLibCalls") :void
  (|pmb| |LLVMPassManagerBuilderRef|)
  (|Value| |LLVMBool|))
(defcfun (-pass-manager-builder-use-inliner-with-threshold
          "LLVMPassManagerBuilderUseInlinerWithThreshold") :void
  (|pmb| |LLVMPassManagerBuilderRef|)
  (|Threshold| :unsigned-int))
(defcfun (-pass-manager-builder-populate-function-pass-manager
          "LLVMPassManagerBuilderPopulateFunctionPassManager") :void
  (|pmb| |LLVMPassManagerBuilderRef|)
  (|pm| |LLVMPassManagerRef|))
(defcfun (-pass-manager-builder-populate-module-pass-manager
          "LLVMPassManagerBuilderPopulateModulePassManager") :void
  (|pmb| |LLVMPassManagerBuilderRef|)
  (|pm| |LLVMPassManagerRef|))
(defcfun (-pass-manager-builder-populate-l-t-o-pass-manager
          "LLVMPassManagerBuilderPopulateLTOPassManager") :void
  (|pmb| |LLVMPassManagerBuilderRef|)
  (|pm| |LLVMPassManagerRef|)
  (|Internalize| |LLVMBool|)
  (|RunInliner| |LLVMBool|))

;;;;Scalar

;;;include llvm-c/Types.h
(defcfun (-add-aggressive-d-c-e-pass "LLVMAddAggressiveDCEPass") :void
  (|pm| |LLVMPassManagerRef|))
(defcfun (-add-bit-tracking-d-c-e-pass "LLVMAddBitTrackingDCEPass") :void
  (|pm| |LLVMPassManagerRef|))
(defcfun (-add-alignment-from-assumptions-pass
          "LLVMAddAlignmentFromAssumptionsPass") :void
  (|pm| |LLVMPassManagerRef|))
(defcfun (-add-c-f-g-simplification-pass
          "LLVMAddCFGSimplificationPass") :void
  (|pm| |LLVMPassManagerRef|))
(defcfun (-add-dead-store-elimination-pass
          "LLVMAddDeadStoreEliminationPass") :void
  (|pm| |LLVMPassManagerRef|))
(defcfun (-add-scalarizer-pass "LLVMAddScalarizerPass") :void
  (|pm| |LLVMPassManagerRef|))
(defcfun (-add-merged-load-store-motion-pass
          "LLVMAddMergedLoadStoreMotionPass") :void
  (|pm| |LLVMPassManagerRef|))
(defcfun (-add-g-v-n-pass "LLVMAddGVNPass") :void
  (|pm| |LLVMPassManagerRef|))
(defcfun (-add-ind-var-simplify-pass "LLVMAddIndVarSimplifyPass") :void
  (|pm| |LLVMPassManagerRef|))
(defcfun (-add-instruction-combining-pass
          "LLVMAddInstructionCombiningPass") :void
  (|pm| |LLVMPassManagerRef|))
(defcfun (-add-jump-threading-pass "LLVMAddJumpThreadingPass") :void
  (|pm| |LLVMPassManagerRef|))
(defcfun (-add-l-i-c-m-pass "LLVMAddLICMPass") :void
  (|pm| |LLVMPassManagerRef|))
(defcfun (-add-loop-deletion-pass "LLVMAddLoopDeletionPass") :void
  (|pm| |LLVMPassManagerRef|))
(defcfun (-add-loop-idiom-pass "LLVMAddLoopIdiomPass") :void
  (|pm| |LLVMPassManagerRef|))
(defcfun (-add-loop-rotate-pass "LLVMAddLoopRotatePass") :void
  (|pm| |LLVMPassManagerRef|))
(defcfun (-add-loop-reroll-pass "LLVMAddLoopRerollPass") :void
  (|pm| |LLVMPassManagerRef|))
(defcfun (-add-loop-unroll-pass "LLVMAddLoopUnrollPass") :void
  (|pm| |LLVMPassManagerRef|))
(defcfun (-add-loop-unswitch-pass "LLVMAddLoopUnswitchPass") :void
  (|pm| |LLVMPassManagerRef|))
(defcfun (-add-mem-cpy-opt-pass "LLVMAddMemCpyOptPass") :void
  (|pm| |LLVMPassManagerRef|))
(defcfun (-add-partially-inline-lib-calls-pass
          "LLVMAddPartiallyInlineLibCallsPass") :void
  (|pm| |LLVMPassManagerRef|))
(defcfun (-add-lower-switch-pass "LLVMAddLowerSwitchPass") :void
  (|pm| |LLVMPassManagerRef|))
(defcfun (-add-promote-memory-to-register-pass
          "LLVMAddPromoteMemoryToRegisterPass") :void
  (|pm| |LLVMPassManagerRef|))
(defcfun (-add-reassociate-pass "LLVMAddReassociatePass") :void
  (|pm| |LLVMPassManagerRef|))
(defcfun (-add-s-c-c-p-pass "LLVMAddSCCPPass") :void
  (|pm| |LLVMPassManagerRef|))
(defcfun (-add-scalar-repl-aggregates-pass
          "LLVMAddScalarReplAggregatesPass") :void
  (|pm| |LLVMPassManagerRef|))
(defcfun (-add-scalar-repl-aggregates-pass-s-s-a
          "LLVMAddScalarReplAggregatesPassSSA") :void
  (|pm| |LLVMPassManagerRef|))
(defcfun (-add-scalar-repl-aggregates-pass-with-threshold
          "LLVMAddScalarReplAggregatesPassWithThreshold") :void
  (|pm| |LLVMPassManagerRef|)
  (|Threshold| :int))
(defcfun (-add-simplify-lib-calls-pass "LLVMAddSimplifyLibCallsPass") :void
  (|pm| |LLVMPassManagerRef|))
(defcfun (-add-tail-call-elimination-pass
          "LLVMAddTailCallEliminationPass") :void
  (|pm| |LLVMPassManagerRef|))
(defcfun (-add-constant-propagation-pass
          "LLVMAddConstantPropagationPass") :void
  (|pm| |LLVMPassManagerRef|))
(defcfun (-add-demote-memory-to-register-pass
          "LLVMAddDemoteMemoryToRegisterPass") :void
  (|pm| |LLVMPassManagerRef|))
(defcfun (-add-verifier-pass "LLVMAddVerifierPass") :void
  (|pm| |LLVMPassManagerRef|))
(defcfun (-add-correlated-value-propagation-pass
          "LLVMAddCorrelatedValuePropagationPass") :void
  (|pm| |LLVMPassManagerRef|))
(defcfun (-add-early-c-s-e-pass "LLVMAddEarlyCSEPass") :void
  (|pm| |LLVMPassManagerRef|))
(defcfun (-add-lower-expect-intrinsic-pass
          "LLVMAddLowerExpectIntrinsicPass") :void
  (|pm| |LLVMPassManagerRef|))
(defcfun (-add-type-based-alias-analysis-pass
          "LLVMAddTypeBasedAliasAnalysisPass") :void
  (|pm| |LLVMPassManagerRef|))
(defcfun (-add-scoped-no-alias-a-a-pass "LLVMAddScopedNoAliasAAPass") :void
  (|pm| |LLVMPassManagerRef|))
(defcfun (-add-basic-alias-analysis-pass
          "LLVMAddBasicAliasAnalysisPass") :void
  (|pm| |LLVMPassManagerRef|))

;;;;Vectorize

;;;include llvm-c/Types.h
(defcfun (-add-b-b-vectorize-pass "LLVMAddBBVectorizePass") :void
  (|pm| |LLVMPassManagerRef|))
(defcfun (-add-loop-vectorize-pass "LLVMAddLoopVectorizePass") :void
  (|pm| |LLVMPassManagerRef|))
(defcfun (-add-s-l-p-vectorize-pass "LLVMAddSLPVectorizePass") :void
  (|pm| |LLVMPassManagerRef|))
