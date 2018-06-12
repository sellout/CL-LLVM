(in-package :llvm)
;;;;from "llvm-c/Core.h"

(defcenum opcode
  ;;terminator instructions
  (:ret 1) ;"LLVMRet" 
  (:br) ;"LLVMBr" 
  (:switch) ;"LLVMSwitch" 
  (:indirect-br) ;"LLVMIndirectBr" 
  (:invoke) ;"LLVMInvoke"
  ;;removed 6 due to api changes
  (:unreachable 7) ;"LLVMUnreachable"
  
  ;;standard binary operators
  (:add) ;"LLVMAdd" 
  (:f-add) ;"LLVMFAdd" 
  (:sub) ;"LLVMSub" 
  (:f-sub) ;"LLVMFSub" 
  (:mul) ;"LLVMMul" 
  (:f-mul) ;"LLVMFMul" 
  (:u-div) ;"LLVMUDiv" 
  (:s-div) ;"LLVMSDiv" 
  (:f-div) ;"LLVMFDiv" 
  (:u-rem) ;"LLVMURem" 
  (:s-rem) ;"LLVMSRem" 
  (:f-rem) ;"LLVMFRem"

  ;;logical operators
  (:shl 20) ;"LLVMShl" 
  (:shr) ;"LLVMLShr" 
  (:a-shr) ;"LLVMAShr" 
  (:and) ;"LLVMAnd" 
  (:or) ;"LLVMOr" 
  (:xor) ;"LLVMXor"

  ;;memory operators
  (:alloca 26) ;"LLVMAlloca" 
  (:load) ;"LLVMLoad" 
  (:store) ;"LLVMStore" 
  (:get-element-ptr) ;"LLVMGetElementPtr"

  ;;cast operators
  (:trunc 30) ;"LLVMTrunc" 
  (:z-ext) ;"LLVMZExt" 
  (:s-ext) ;"LLVMSExt" 
  (:fp-to-ui) ;"LLVMFPToUI" 
  (:fp-to-si) ;"LLVMFPToSI" 
  (:ui-to-fp) ;"LLVMUIToFP" 
  (:si-to-fp) ;"LLVMSIToFP" 
  (:fp-trunc) ;"LLVMFPTrunc" 
  (:fp-ext) ;"LLVMFPExt" 
  (:ptr-to-int) ;"LLVMPtrToInt" 
  (:int-to-ptr) ;"LLVMIntToPtr" 
  (:bit-cast) ;"LLVMBitCast"
  ;;;does not exist on 3.0 is "LLVMUnwind" under exception handling instead
  (:addr-space-cast 60) ;"LLVMAddrSpaceCast"

  ;;other operators
  (:i-cmp 42) ;"LLVMICmp" 
  (:f-cmp) ;"LLVMFCmp" 
  (:phi) ;"LLVMPHI" 
  (:call) ;"LLVMCall" 
  (:select) ;"LLVMSelect"
  ;;[why these missing?
  (:user-op-1) ;"LLVMUserOp1"
  (:user-op-2) ;"LLVMUserOp2"
  ;;] 
  (:va-arg 49) ;"LLVMVAArg" 
  (:extract-element) ;"LLVMExtractElement" 
  (:insert-element) ;"LLVMInsertElement" 
  (:shuffle-vector) ;"LLVMShuffleVector" 
  (:extract-value) ;"LLVMExtractValue" 
  (:insert-value) ;"LLVMInsertValue"

  ;;atomic operators
  (:fence 55) ;"LLVMFence" 
  (:atomic-cas) ;"LLVMAtomicCmpXchg"
  (:atomic-rmw) ;"LLVMAtomicRMw

  ;;execption handling
  (:resume 58) ;"LLVMResume"
  (:landing-pad 59) ;"LLVMLandingPad"
  ;;do not exist at 3.0
  (:cleanup-ret 61) ;"LLVMCleanupRet"
  (:catch-ret) ;"LLVMCatchRet"
  (:catch-pad) ;" LLVMCatchPad"
  (:cleanup-pad) ;"LLVMCleanupPad"
  (:catch-switch) ;"LLVMCatchSwitch"
  )

(defcenum type-kind
  (:void) ;"LLVMVoidTypeKind"
  ;;not in 3.0
  (:half) ;"LLVMHalfTypeKind" 
  (:float) ;"LLVMFloatTypeKind" 
  (:double) ;"LLVMDoubleTypeKind" 
  (:x86-fp80) ;"LLVMX86_FP80TypeKind" 
  (:fp128) ;"LLVMFP128TypeKind" 
  (:ppc-fp128) ;"LLVMPPC_FP128TypeKind" 
  (:label) ;"LLVMLabelTypeKind" 
  (:integer) ;"LLVMIntegerTypeKind" 
  (:function) ;"LLVMFunctionTypeKind" 
  (:struct) ;"LLVMStructTypeKind" 
  (:array) ;"LLVMArrayTypeKind" 
  (:pointer) ;"LLVMPointerTypeKind" 
  (:vector) ;"LLVMVectorTypeKind" 
  (:metadata) ;"LLVMMetadataTypeKind" 
  (:x86-mmx) ;"LLVMX86_MMXTypeKind"
  ;;not in 3.0
  (:token) ;" LLVMTokenTypeKind"  
  )

(defcenum linkage
  (:external) ;"LLVMExternalLinkage" 
  (:available-externally) ;"LLVMAvailableExternallyLinkage" 
  (:link-once-any) ;"LLVMLinkOnceAnyLinkage" 
  (:link-once-odr) ;"LLVMLinkOnceODRLinkage"
  ;;not in 3.0
  (:link-once-odr-auto-hide-linkage)
  (:weak-any) ;"LLVMWeakAnyLinkage" 
  (:weak-odr) ;"LLVMWeakODRLinkage" 
  (:appending) ;"LLVMAppendingLinkage" 
  (:internal) ;"LLVMInternalLinkage" 
  (:private) ;"LLVMPrivateLinkage" 
  (:dll-import) ;"LLVMDLLImportLinkage" 
  (:dll-export) ;"LLVMDLLExportLinkage" 
  (:external-weak) ;"LLVMExternalWeakLinkage" 
  (:ghost) ;"LLVMGhostLinkage" 
  (:common) ;"LLVMCommonLinkage" 
  (:linker-private) ;"LLVMLinkerPrivateLinkage" 
  (:linker-private-weak) ;"LLVMLinkerPrivateWeakLinkage"
  ;;:LIBLLVM-UPPER-3.4.0
  #+nil 
  (:linker-private-weak-auto) ;"LLVMLinkerPrivateWeakDefAutoLinkage"
  )

(defcenum visibility
  (:default) ;"LLVMDefaultVisibility" 
  (:hidden) ;"LLVMHiddenVisibility" 
  (:protected) ;"LLVMProtectedVisibility")

(defcenum calling-convention
  (:c 0) ;"LLVMCCallConv" 
  (:fast 8) ;"LLVMFastCallConv" 
  (:cold 9) ;"LLVMColdCallConv" 
  (:x86-stdcall 64) ;"LLVMX86StdcallCallConv" 
  (:x86-fastcall 65) ;"LLVMX86FastcallCallConv"
  )

(defcenum int-predicate
  (:= 32) ;"LLVMIntEQ" 
  (:/=) ;"LLVMIntNE" 
  (:unsigned->) ;"LLVMIntUGT" 
  (:unsigned->=) ;"LLVMIntUGE" 
  (:unsigned-<) ;"LLVMIntULT" 
  (:unsigned-<=) ;"LLVMIntULE" 
  (:>) ;"LLVMIntSGT" 
  (:>=) ;"LLVMIntSGE" 
  (:<) ;"LLVMIntSLT" 
  (:<=) ;"LLVMIntSLE"
  )

(defcenum real-predicate
  (:false) ;"LLVMRealPredicateFalse" 
  (:=) ;"LLVMRealOEQ" 
  (:>) ;"LLVMRealOGT" 
  (:>=) ;"LLVMRealOGE" 
  (:<) ;"LLVMRealOLT" 
  (:<=) ;"LLVMRealOLE" 
  (:/=) ;"LLVMRealONE" 
  (:ordered) ;"LLVMRealORD" 
  (:unordered) ;"LLVMRealUNO" 
  (:unordered-=) ;"LLVMRealUEQ" 
  (:unordered->) ;"LLVMRealUGT" 
  (:unordered->=) ;"LLVMRealUGE" 
  (:unordered-<) ;"LLVMRealULT" 
  (:unordered-<=) ;"LLVMRealULE" 
  (:unordered-/=) ;"LLVMRealUNE" 
  (:true) ;"LLVMRealPredicateTrue"
  )

(defcenum landing-pad-clause-type
  (:catch) ;"LLVMLandingPadCatch" 
  (:filter) ;"LLVMLandingPadFilter"
  )
