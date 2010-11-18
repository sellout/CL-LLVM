(in-package :llvm)

(defcfun* "LLVMAddAggressiveDCEPass" :void (pm pass-manager))

(defcfun* "LLVMAddCFGSimplificationPass" :void (pm pass-manager))

(defcfun* "LLVMAddCondPropagationPass" :void (pm pass-manager))

(defcfun* "LLVMAddDeadStoreEliminationPass" :void (pm pass-manager))

(defcfun* "LLVMAddGVNPass" :void (pm pass-manager))

(defcfun (add-independent-variable-simplification-pass
          "LLVMAddIndVarSimplifyPass")
         :void
  (pm pass-manager))

(defcfun* "LLVMAddInstructionCombiningPass" :void (pm pass-manager))

(defcfun* "LLVMAddJumpThreadingPass" :void (pm pass-manager))

(defcfun* "LLVMAddLICMPass" :void (pm pass-manager))

(defcfun* "LLVMAddLoopDeletionPass" :void (pm pass-manager))

(defcfun* "LLVMAddLoopIndexSplitPass" :void (pm pass-manager))

(defcfun* "LLVMAddLoopRotateEPass" :void (pm pass-manager))

(defcfun* "LLVMAddLoopUnrollPass" :void (pm pass-manager))

(defcfun* "LLVMAddLoopUnswitchPass" :void (pm pass-manager))

(defcfun* "LLVMAddMemCpyOptPass" :void (pm pass-manager))

(defcfun* "LLVMAddPromoteMemoryToRegisterPass" :void (pm pass-manager))

(defcfun* "LLVMAddReassociatePass" :void (pm pass-manager))

(defcfun* "LLVMAddSCCPPass" :void (pm pass-manager))

(defcfun* "LLVMAddScalarReplAggregatesPass" :void (pm pass-manager))

(defcfun* "LLVMAddSimplifyLibCallsPass" :void (pm pass-manager))

(defcfun* "LLVMAddTailCallEliminationPass" :void (pm pass-manager))

(defcfun* "LLVMAddConstantPropagationPass" :void (pm pass-manager))

(defcfun* "LLVMAddDemoteMemoryToRegisterPass" :void (pm pass-manager))
