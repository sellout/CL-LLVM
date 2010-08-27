(in-package :llvm)

(cc-flags "-D__STDC_LIMIT_MACROS"
          "-D__STDC_CONSTANT_MACROS")
(include "llvm-c/Analysis.h")

(cenum verifier-failure-action
       ((:abort-process "LLVMAbortProcessAction"))
       ((:print-message "LLVMPrintMessageAction"))
       ((:return-status "LLVMReturnStatusAction")))
