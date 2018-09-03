(in-package :llvm)
;;from "llvm-c/Analysis.h"

(defcenum (verifier-failure-action)
  :abort-process ;"LLVMAbortProcessAction"
  :print-message ;"LLVMPrintMessageAction"
  :return-status ;"LLVMReturnStatusAction"
  )
