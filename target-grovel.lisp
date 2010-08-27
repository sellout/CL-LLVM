(in-package :llvm)

(cc-flags "-D__STDC_LIMIT_MACROS"
          "-D__STDC_CONSTANT_MACROS")
(include "llvm-c/Target.h")

(cenum byte-ordering
       ((:big-endian "LLVMBigEndian"))
       ((:little-endian "LLVMLittleEndian")))
