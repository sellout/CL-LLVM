(in-package :llvm)
;;;;from "llvm-c/Target.h"

(defcenum (byte-ordering)
  :big-endian ;"LLVMBigEndian"
  :little-endian ;"LLVMLittleEndian"
  )
