(defpackage #:llvm
  (:use #:cl))

(cffi:load-foreign-library
 #+nil
 "/home/imac/install/llvm/6.0.0/build/lib/libLLVM-6.0.so"
; #+nil
 "/home/imac/install/llvm/3.8.0/build/lib/libLLVM-3.8.so"
 )
(cffi:load-foreign-library
 #+nil
 "/home/imac/install/llvm/6.0.0/llvm-clang-samples-llvm6.0/build/crap.so"
; #+nil
 "/home/imac/install/llvm/3.8.0/llvm-clang-samples-llvm3.8/build/crap.so"
 )
