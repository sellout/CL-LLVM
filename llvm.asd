(defpackage llvm-system
  (:use #:cl #:asdf))

(in-package :llvm-system)

;;; NOTE: before this will work, you need to have LLVM installed, and need to
;;;       build a shared lib using something like:
#|
g++ -shared -o libLLVM.dylib `llvm-config --ldflags` `llvm-config --libs` \
    -u _LLVMGetGlobalContext -u _LLVMCreateExecutionEngine \
    -u _LLVMLinkInJIT -u _LLVMLinkInInterpreter \
    -u _LLVMAddTargetData -u _LLVMAddInstructionCombiningPass \
    -u _LLVMVerifyFunction
sudo cp libLLVM.dylib /usr/local/lib
|#

(eval-when (:compile-toplevel :load-toplevel :execute)
  (oos 'load-op 'cffi-grovel))

(defsystem llvm
  :description "CFFI bindings to the LLVM libraries."
  :long-description "LLVM is a collection of modular and reusable compiler and
                     toolchain technologies. This library makes it easy (and
                     hopefully intuitive) to use them in Common Lisp."
  :license "undecided"
  :author "Greg Pfeil <greg@technomadic.org>"
  :depends-on (cffi cffi-grovel)
  ;;:pathname "source"
  :components
  ((:file "package")
   (:file "cffi" :depends-on ("package"))
   (:module "core"
            :depends-on ("package" "cffi")
            :components ((cffi-grovel:grovel-file "grovel")
                         (:file "error-handling" :depends-on ("grovel"))
                         (:file "modules" :depends-on ("grovel"))
                         (:file "types" :depends-on ("grovel" "modules"))
                         (:file "values" :depends-on ("grovel" "modules"))
                         (:file "instruction-builders" :depends-on ("grovel"))
                         (:file "module-providers" :depends-on ("grovel"))
                         (:file "memory-buffers" :depends-on ("grovel"))
                         (:file "pass-managers" :depends-on ("grovel"))))
   (:module "" :pathname ""
            :depends-on ("package" "cffi" "core")
            :components ((cffi-grovel:grovel-file "analysis-grovel")
                         (:file "analysis" :depends-on ("analysis-grovel"))
                         (:file "bit-reader")
                         (:file "bit-writer")
                         (:file "execution-engine")
                         (cffi-grovel:grovel-file "target-grovel")
                         (:file "target" :depends-on ("target-grovel"))
                         (:file "scalar-transforms")))))

(defsystem kaleidoscope
  :description "A translation of the language created in the LLVM tutorial."
  :depends-on (llvm)
  :components ((:file "kaleidoscope")))
