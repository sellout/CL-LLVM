(in-package :kaleidoscope.chapter7)

;;; driver

;(defvar *myjit*)

(defun toplevel ()
 ; (llvm::initialize-native-target?)
 ; (llvm::initialize-native-Asm-parser)
 ; (llvm::initialize-native-asm-printer)
  (llvm:with-objects ((*builder* llvm:builder)
		      (*module* llvm:module "my cool jit")
		      (*execution-engine* llvm:execution-engine *module*)
		      ;(*myjit* llvm:jit-compiler *module*)
		      (*fpm* llvm:function-pass-manager *module*))    
    (llvm:add-target-data (llvm:target-data *execution-engine*) *fpm*)
    ;;passes    
    (progn
      (llvm:add-promote-memory-to-register-pass *fpm*)
      (llvm:add-instruction-combining-pass *fpm*)
      (llvm:add-reassociate-pass *fpm*)
      (llvm:add-gvn-pass *fpm*)
      (llvm:add-cfg-simplification-pass *fpm*))

    (llvm:initialize-function-pass-manager *fpm*)
     
    (with-chapter 7
      (reset-token-reader)
      (get-next-token)
      (set-binop-precedence)
      (format *output?* "~&ready> ")
      (callcc (function main-loop)))
    (dump-module *module*))
  (values))
