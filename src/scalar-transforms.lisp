(in-package :llvm)

(defmacro define-legacy-pass (name)
  `(defun ,name (pass-manager)
     "Compatibility no-op for a legacy LLVM scalar-pass-manager entry point.

LLVM 23 removed the LLVMAdd*Pass C API.  Use LLVMRunPasses and a textual
pipeline for new code."
     (declare (ignore pass-manager))
     nil))

(define-legacy-pass add-aggressive-dce-pass)
(define-legacy-pass add-cfg-simplification-pass)
(define-legacy-pass add-cond-propagation-pass)
(define-legacy-pass add-dead-store-elimination-pass)
(define-legacy-pass add-gvn-pass)
(define-legacy-pass add-independent-variable-simplification-pass)
(define-legacy-pass add-instruction-combining-pass)
(define-legacy-pass add-jump-threading-pass)
(define-legacy-pass add-licm-pass)
(define-legacy-pass add-loop-deletion-pass)
(define-legacy-pass add-loop-index-split-pass)
(define-legacy-pass add-loop-rotate-e-pass)
(define-legacy-pass add-loop-unroll-pass)
(define-legacy-pass add-loop-unswitch-pass)
(define-legacy-pass add-mem-cpy-opt-pass)
(define-legacy-pass add-promote-memory-to-register-pass)
(define-legacy-pass add-reassociate-pass)
(define-legacy-pass add-sccp-pass)
(define-legacy-pass add-scalar-repl-aggregates-pass)
(define-legacy-pass add-simplify-lib-calls-pass)
(define-legacy-pass add-tail-call-elimination-pass)
(define-legacy-pass add-constant-propagation-pass)
(define-legacy-pass add-demote-memory-to-register-pass)
