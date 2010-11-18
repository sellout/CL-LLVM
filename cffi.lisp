(in-package :llvm)

;; NOTE: subsequences must come after any word they're contained in
;; FIXME: this should really handle arbitrary mappings:
;;        Ptr->pointer, Get->"", Var->variable, etc.
(let ((special-words '("ABI"
                       "CFG"
                       "DCE"
                       "FP80" "FP128"
                       "FP"
                       "GC"
                       "GVN"
                       "Int8" "Int16" "Int1" "Int32" "Int64"
                       "JIT"
                       "LICM"
                       "NSW"
                       "NUW"
                       "SCCP"
                       "SI"
                       "STDIN"
                       "UI"
                       "VA"
                       "X86")))
  (defmacro defcfun* (foreign-name return-type &body arguments)
    "A specialized version of DEFCFUN than auto-converts LLVM that fit a certain
     pattern."
    `(defcfun (,(translate-camelcase-name (subseq foreign-name 4)
                                          :upper-initial-p t
                                          :special-words special-words)
               ,foreign-name)
         ,return-type ,@arguments)))

(define-foreign-library libllvm
  (t (:default "libLLVM")))

(use-foreign-library libllvm)
