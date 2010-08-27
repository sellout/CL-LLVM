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
                       "SCCP"
                       "SI"
                       "STDIN"
                       "UI"
                       "VA"
                       "X86")))
  (defmethod cffi:lisp-name
             ((spec string) (package (eql (find-package :llvm))) &optional varp)
    "LLVMUpperCamelCase -> 'llvm:upper-camel-case"
    (intern (format nil (if varp "*~a*" "~a")
                    (translate-camelcase-name (subseq spec 4)
                                              :upper-initial-p t
                                              :special-words special-words))))
  (defmethod cffi:foreign-name
             ((spec symbol) (package (eql (find-package :llvm))) &optional varp)
    "'llvm:lisp-name -> LLVMLispName"
    (let ((name (translate-camelcase-name spec
                                          :upper-initial-p t
                                          :special-words special-words)))
      (concatenate 'string
                   (package-name (symbol-package spec))
                   (if varp
                     (subseq name 1 (1- (length name)))
                     name)))))

(define-foreign-library libllvm
  (t (:default "libLLVM")))

(use-foreign-library libllvm)
