(in-package :llvm)

(defun collapse-prefix (l special-words)
  (unless (null l)
    (multiple-value-bind (newpre skip) (check-prefix l special-words)
      (cons newpre (collapse-prefix (nthcdr skip l) special-words)))))

(defun check-prefix (l special-words)
  (let ((pl (loop for i from (1- (length l)) downto 0
              collect (apply #'concatenate 'simple-string (butlast l i)))))
    (loop for w in special-words
          for p = (position-if #'(lambda (s) (string= s w)) pl)
          when p do (return-from check-prefix (values (nth p pl) (1+ p))))
    (values (first l) 1)))

(defun split-if (test seq &optional (dir :before))
  (remove-if #'(lambda (x) (equal x (subseq seq 0 0)))
             (loop for start fixnum = 0 
                then (if (eq dir :before)
                         stop
                         (the fixnum (1+ (the fixnum stop))))
                while (< start (length seq))
                for stop = (position-if 
                            test seq 
                            :start (if (eq dir :elide)
                                       start
                                       (the fixnum (1+ start))))
                collect (subseq 
                         seq start 
                         (if (and stop (eq dir :after)) 
                             (the fixnum (1+ (the fixnum stop))) 
                             stop))
                while stop)))

(defun translate-camelcase-name (name &key upper-initial-p special-words)
  (declare (ignore upper-initial-p))
  (values (intern (reduce #'(lambda (s1 s2)
                              (concatenate 'simple-string s1 "-" s2))
                          (mapcar #'string-upcase
                                  (collapse-prefix
                                   (split-if (lambda (ch)
                                               (or (upper-case-p ch)
                                                   (digit-char-p ch)))
                                             name)
                                   special-words))))))

;; NOTE: subsequences must come after any word they're contained in
;; FIXME: this should really handle arbitrary mappings:
;;        Ptr->pointer, Get->"", Var->variable, etc.
(let ((special-words '("ABI"
                       "ARM"
                       "CFG"
                       "DCE"
                       "FP80" "FP128"
                       "FP"
                       "GC"
                       "GEP"
                       "GVN"
                       "Int8" "Int16" "Int1" "Int32" "Int64"
                       "JIT"
                       "LICM"
                       "MSIL"
                       "MSP430"
                       "NSW"
                       "NUW"
                       "PIC16"
                       "PowerPC"
                       "SCCP"
                       "SI"
                       "SPU"
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
  (:darwin (:or (:default "libLLVM")
                (:default "libLLVM-3.1")
                (:default "libLLVM-3.1svn")
                (:default "libLLVM-3.0")))
  (:unix (:or "libLLVM.so" "libLLVM.so.1"
              "libLLVM-3.1.so" "libLLVM-3.1.so.1"
              "libLLVM-3.1svn.so" "libLLVM-3.1svn.so.1"
              "libLLVM-3.0.so" "libLLVM-3.0.so.1"))
  (t (:or (:default "libLLVM")
          (:default "libLLVM-3.1")
          (:default "libLLVM-3.1svn")
          (:default "libLLVM-3.0"))))

(use-foreign-library libllvm)
