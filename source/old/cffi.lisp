(define-foreign-library libllvm
  (:darwin (:or (:default "libLLVM")
                (:default "libLLVM-3.6")
                (:default "libLLVM-3.5")
                (:default "libLLVM-3.1")
                (:default "libLLVM-3.1svn")
                (:default "libLLVM-3.0")))
  (:unix (:or "libLLVM.so"
	      "libLLVM.so.1"
	      "libLLVM-3.6.so"
              "libLLVM-3.1.so"
	      "libLLVM-3.1.so.1"
	      "libLLVM-3.1svn.so"
              "libLLVM-3.1svn.so.1"
	      "libLLVM-3.0.so"
	      "libLLVM-3.0.so.1"))
  (t (:or (:default "libLLVM")
          (:default "libLLVM-3.6")
          (:default "libLLVM-3.1")
          (:default "libLLVM-3.1svn")
          (:default "libLLVM-3.0"))))


;;;required for the old core-grovel.lisp
(flet ((parse-version (version)
          (let ((splitted (split-sequence:split-sequence #\. version)))
            (when (and (>= (length splitted) 2)
                       (>= (parse-integer (car splitted)) 3)
                       (>= (parse-integer (cadr splitted)) 4))
              (push :libllvm-upper-3.4.0 *features*)))))
  (multiple-value-bind (version err) (trivial-shell:shell-command "llvm-config --version")
    (if (zerop (length err))
        (progn
          (push :llvm-config *features*)
          (parse-version (string-trim '(#\Newline) version)))
        (cl-ppcre:register-groups-bind (version)
            ("libLLVM-(([0-9]+\\.?)+)" (pathname-name (cffi::foreign-library-handle (cffi::get-foreign-library 'libllvm))))
          (when version
            (parse-version version))))))
