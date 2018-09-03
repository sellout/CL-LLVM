#+nil
(defpackage llvm-system
  (:use #:cl #:asdf))

#+nil
(in-package :llvm-system)

;;; NOTE: before this will work, you need to have LLVM installed (and don't
;;;       forget to build the shared lib with --enable-shared)

#+nil
(eval-when (:compile-toplevel :load-toplevel :execute)
  (mapc #+quicklisp #'ql:quickload #-quicklisp #'asdf:load-system
        '(cffi-grovel)))

(asdf:defsystem #:llvm256
  :description "CFFI bindings to the LLVM libraries."
  :long-description "LLVM is a collection of modular and reusable compiler and
                     toolchain technologies. This library makes it easy (and
                     hopefully intuitive) to use them in Common Lisp."
  :license "MIT"
  :author "Greg Pfeil <greg@technomadic.org>"
  :depends-on
  (#:cffi
   #:cffi-grovel
   #:trivial-features
   #:cl-ppcre
   #:split-sequence
   ;;trivial-shell
   )
  :serial t
  :components
  ((:module
    "source"
    :components
    ((:file "package")
     (:file "cffi" :depends-on ("package"))
     #+nil
     (:module "grovel"
	      :components
	      ((cffi-grovel:grovel-file "analysis-grovel")
	       (cffi-grovel:grovel-file "target-grovel")
	       (cffi-grovel:grovel-file "core-grovel")))
     (:module "header"
	      :depends-on ("package")
	      :components
	      ((:file "analysis-header")
	       (:file "target-header")
	       (:file "core-header")))
     (:module "core"
	      :depends-on ("package" "cffi" "header")
	      :components ;;all depend on "core-grovel"
	      ((:file "error-handling")
	       (:file "modules"
		      :depends-on ("error-handling"))
	       (:file "types"
		      :depends-on ("modules" "error-handling"))
	       (:file "values"
		      :depends-on ("modules" "error-handling"))
	       (:file "instruction-builders"
		      :depends-on ("error-handling"))
	       (:file "memory-buffers"
		      :depends-on ("error-handling"))
	       (:file "pass-managers"
		      :depends-on ("error-handling"))))
     (:module "src"
	      :depends-on ("package" "cffi" "core" "header")
	      :components
	      ((:file "analysis") ;;depends-on ("analysis-header")
	       (:file "bit-reader")
	       (:file "bit-writer")
	       (:file "execution-engine")	     
	       (:file "target") ;;depends-on ("target-header")
	       (:file "scalar-transforms")))))))
