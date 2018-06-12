(defpackage llvm-system
  (:use #:cl #:asdf))

(in-package :llvm-system)

;;; NOTE: before this will work, you need to have LLVM installed (and don't
;;;       forget to build the shared lib with --enable-shared)

;#+nil
(eval-when (:compile-toplevel :load-toplevel :execute)
  (mapc #+quicklisp #'ql:quickload #-quicklisp #'asdf:load-system
        '(cffi-grovel)))

(defsystem llvm256
  :description "CFFI bindings to the LLVM libraries."
  :long-description "LLVM is a collection of modular and reusable compiler and
                     toolchain technologies. This library makes it easy (and
                     hopefully intuitive) to use them in Common Lisp."
  :license "MIT"
  :author "Greg Pfeil <greg@technomadic.org>"
  :depends-on (cffi cffi-grovel trivial-features cl-ppcre split-sequence trivial-shell)
  :pathname "src/"
  :components
  ((:file "package")
   (:file "cffi" :depends-on ("package"))
   #+nil
   (:module "grovel"
	    :components
	    ((cffi-grovel:grovel-file "analysis-grovel")
	     (cffi-grovel:grovel-file "target-grovel")))
   (:module "core"
            :depends-on ("package" "cffi")
            :components
	    (;#+nil
	     (cffi-grovel:grovel-file "grovel")
	     (:file "error-handling" :depends-on ("grovel"))
	     (:file "modules"
		    :depends-on ("grovel" "error-handling"))
	     (:file "types"
		    :depends-on ("grovel" "modules" "error-handling"))
	     (:file "values"
		    :depends-on ("grovel" "modules" "error-handling"))
	     (:file "instruction-builders"
		    :depends-on ("grovel" "error-handling"))
	     (:file "memory-buffers"
		    :depends-on ("grovel" "error-handling"))
	     (:file "pass-managers"
		    :depends-on ("grovel" "error-handling"))))
   (:module "" :pathname ""
            :depends-on ("package" "cffi" "core")
            :components
	    ((:file "analysis-header")
	     (:file "analysis" :depends-on ("analysis-header"))
	     (:file "bit-reader")
	     (:file "bit-writer")
	     (:file "execution-engine")
	     (:file "target-header")	     
	     (:file "target" :depends-on ("target-header"))
	     (:file "scalar-transforms")))))

;;; NOTE: In order to load and run the Kaleidoscope tutorial, you first need to
;;;       run `./build-library.sh` in the tutorial subdirectory.

(defsystem kaleidoscope256
    :description "A translation of the language created in the LLVM tutorial."
    :depends-on (llvm256)
    :pathname "tutorial/"
    :components
    (#+nil
     (:file "cffi")
     (:file "chapter2")
     (:file "chapter3")
     (:file "chapter4")
     (:file "chapter5")
     (:file "chapter6")
     (:file "chapter7")))
