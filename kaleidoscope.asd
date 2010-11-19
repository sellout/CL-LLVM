(defpackage kaleidoscope-system
  (:use #:cl #:asdf))

(in-package :kaleidoscope-system)

(defsystem kaleidoscope
  :description "A translation of the language created in the LLVM tutorial."
  :depends-on (llvm)
  :pathname "tutorial/"
  :components ((:file "chapter2")
               (:file "chapter3")
               (:file "chapter4")
               (:file "chapter5")
               (:file "chapter6")
               (:file "chapter7")))