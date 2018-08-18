;;; NOTE: In order to load and run the Kaleidoscope tutorial, you first need to
;;;       run make in the tutorial subdirectory to build the c library for chap 5 and 6.

(asdf:defsystem #:kaleidoscope256
  :description "A translation of the language created in the LLVM tutorial."
  :depends-on
  (#:cffi
   #:utility
   #:filesystem-util)
  :components
  ((:module
    "tutorial"
    :serial t
    :components
    ((:file "package")
     (:file "c-bindings")
     (:file "shared")
     (:file "chapter")
     (:file "testcases")))))
