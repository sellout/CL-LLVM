(asdf:defsystem #:llvm256
  :description "A translation of the language created in the LLVM tutorial."
  :depends-on
  (#:cffi)
  :components
  ((:module
    "src"
    :components
    ((:file "c-bindings")))))
