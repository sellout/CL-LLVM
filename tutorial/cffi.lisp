;;; This just makes sure the library gets loaded for use by LLVM

(cffi:define-foreign-library libkaleidoscope
  (t (:default "tutorial/.libs/libkaleidoscope")))

(cffi:use-foreign-library libkaleidoscope)
