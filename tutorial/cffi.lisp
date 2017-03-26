;;; This just makes sure the library gets loaded for use by LLVM

(cffi:define-foreign-library libkaleidoscope
  (t (:default #.(namestring
                  (asdf:system-relative-pathname :kaleidoscope
                                                 "tutorial/.libs/libkaleidoscope")))))

(cffi:use-foreign-library libkaleidoscope)
