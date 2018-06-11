;;wtf?

;;; This just makes sure the library gets loaded for use by LLVM
#+nil
(progn
  (cffi:define-foreign-library libkaleidoscope
    (t (:default #.(namestring
		    (asdf:system-relative-pathname :kaleidoscope256
						   "tutorial/.libs/libkaleidoscope")))))

  (cffi:use-foreign-library libkaleidoscope))
