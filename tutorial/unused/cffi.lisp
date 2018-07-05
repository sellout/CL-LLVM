;;wtf? -- terminal625 june 11 2018

;;; This just makes sure the library gets loaded for use by LLVM

(progn
  (cffi:define-foreign-library libkaleidoscope
    (t (:default #.(namestring
		    (asdf:system-relative-pathname :kaleidoscope256
						   "tutorial/.libs/libkaleidoscope")))))

  (cffi:use-foreign-library libkaleidoscope))
