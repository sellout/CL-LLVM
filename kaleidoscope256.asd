;;; NOTE: In order to load and run the Kaleidoscope tutorial, you first need to
;;;       run `./build-library.sh` in the tutorial subdirectory.

(asdf:defsystem #:kaleidoscope256
  :description "A translation of the language created in the LLVM tutorial."
  :depends-on
  (#:llvm256
   #:filesystem-util
   #:split-sequence)
  :components
  ((:module "tutorial"
	     :serial t
	     :components
	     (#+nil
	      (:file "cffi")
	      (:file "lexer")
	      (:file "chapter2")
	      (:file "chapter3")
	      (:file "chapter4")
	      (:file "chapter5")
	      (:file "chapter6")
	      (:file "chapter7")
	      (:file "test")))))
