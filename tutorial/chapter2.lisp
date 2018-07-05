(in-package :kaleidoscope.chapter2)

;;; driver

(defun toplevel ()
  (with-chapter 2
    (reset-token-reader)
    (get-next-token)
    (set-binop-precedence)
    (format *output?* "~&ready> ")
    (callcc (function main-loop)))
  (values))
