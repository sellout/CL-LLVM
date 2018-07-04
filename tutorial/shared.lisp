(defpackage #:k-shared
  (:use
   #:cl
   #:k-lexer)
  (:export
   #:*output?*
   #:*input?*)
  (:export
   #:callcc))
(in-package #:k-shared)

(defparameter *output?* nil)
(defparameter *input?* nil)

(defun callcc (fun)
  (block nil
    (funcall
     fun
     (lambda ()
       (return-from nil)))))
