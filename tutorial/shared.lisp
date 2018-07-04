(defpackage #:k-shared
  (:use
   #:cl)
  (:export
   #:*output?*
   #:*input?*)
  (:export
   #:callcc)
  (:export
   #:dump-value
   #:dump-module))
(in-package #:k-shared)

(defparameter *output?* (make-synonym-stream '*standard-output*))
(defparameter *input?* (make-synonym-stream '*standard-input*))

(defun callcc (fun)
  (block nil
    (funcall
     fun
     (lambda (&rest values)
       (return-from nil (apply (function values)
			       values))))))

(defun dump-value (value)
  (write-string (llvm:print-value-to-string value) *output?*))

(defun dump-module (module)
  (write-string (llvm:print-module-to-string module) *output?*))
