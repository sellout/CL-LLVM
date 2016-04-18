(in-package :llvm)

(defcfun* "LLVMCreateMemoryBufferWithContentsOfFile" :int
  (path :string)
  (out-mem-buf (:pointer memory-buffer)) (out-message (:pointer :string)))
(defcfun* "LLVMCreateMemoryBufferWithSTDIN" :int
  (out-mem-buf (:pointer memory-buffer)) (out-message (:pointer :string)))
(defun make-memory-buffer (&optional path)
  (with-foreign-objects ((out-mem-buf '(:pointer memory-buffer))
                         (out-message '(:pointer :string)))
    (if (= (if path
             (create-memory-buffer-with-contents-of-file path
                                                         out-mem-buf out-message)
             (create-memory-buffer-with-stdin out-mem-buf out-message))
           0)
      (mem-ref out-mem-buf 'memory-buffer)
      (throw-llvm-error out-message))))
(defcfun* "LLVMDisposeMemoryBuffer" :void (mem-buf memory-buffer))
