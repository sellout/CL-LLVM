(in-package :llvm)

(defcfun (%write-bitcode-to-file-handle "LLVMWriteBitcodeToFileHandle") :boolean
  (m module) (handle :int))
(defun write-bitcode-to-file-handle (m handle)
  (if (%write-bitcode-to-file-handle m handle)
    (error "Failed writing bitcode to file handle.")))

(defcfun (%write-bitcode-to-file "LLVMWriteBitcodeToFile") :boolean
  (m module) (path :string))
(defun write-bitcode-to-file (m path)
  (if (%write-bitcode-to-file m path)
    (error "Failed writing bitcode to file.")))
