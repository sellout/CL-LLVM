(in-package :llvm)

(defmacro with-object ((var class &rest args) &body body)
  (let ((class-name (gensym "CLASS")))
    `(let* ((,class-name ,class)
            (,var (make-instance ,class-name ,@args)))
       (unwind-protect (progn ,@body)
         (funcall (case ,class-name
                    (context #'dispose-context)
                    ;; FIXME: dispose-module causes SIGBUS
                    (module #'identity) ;#'dispose-module)
                    (type-handle #'dispose-type-handle)
                    (builder #'dispose-builder)
                    (memory-buffer #'dispose-memory-buffer)
                    ((pass-manager function-pass-manager)
                     #'dispose-pass-manager)
                    (target-data #'dispose-target-data)
                    ((generic-value-of-int
                      generic-value-of-pointer
                      generic-value-of-float)
                     #'dispose-generic-value)
                    ((execution-engine interpreter jit-compiler)
                     #'dispose-execution-engine))
                  ,var)))))

(defmacro with-objects ((&rest bindings) &body body)
  (if (endp bindings)
    `(progn ,@body)
    `(with-object ,(car bindings)
       (with-objects ,(cdr bindings)
         ,@body))))

(defcfun* "LLVMDisposeMessage" :void (message (:pointer :char)))

(define-condition llvm-error (error)
  ((message-string :reader message))
  (:report (lambda (condition stream)
             (write-string (message condition) stream))))

(defmethod initialize-instance :after ((object llvm-error)
                                       &key message &allow-other-keys)
  "This cleans up the memory used by the C string passed as the MESSAGE
   parameter."
  (setf (slot-value object 'message-string) (mem-ref message :string))
  (dispose-message (mem-ref message '(:pointer :char))))

(define-condition required-parameter-error (error)
  ((parameter-name :initarg :name :reader name))
  (:report (lambda (condition stream)
             (format stream "~a is a required parameter."
                     (name condition)))))

;;; Core
(defctype context :pointer) ; "LLVMContextRef")
(defctype module :pointer) ; "LLVMModuleRef")
(defctype type :pointer) ; "LLVMTypeRef")
(defctype type-handle :pointer) ; "LLVMTypeHandleRef")
(defctype value :pointer) ; "LLVMValueRef")
(defctype basic-block :pointer) ; "LLVMBasicBlockRef")
(defctype builder :pointer) ; "LLVMBuilderRef")
(defctype memory-buffer :pointer) ; "LLVMMemoryBufferRef")
(defctype pass-manager :pointer) ; "LLVMPassManagerRef")
;;; Target
(defctype target-data :pointer) ; "LLVMTargetDataRef")
(defctype struct-layout :pointer) ; "LLVMStructLayoutRef")
;;; ExecutionEngine
(defctype generic-value :pointer) ; "LLVMGenericValueRef")
(defctype execution-engine :pointer) ; "LLVMExecutionEngineRef")

;;; numbers should be converted where necessary

(define-foreign-type real-double ()
  ()
  (:actual-type :double))

(define-parse-method real-double ()
  (make-instance 'real-double))

(defmethod translate-to-foreign (object (type real-double))
  (coerce object 'double-float))

;;; need to convert arrays properly

(define-foreign-type carray ()
  ((value-type :initarg :value-type :reader value-type)
   (capacity :initform nil :initarg :capacity :reader capacity))
  (:actual-type :pointer))

(define-parse-method carray (value-type &optional capacity)
  (make-instance 'carray :value-type value-type :capacity capacity))

(defmethod translate-to-foreign (object (type carray))
  (if (and (capacity type) (/= (capacity type) (length object)))
    (error "Not the correct array length")
    (let ((array (coerce object 'array)))
      (funcall (first (waaf-cffi::%get-transformation-function-pair (array-element-type array)
                                                                    (value-type type)
                                                                    nil))
               array nil nil t))))

(defmethod translate-from-foreign (pointer (type carray))
  (let* ((length (or (capacity type)
                     (loop for i from 0
                       while (not (null-pointer-p (mem-aref pointer (value-type type)
                                                            i)))
                       finally (return i))))
         (array (make-array length)))
    (funcall (second (waaf-cffi::%get-transformation-function-pair t
                                                                   (value-type type)
                                                                   nil))
             array pointer 0 length)
    array))

(defmethod free-translated-object (value (type carray) param)
  (declare (ignore param))
  (foreign-free value))

;;; optimization-level should be a cenum, but only exists in C++.

(let ((opt-level '((:none . 0)
                   (:less . 1)
                   (:default . 2)
                   (:aggressive . 3))))
  (define-foreign-type optimization-level ()
    ()
    (:actual-type :unsigned-int))

  (define-parse-method optimization-level ()
    (make-instance 'optimization-level))

  (defmethod translate-to-foreign (object (type optimization-level))
    (cdr (assoc object opt-level)))

  (defmethod translate-from-foreign (object (type optimization-level))
    (car (rassoc object opt-level))))
