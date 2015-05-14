(in-package :llvm)

(defparameter *should-dispose-modules* nil)

(defmacro with-object ((var type &rest args) &body body)
  (let ((managers
          ;; Each element is of the form (designator constructor destructor)
          '((module make-module dispose-module)
            (builder make-builder dispose-builder)
            (memory-buffer make-memory-buffer dispose-memory-buffer)
            (type-handle create-type-handle dispose-type-handle)
            (context context-create dispose-context)
            (pass-manager create-pass-manager dispose-pass-manager)
            (function-pass-manager create-function-pass-manager-for-module dispose-pass-manager)
            (target-data create-target-data dispose-target-data)
            (generic-value-of-int create-generic-value-of-int dispose-generic-value)
            (generic-value-of-pointer create-generic-value-of-pointer dispose-generic-value)
            (generic-value-of-float create-generic-value-of-float dispose-generic-value)
            (execution-engine make-execution-engine dispose-execution-engine)
            (interpreter make-interpreter dispose-execution-engine)
            (jit-compiler make-jit-compiler dispose-execution-engine))))
    `(let ((,var (,(second (assoc type managers)) ,@args)))
       ,(when (eql type 'module)
          `(push ,var *should-dispose-modules*))
       ,(when (eql type 'execution-engine)
          `(setq *should-dispose-modules*
                 (remove ,(car args) *should-dispose-modules*)))
       (unwind-protect (progn ,@body)
         ,(if (eql type 'module)
              `(when (member ,var *should-dispose-modules*)
                 (setq *should-dispose-modules*
                       (remove ,var *should-dispose-modules*))
                 (,(third (assoc type managers)) ,var))
              `(,(third (assoc type managers)) ,var))))))

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

(defbitfield attribute
  (:z-ext #.(cl:ash 1 0))
  (:s-ext #.(cl:ash 1 1))
  (:no-return #.(cl:ash 1 2))
  (:in-reg #.(cl:ash 1 3))
  (:struct-ret #.(cl:ash 1 4))
  (:no-unwind #.(cl:ash 1 5))
  (:no-alias #.(cl:ash 1 6))
  (:by-val #.(cl:ash 1 7))
  (:nest #.(cl:ash 1 8))
  (:read-none #.(cl:ash 1 9))
  (:read-only #.(cl:ash 1 10))
  (:no-inline #.(cl:ash 1 11))
  (:always-inline #.(cl:ash 1 12))
  (:optimize-for-size #.(cl:ash 1 13))
  (:stack-protect #.(cl:ash 1 14))
  (:stack-protect-req #.(cl:ash 1 15))
  (:no-capture #.(cl:ash 1 21))
  (:no-red-zone #.(cl:ash 1 22))
  (:no-implicit-float #.(cl:ash 1 23))
  (:naked #.(cl:ash 1 24))
  (:inline-hint #.(cl:ash 1 25))
  (:stack-alignment #.(cl:ash 1 26)))

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
  ;; FIXME: should really make this null-terminated, but the actual null value
  ;;        (0 or +null-pointer+) depends on the value-type.
  (foreign-alloc (value-type type)
                 :initial-contents object
                 :count (or (capacity type) (length object))))

(defmethod translate-from-foreign (pointer (type carray))
  (if (capacity type)
      (loop for i from 0 below (capacity type)
            for value = (mem-aref pointer (value-type type) i)
            collect value)
      (loop for i from 0
            for value = (mem-aref pointer (value-type type) i)
            while (not (null-pointer-p value))
            collect value)))

(defmethod free-translated-object (value (type carray) param)
  (declare (ignore param))
  (foreign-free value))

(defmacro with-pointer-to-list ((pointer-var type length) &body body)
  `(with-foreign-object (,pointer-var ',type ,length)
     ,@body
     (convert-from-foreign ,pointer-var `(carray ,',type ,,length))))

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
