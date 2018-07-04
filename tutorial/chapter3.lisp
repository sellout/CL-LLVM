(defpackage kaleidoscope.chapter3
  (:use
   #:cl
   #:k-lexer
   #:k-shared) ; would normally use #:llvm, but wanted to make usage clear
  (:export #:toplevel))

(in-package :kaleidoscope.chapter3)

;;; abstract syntax tree

(defclass expression ()
  ()
  (:documentation "Base class for all expression nodes."))

(defclass number-expression (expression)
  ((value :initarg :value :reader value))
  (:documentation "Expression class for numeric literals like “1.0”."))

(defclass variable-expression (expression)
  ((name :initarg :name :reader name))
  (:documentation "Expression class for referencing a variable, like “a”."))

(defclass binary-expression (expression)
  ((operator :initarg :operator :reader operator)
   (lhs :initarg :lhs :reader lhs)
   (rhs :initarg :rhs :reader rhs))
  (:documentation "Expression class for a binary operator."))

(defclass call-expression (expression)
  ((callee :initarg :callee :reader callee)
   (arguments :initarg :arguments :reader arguments))
  (:documentation "Expression class for function calls."))

(defclass prototype ()
  ((name :initform "" :initarg :name :reader name)
   (arguments :initform (make-array 0) :initarg :arguments :reader arguments))
  (:documentation
   "This class represents the “prototype” for a function, which captures its
    name, and its argument names (thus implicitly the number of arguments the
    function takes)."))

(defclass function-definition ()
  ((prototype :initarg :prototype :reader prototype)
   (body :initarg :body :reader body))
  (:documentation "This class represents a function definition itself."))

;;; parser

(defvar *binop-precedence* (make-hash-table :size 4))

(defun get-precedence (token)
  (gethash token *binop-precedence* -1))

(defun parse-identifier-expression ()
  (let ((id-name *identifier-string*))
    (if (eql (get-next-token) #\()
      (prog2 (get-next-token) ; eat (
          (make-instance 'call-expression
            :callee id-name
            :arguments (if (not (eql *current-token* #\)))
                         (loop
                           for arg = (parse-expression)
                           unless arg
                           do (return-from parse-identifier-expression)
                           collecting arg
                           until (eql *current-token* #\))
                           do (or (eql *current-token* #\,)
                                  (error 'kaleidoscope-error
                                         :message "Expected ')' or ',' in argument list"))
                              (get-next-token))))
        (get-next-token)) ; eat the ')'.
      (make-instance 'variable-expression :name id-name))))

(defun parse-number-expression ()
  (prog1 (make-instance 'number-expression :value *number-value*)
    (get-next-token)))

(defun parse-paren-expression ()
  (get-next-token)
  (let ((v (parse-expression)))
    (when v
      (if (eql *current-token* #\))
        (get-next-token)
        (error 'kaleidoscope-error :message "expected ')'"))
      v)))

(defun parse-primary ()
  (case *current-token*
    (:tok-identifier (parse-identifier-expression))
    (:tok-number (parse-number-expression))
    (#\( (parse-paren-expression))
    (otherwise (error 'kaleidoscope-error
                      :message "unknown token when expecting an expression"))))

(defun parse-bin-op-rhs (expression-precedence lhs)
  (do () (nil)
    (let ((token-precedence (get-precedence *current-token*)))
      (if (< token-precedence expression-precedence)
        (return-from parse-bin-op-rhs lhs)
        (let ((binary-operator *current-token*))
          (get-next-token)
          (let ((rhs (parse-primary)))
            (when rhs
              (let ((next-precedence (get-precedence *current-token*)))
                (when (< token-precedence next-precedence)
                  (setf rhs (parse-bin-op-rhs (1+ token-precedence) rhs))
                  (unless rhs
                    (return-from parse-bin-op-rhs))))
              (setf lhs
                    (make-instance 'binary-expression
                      :operator binary-operator
                      :lhs lhs :rhs rhs)))))))))

(defun parse-expression ()
  (let ((lhs (parse-primary)))
    (when lhs
      (parse-bin-op-rhs 0 lhs))))

(defun parse-prototype ()
  "prototype
     ::= id '(' id* ')'"
  (if (eql *current-token* ':tok-identifier)
      (let ((function-name *identifier-string*))
        (unless (eql (get-next-token) #\()
          (error 'kaleidoscope-error :message "Expected '(' in prototype"))
        (let ((arg-names (coerce (loop while (eql (get-next-token)
                                                  ':tok-identifier)
                                    collecting *identifier-string*)
                                 'vector)))
          (unless (eql *current-token* #\))
            (error 'kaleidoscope-error :message "Expected ')' in prototype"))
          (get-next-token)
          (make-instance 'prototype :name function-name :arguments arg-names)))
      (error 'kaleidoscope-error
             :message "Expected function name in prototype")))

(defun parse-definition ()
  (get-next-token) ; eat def
  (let ((prototype (parse-prototype)))
    (if prototype
      (let ((expression (parse-expression)))
        (if expression
          (make-instance 'function-definition
            :prototype prototype
            :body expression))))))

(defun parse-top-level-expression ()
  (let ((expression (parse-expression)))
    (if expression
      (make-instance 'function-definition
        :prototype (make-instance 'prototype)
        :body expression))))

(defun parse-extern ()
  (get-next-token) ; eat extern
  (parse-prototype))

;;; code generation

(defvar *module*)
(defvar *builder*)
(defvar *named-values*)

(defmethod codegen ((expression number-expression))
  (llvm:const-real (llvm:double-type) (value expression)))

(defmethod codegen ((expression variable-expression))
  (let ((v (gethash (name expression) *named-values*)))
    (or v
        (error 'kaleidoscope-error :message "unknown variable name"))))

(defmethod codegen ((expression binary-expression))
  (let ((l (codegen (lhs expression)))
        (r (codegen (rhs expression))))
    (when (and l r)
      (case (operator expression)
        (#\+ (llvm:build-f-add *builder* l r "addtmp"))
        (#\- (llvm:build-f-sub *builder* l r "subtmp"))
        (#\* (llvm:build-f-mul *builder* l r "multmp"))
        (#\< (llvm:build-ui-to-fp *builder*
                                  (llvm:build-f-cmp *builder*
                                                    :unordered-< l r
                                                    "cmptmp")
                                  (llvm:double-type)
                                  "booltmp"))
        (otherwise (error 'kaleidoscope-error
                          :message "invalid binary operators"))))))

(defmethod codegen ((expression call-expression))
  (let ((callee (llvm:named-function *module* (callee expression))))
    (if callee
      (if (= (llvm:count-params callee) (length (arguments expression)))
        (llvm:build-call *builder*
                         callee
                         (map 'vector #'codegen (arguments expression))
                         "calltmp")
        (error 'kaleidoscope-error :message "incorrect # arguments passed"))
      (error 'kaleidoscope-error :message "unknown function referenced"))))

(defmethod codegen ((expression prototype))
  (let* ((doubles (make-array (length (arguments expression))
                              :initial-element (llvm:double-type)))
         (f-type (llvm:function-type (llvm:double-type) doubles))
         (function (llvm:add-function *module* (name expression) f-type)))
    ;; If F conflicted, there was already something named 'Name'.  If it has a
    ;; body, don't allow redefinition or reextern.
    (when (not (string= (llvm:value-name function) (name expression)))
      (llvm:delete-function function)
      (setf function (llvm:named-function *module* (name expression))))
    (if (= (llvm:count-basic-blocks function) 0)
        (if (= (llvm:count-params function) (length (arguments expression)))
            (when (boundp '*named-values*)
              ;; Set names for all arguments.
              (map nil
                   (lambda (argument name)
                     (setf (llvm:value-name argument) name
                           (gethash name *named-values*) argument))
                   (llvm:params function)
                   (arguments expression)))
            (error 'kaleidoscope-error
                   :message "redefinition of function with different # args"))
        (error 'kaleidoscope-error :message "redefinition of function"))
    function))

(defmethod codegen ((expression function-definition))
  (let* ((*named-values* (make-hash-table :test #'equal))
         (function (codegen (prototype expression))))
    (when function
      (llvm:position-builder-at-end *builder*
                                    (llvm:append-basic-block function "entry"))
      (let ((retval (codegen (body expression))))
        (if retval
            (progn
              (llvm:build-ret *builder* retval)
              (unless (llvm:verify-function function)
                (error 'kaleidoscope-error
                       :message "Function verification failure."))
              function)
            (llvm:delete-function function))))))

;;; top-level
  

(defun handle-definition ()
  (let ((function (parse-definition)))
    (if function
        (let ((lf (codegen function)))
          (when lf
            (format *output?* "Read function definition:")
            (dump-value lf)))
        (get-next-token))))

(defun handle-extern ()
  (let ((prototype (parse-extern)))
    (if prototype
        (let ((function (codegen prototype)))
          (when function
            (format *output?* "Read extern: ")
            (dump-value function)))
        (get-next-token))))

(defun handle-top-level-expression ()
  "Evaluate a top-level expression into an anonymous function."
  (handler-case 
      (let ((lf (codegen (parse-top-level-expression))))
        (format *output?* "Read top-level expression:")
        (dump-value lf))
    (kaleidoscope-error (e)
      (get-next-token)
      (format *output?* "error: ~a~%" e))))

(define-condition kaleidoscope-error (error)
  ((message :initarg :message :reader message))
  (:report (lambda (condition stream)
             (write-string (message condition) stream))))

(defun main-loop (exit)
  (do () ((eql *current-token* ':tok-eof))
    (format *output?* "~&ready> ")
    (handler-case (case *current-token*
                    (#\; (get-next-token))
                    (:tok-def (handle-definition))
                    (:tok-extern (handle-extern))
		    (:tok-quit (funcall exit))
                    (otherwise (handle-top-level-expression)))
      (kaleidoscope-error (e) (format *output?* "error: ~a~%" e)))))

;;; "Library" functions that can be "extern'd" from user code.

;;; NOTE: These functions are defined in kaleidoscope-extern.c

;;; driver

(defun toplevel ()
  ;; install standard binary operators
  ;; 1 is lowest precedence
  (setf (gethash #\< *binop-precedence*) 10
        (gethash #\+ *binop-precedence*) 20
        (gethash #\- *binop-precedence*) 30
        (gethash #\* *binop-precedence*) 40)
  (reset-token-reader)
  (llvm:with-objects ((*builder* llvm:builder)
                      (*module* llvm:module "my cool jit"))
    (format *output?* "~&ready> ")
    (let ((*token-types* k-lexer::*tokens2*))
      (get-next-token)
      (callcc (function main-loop)))
    (dump-module *module*)
    (values)))
