(defpackage kaleidoscope.chapter2
  (:use #:cl #:k-lexer) ; would normally use #:llvm, but wanted to make usage clear
  (:export #:toplevel))

(in-package :kaleidoscope.chapter2)

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

(defvar *unknowns* 0)

(defun parse-primary ()
  (case *current-token*
    (tok-identifier (parse-identifier-expression))
    (tok-number (parse-number-expression))
    (#\( (parse-paren-expression))
    (otherwise (incf *unknowns*)
               (if (> *unknowns* 1) (break))
               (error 'kaleidoscope-error
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
  (if (eql *current-token* 'tok-identifier)
      (let ((function-name *identifier-string*))
        (unless (eql (get-next-token) #\()
          (error 'kaleidoscope-error :message "Expected '(' in prototype"))
        (let ((arg-names (coerce (loop while (eql (get-next-token)
                                                  'tok-identifier)
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

;;; top-level

(defun handle-definition ()
  (if (parse-definition)
      (format *error-output* "Parsed a function definition~%")
      (get-next-token)))

(defun handle-extern ()
  (if (parse-extern)
      (format *error-output* "Parsed an extern~%")
      (get-next-token)))

(defun handle-top-level-expression ()
  "Evaluate a top-level expression into an anonymous function."
  (handler-case 
      (progn (parse-top-level-expression)
             (format *error-output* "Parsed a top-level expr~%"))
    (kaleidoscope-error (e)
      (get-next-token)
      (format *error-output* "error: ~a~%" e))))

(define-condition kaleidoscope-error (error)
  ((message :initarg :message :reader message))
  (:report (lambda (condition stream)
             (write-string (message condition) stream))))

(defun main-loop ()
  (do () ((eql *current-token* 'tok-eof))
    (format *error-output* "~&ready> ")
    (handler-case (case *current-token*
                    (#\; (get-next-token))
                    (tok-def (handle-definition))
                    (tok-extern (handle-extern))
                    (otherwise (handle-top-level-expression)))
      (kaleidoscope-error (e) (format *error-output* "error: ~a~%" e)))))

;;; driver

(defun toplevel ()
  ;; install standard binary operators
  ;; 1 is lowest precedence
  (setf (gethash #\< *binop-precedence*) 10
        (gethash #\+ *binop-precedence*) 20
        (gethash #\- *binop-precedence*) 30
        (gethash #\* *binop-precedence*) 40)
  (format *error-output* "~&ready> ")
  (get-next-token)
  (main-loop))
