(defpackage kaleidoscope.chapter5
  (:use
   #:cl
   #:k-lexer
   #:k-shared) ; would normally use #:llvm, but wanted to make usage clear
  (:shadow #:condition)
  (:export #:toplevel))

(in-package :kaleidoscope.chapter5)

(defun get-next-token ()
  (%get-next-token k-lexer::*tokens5*))

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

(defclass if-expression (expression)
  ((condition :initarg :condition :reader condition)
   (then :initarg :then :reader then)
   (else :initarg :else :reader else))
  (:documentation "Expression class for if/then/else."))

(defclass for-expression (expression)
  ((var-name :initarg :var-name :reader var-name)
   (start :initarg :start :reader start)
   (end :initarg :end :reader end)
   ;; FIXME: why is CCL's conflicting STEP visible here?
   (step :initarg :step :reader step*)
   (body :initarg :body :reader body))
  (:documentation "Expression class for for/in."))

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

(defun parse-if-expression ()
  (get-next-token) ; eat the if
  (let ((condition (parse-expression)))
    (when condition
      (unless (eql *current-token* ':tok-then)
        (error 'kaleidoscope-error :message "expected then"))
      (get-next-token) ; eat the then
      (let ((then (parse-expression)))
        (when then
          (unless (eql *current-token* ':tok-else)
            (error 'kaleidoscope-error :message "expected else"))
          (get-next-token) ; eat the else
          (let ((else (parse-expression)))
            (when else
              (make-instance 'if-expression
                :condition condition :then then :else else))))))))

(defun parse-for-expression ()
  (get-next-token) ; eat the for.
  (unless (eql *current-token* ':tok-identifier)
    (error 'kaleidoscope-error :message "expected identifier after for"))
  (let ((id-name *identifier-string*))
    (get-next-token) ; eat identifier.
    (unless (eql *current-token* #\=)
      (error 'kaleidoscope-error :message "expected '=' after for"))
    (get-next-token)
    (let ((start (parse-expression)))
      (when start
        (unless (eql *current-token* #\,)
          (error 'kaleidoscope-error
                 :message "expected ',' after for start value"))
        (get-next-token)
        (let ((end (parse-expression)))
          (when end
            ;; The step value is optional
            (let ((step))
              (when (eql *current-token* #\,)
                (get-next-token)
                (setf step (parse-expression))
                (unless step
                  (return-from parse-for-expression)))
              (unless (eql *current-token* ':tok-in)
                (error 'kaleidoscope-error :message "expected 'in' after for"))
              (get-next-token) ; eat 'in',
              (let ((body (parse-expression)))
                (when body
                  (make-instance 'for-expression
                    :var-name id-name :start start :end end :step step
                    :body body))))))))))

(defun parse-primary ()
  (case *current-token*
    (:tok-identifier (parse-identifier-expression))
    (:tok-number (parse-number-expression))
    (#\( (parse-paren-expression))
    (:tok-if (parse-if-expression))
    (:tok-for (parse-for-expression))
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
(defvar *fpm*)

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

(defmethod codegen ((expression if-expression))
  (let ((cond-v (codegen (condition expression))))
    (when cond-v
      (setf cond-v
            (llvm:build-f-cmp *builder* 
                              :/= cond-v (llvm:const-real (llvm:double-type) 0)
                              "ifcond"))
      (let* ((function (llvm:basic-block-parent
                        (llvm:insertion-block *builder*)))
             (then-bb (llvm:append-basic-block function "then"))
             ;; FIXME: not sure if we can append these at this point
             (else-bb (llvm:append-basic-block function "else"))
             (merge-bb (llvm:append-basic-block function "ifcont")))
        (llvm:build-cond-br *builder* cond-v then-bb else-bb)
        (llvm:position-builder *builder* then-bb)
        (let ((then-v (codegen (then expression))))
          (when then-v
            (llvm:build-br *builder* merge-bb)
            ;; Codegen of 'Then' can change the current block, update THEN-BB
            ;; for the PHI.
            (setf then-bb (llvm:insertion-block *builder*))
            (llvm:position-builder *builder* else-bb)
            (let ((else-v (codegen (else expression))))
              (when else-v
                (llvm:build-br *builder* merge-bb)
                ;; Codegen of 'Else' can change the current block, update
                ;; ELSE-BB for the PHI.
                (setf else-bb (llvm:insertion-block *builder*))
                ;; Emit merge block.
                (llvm:position-builder *builder* merge-bb)
                (let ((pn (llvm:build-phi *builder*
                                          (llvm:double-type) "iftmp")))
                  (llvm:add-incoming pn
                                     (list then-v else-v)
                                     (list then-bb else-bb))
                  pn)))))))))

(defmethod codegen ((expression for-expression))
  (let ((start-val (codegen (start expression))))
    (when start-val
      ;; Make the new basic block for the loop header, inserting after current
      ;; block.
      (let* ((preheader-bb (llvm:insertion-block *builder*))
             (function (llvm:basic-block-parent preheader-bb))
             (loop-bb (llvm:append-basic-block function "loop")))
        (llvm:build-br *builder* loop-bb)
        (llvm:position-builder *builder* loop-bb)
        (let ((variable (llvm:build-phi *builder*
                                        (llvm:double-type)
                                        (var-name expression))))
          (llvm:add-incoming variable (list start-val) (list preheader-bb))
          (let ((old-val (gethash (var-name expression) *named-values*)))
            (setf (gethash (var-name expression) *named-values*) variable)
            (when (codegen (body expression))
              (let ((step-val (if (step* expression)
                                (codegen (step* expression))
                                (llvm:const-real (llvm:double-type) 1))))
                (when step-val
                  (let ((next-var (llvm:build-f-add *builder*
                                                    variable
                                                    step-val
                                                    "nextvar"))
                        (end-cond (codegen (end expression))))
                    (when end-cond
                      (setf end-cond
                            (llvm:build-f-cmp *builder*
                                              :/=
                                              end-cond
                                              (llvm:const-real
                                               (llvm:double-type)
                                               0)
                                              "loopcond"))
                      (let ((loop-end-bb (llvm:insertion-block *builder*))
                            (after-bb (llvm:append-basic-block function
                                                               "afterloop")))
                        (llvm:build-cond-br *builder* end-cond loop-bb after-bb)
                        (llvm:position-builder *builder* after-bb)
                        (llvm:add-incoming variable
                                           (list next-var) (list loop-end-bb))
                        (if old-val
                          (setf (gethash (var-name expression) *named-values*)
                                old-val)
                          (remhash (var-name expression) *named-values*))
                        ;; for expr always returns 0.
                        (llvm:const-null (llvm:double-type))))))))))))))

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
            (llvm:run-function-pass-manager *fpm* function)
            function)
          (llvm:delete-function function))))))

;;; top-level

(defvar *execution-engine*)

(defun handle-definition ()
  (let ((function (parse-definition)))
    (if function
      (let ((lf (codegen function)))
        (when lf
          (format *error-output* "Read function definition:")
          (llvm:dump-value lf)))
      (get-next-token))))

(defun handle-extern ()
  (let ((prototype (parse-extern)))
    (if prototype
      (let ((function (codegen prototype)))
        (when function
          (format *error-output* "Read extern: ")
          (llvm:dump-value function)))
      (get-next-token))))

(defun handle-top-level-expression ()
  "Evaluate a top-level expression into an anonymous function."
  (handler-case 
      (let* ((lf (codegen (parse-top-level-expression)))
             (ptr (llvm:pointer-to-global *execution-engine* lf)))
        (llvm:dump-value lf)
        (format *error-output* "Evaluated to ~f"
                ;; NOTE: The C version of the tutorial only has the JIT side
                ;;       of this, so if you have an interpreter, it breaks.
                (if (cffi:pointer-eq ptr lf)        ; we have an interpreter
                    (llvm:generic-value-to-float
                     (llvm:double-type)
                     (llvm:run-function *execution-engine* ptr ()))
                    (cffi:foreign-funcall-pointer ptr () :double))))
    (kaleidoscope-error (e)
      (get-next-token)
      (format *error-output* "error: ~a~%" e))))

(define-condition kaleidoscope-error (error)
  ((message :initarg :message :reader message))
  (:report (lambda (condition stream)
             (write-string (message condition) stream))))

(defun main-loop ()
  (do () ((eql *current-token* ':tok-eof))
    (format *error-output* "~&ready> ")
    (handler-case (case *current-token*
                    (#\; (get-next-token))
                    (:tok-def (handle-definition))
                    (:tok-extern (handle-extern))
                    (otherwise (handle-top-level-expression)))
      (kaleidoscope-error (e) (format *error-output* "error: ~a~%" e)))))

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
  (llvm:with-objects ((*builder* llvm:builder)
                      (*module* llvm:module "my cool jit")
                      (*execution-engine* llvm:execution-engine *module*)
                      (*fpm* llvm:function-pass-manager *module*))
    (llvm:add-target-data (llvm:target-data *execution-engine*) *fpm*)
    (llvm:add-instruction-combining-pass *fpm*)
    (llvm:add-reassociate-pass *fpm*)
    (llvm:add-gvn-pass *fpm*)
    (llvm:add-cfg-simplification-pass *fpm*)
    (llvm:initialize-function-pass-manager *fpm*)

    (format *error-output* "~&ready> ")
    (get-next-token)
    (main-loop)
    (llvm:dump-module *module*)))
