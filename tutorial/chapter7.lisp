(defpackage kaleidoscope.chapter7
  (:use
   #:cl
   #:k-lexer
   #:k-shared) ; would normally use #:llvm, but wanted to make usage clear
  (:shadow #:condition)
  (:export #:toplevel))

(in-package :kaleidoscope.chapter7)

(defun get-next-token ()
  (%get-next-token k-lexer::*tokens7*))

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

(defclass unary-expression (expression)
  ((opcode :initarg :opcode :reader opcode)
   (operand :initarg :operand :reader operand))
  (:documentation "Expression class for a unary operator."))

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

(defclass var-expression (expression)
  ((var-names :initarg :var-names :reader var-names)
   (body :initarg :body :reader body))
  (:documentation "Expression class for var/in"))

(defclass prototype ()
  ((name :initform "" :initarg :name :reader name)
   (arguments :initform (make-array 0) :initarg :arguments :reader arguments)
   (operatorp :initform nil :initarg :operatorp :reader operatorp)
   (precedence :initform 0 :initarg :precedence :reader precedence))
  (:documentation
   "This class represents the “prototype” for a function, which captures its
    name, and its argument names (thus implicitly the number of arguments the
    function takes)."))

(defmethod unary-operator-p ((expression prototype))
  (and (operatorp expression) (= (length (arguments expression)) 1)))
(defmethod binary-operator-p ((expression prototype))
  (and (operatorp expression) (= (length (arguments expression)) 2)))

(defmethod operator-name ((expression prototype))
  (assert (or (unary-operator-p expression) (binary-operator-p expression)))
  (elt (name expression) (1- (length (name expression)))))

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

(defun parse-var-expression ()
  (get-next-token)
  (unless (eql *current-token* ':tok-identifier)
    (error 'kaleidoscope-error :message "expected identifier after var"))
  (let ((var-names (loop
                     for name = *identifier-string*
                     for init = nil
                     do (get-next-token)
                        (when (eql *current-token* #\=)
                          (get-next-token)
                          (setf init (parse-expression))
                          (unless init
                            (return-from parse-var-expression)))
                     collecting (cons name init)
                     while (eql *current-token* #\,)
                     do (get-next-token)
                        (unless (eql *current-token* ':tok-identifier)
                          (error 'kaleidoscope-error
                                 :message
                                 "expected identifier list after var")))))
    (unless (eql *current-token* ':tok-in)
      (error 'kaleidoscope-error :message "expected 'in' keyword after 'var'"))
    (get-next-token)
    (let ((body (parse-expression)))
      (when body
        (make-instance 'var-expression :var-names var-names :body body)))))

(defun parse-primary ()
  (case *current-token*
    (:tok-identifier (parse-identifier-expression))
    (:tok-number (parse-number-expression))
    (#\( (parse-paren-expression))
    (:tok-if (parse-if-expression))
    (:tok-for (parse-for-expression))
    (:tok-var (parse-var-expression))
    (otherwise (error 'kaleidoscope-error
                      :message "unknown token when expecting an expression"))))

(defun parse-unary ()
  ;; If the current token is not an operator, it must be a primary expr.
  (if (or (not (characterp *current-token*))
          (find *current-token* '(#\( #\,)))
    (parse-primary)
    ;; If this is a unary operator, read it.
    (let ((opcode *current-token*))
      (get-next-token)
      (let ((operand (parse-unary)))
        (when operand
          (make-instance 'unary-expression :opcode opcode :operand operand))))))

(defun parse-bin-op-rhs (expression-precedence lhs)
  (do () (nil)
    (let ((token-precedence (get-precedence *current-token*)))
      (if (< token-precedence expression-precedence)
        (return-from parse-bin-op-rhs lhs)
        (let ((binary-operator *current-token*))
          (get-next-token)
          (let ((rhs (parse-unary)))
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
  (let ((lhs (parse-unary)))
    (when lhs
      (parse-bin-op-rhs 0 lhs))))

(defun parse-prototype ()
  "prototype
     ::= id '(' id* ')'
     ::= binary LETTER number? (id, id)
     ::= unary LETTER (id)"
  (let ((function-name)
        (operator-arity nil)
        (binary-precedence 30))
    (case *current-token*
      (:tok-identifier (setf function-name *identifier-string*))
      (:tok-unary
       (get-next-token)
       (unless (characterp *current-token*)
         (error 'kaleidoscope-error :message "Expected unary operator"))
       (setf function-name (format nil "unary~a" *current-token*)
             operator-arity 1))
      (:tok-binary
       (get-next-token)
       (unless (characterp *current-token*)
         (error 'kaleidoscope-error :message "Expected binary operator"))
       (setf function-name (format nil "binary~a" *current-token*)
             operator-arity 2)
       (get-next-token)
       (when (eql *current-token* ':tok-number)
         (unless (<= 1 *number-value* 100)
           (error 'kaleidoscope-error
                  :message "Invalid precedence: must be 1..100"))
         (setf binary-precedence *number-value*)))
      (otherwise (error 'kaleidoscope-error
                        :message "Expected function name in prototype")))
    (unless (eql (get-next-token) #\()
      (error 'kaleidoscope-error :message "Expected '(' in prototype"))
    (let ((arg-names (coerce (loop while (eql (get-next-token) ':tok-identifier)
                               collecting *identifier-string*)
                             'vector)))
      (unless (eql *current-token* #\))
        (error 'kaleidoscope-error :message "Expected ')' in prototype"))
      (get-next-token)
      (when (and operator-arity (/= (length arg-names) operator-arity))
        (error 'kaleidoscope-error
               :message "Invalid number of operands for operator"))
      (make-instance 'prototype
        :name function-name :arguments arg-names
        :operatorp operator-arity :precedence binary-precedence))))

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

(defun create-entry-block-alloca (function var-name)
  "Create an alloca instruction in the entry block of the function. This is used
   for mutable variables etc."
  (let ((tmp-b (llvm:make-builder)))
    ;; FIXME: this doesn't set the proper insertion point
    (llvm:position-builder tmp-b (llvm:entry-basic-block function))
    (llvm:build-alloca tmp-b (llvm:double-type) var-name)))

(defmethod codegen ((expression number-expression))
  (llvm:const-real (llvm:double-type) (value expression)))

(defmethod codegen ((expression variable-expression))
  (let ((v (gethash (name expression) *named-values*)))
    (if v
        (llvm:build-load *builder* v (name expression))
        (error 'kaleidoscope-error :message "unknown variable name"))))

(defmethod codegen ((expression unary-expression))
  (let ((operand-v (codegen (operand expression))))
    (when operand-v
      (let ((f (llvm:named-function *module*
                                    (format nil "unary~a"
                                            (opcode expression)))))
        (unless f
          (error 'kaleidoscope-error :message "Unknown unary operator"))
        (llvm:build-call *builder* f (list operand-v) "unop")))))

(defmethod codegen ((expression binary-expression))
  (if (eql (operator expression) #\=)
    ;; TODO: can we typecheck (lhs expression) here?
    (let ((lhse (lhs expression))
          (val (codegen (rhs expression))))
      (when val
        (let ((variable (gethash (name lhse) *named-values*)))
          (unless variable
            (error 'kaleidoscope-error :message "Unknown variable name"))
          (llvm:build-store *builder* val variable)
          val)))
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
          (otherwise
           (let ((f (llvm:named-function *module*
                                         (format nil "binary~a"
                                                 (operator expression)))))
             (assert f () "binary operator not found!")
             (llvm:build-call *builder* f (list l r) "binop"))))))))

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
  (let* ((function (llvm:basic-block-parent (llvm:insertion-block *builder*)))
         (alloca (create-entry-block-alloca function (var-name expression)))
         (start-val (codegen (start expression))))
    (when start-val
      (llvm:build-store *builder* start-val alloca)
      ;; Make the new basic block for the loop header, inserting after current
      ;; block.
      (let* ((loop-bb (llvm:append-basic-block function "loop")))
        (llvm:build-br *builder* loop-bb)
        (llvm:position-builder *builder* loop-bb)
        (let ((old-val (gethash (var-name expression) *named-values*)))
          (setf (gethash (var-name expression) *named-values*) alloca)
          (when (codegen (body expression))
            (let ((step-val (if (step* expression)
                                (codegen (step* expression))
                                (llvm:const-real (llvm:double-type) 1))))
              (when step-val
                (let ((end-cond (codegen (end expression))))
                  (when end-cond
                    (let* ((cur-var (llvm:build-load *builder*
                                                     alloca
                                                     (var-name expression)))
                           (next-var (llvm:build-f-add *builder*
                                                       cur-var
                                                       step-val
                                                       "nextvar")))
                      (llvm:build-store *builder* next-var alloca)
                      (setf end-cond
                            (llvm:build-f-cmp *builder*
                                              :/=
                                              end-cond
                                              (llvm:const-real
                                               (llvm:double-type)
                                               0)
                                              "loopcond"))
                      (let ((after-bb (llvm:append-basic-block function
                                                               "afterloop")))
                        (llvm:build-cond-br *builder* end-cond loop-bb after-bb)
                        (llvm:position-builder *builder* after-bb)
                        (if old-val
                            (setf (gethash (var-name expression) *named-values*)
                                  old-val)
                            (remhash (var-name expression) *named-values*))
                        ;; for expr always returns 0.
                        (llvm:const-null (llvm:double-type))))))))))))))

(defmethod codegen ((expression var-expression))
  (let* ((function (llvm:basic-block-parent (llvm:insertion-block *builder*)))
         (old-bindings (map 'vector
                            (lambda (var-binding)
                              (destructuring-bind (var-name . init) var-binding
                                (let ((alloca
                                       (create-entry-block-alloca function
                                                                  var-name)))
                                  (llvm:build-store *builder*
                                                    (if init
                                                      ;; FIXME: handle error
                                                      (codegen init)
                                                      (llvm:const-real
                                                       (llvm:double-type)
                                                       0))
                                                    alloca)
                                  (prog1 (gethash var-name *named-values*)
                                    (setf (gethash var-name *named-values*)
                                          alloca)))))
                            (var-names expression)))
         (body-val (codegen (body expression))))
    (when body-val
      (map 'vector
           (lambda (var-binding old-binding)
             (setf (gethash (car var-binding) *named-values*) old-binding))
           (var-names expression) old-bindings)
      body-val)))
             

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
            ;; Set names for all arguments.
            (map nil
                 (lambda (argument name)
                   (setf (llvm:value-name argument) name))
                 (llvm:params function)
                 (arguments expression))
            (error 'kaleidoscope-error
                   :message "redefinition of function with different # args"))
        (error 'kaleidoscope-error :message "redefinition of function"))
    function))

(defmethod create-argument-allocas ((expression prototype) f)
  (map nil
       (lambda (parameter argument)
         (let ((alloca (create-entry-block-alloca f argument)))
           (llvm:build-store *builder* parameter alloca)
           (setf (gethash argument *named-values*) alloca)))
       (llvm:params f) (arguments expression)))

(defmethod codegen ((expression function-definition))
  (let* ((*named-values* (make-hash-table :test #'equal))
         (function (codegen (prototype expression))))
    (when function
      ;; If this is an operator, install it.
      (when (binary-operator-p (prototype expression))
        (setf (gethash (operator-name (prototype expression))
                       *binop-precedence*)
              (precedence (prototype expression))))
      (llvm:position-builder-at-end *builder*
                                    (llvm:append-basic-block function "entry"))
      (create-argument-allocas (prototype expression) function)
      (let ((retval (codegen (body expression))))
        (if retval
          (progn
            (llvm:build-ret *builder* retval)
            (unless (llvm:verify-function function)
              (error 'kaleidoscope-error
                     :message "Function verification failure."))
            (llvm:run-function-pass-manager *fpm* function)
            function)
          (progn
            (llvm:delete-function function)
            (when (binary-operator-p (prototype expression))
              (remhash (operator-name (prototype expression))
                       *binop-precedence*))))))))

;;; top-level

(defvar *execution-engine*)

(defun handle-definition ()
  (let ((function (parse-definition)))
    (if function
      (let ((lf (codegen function)))
        (when lf
          (format *output?* "Read function definition:")
          (write-string (llvm:print-value-to-string lf) *output?*)))
      (get-next-token))))

(defun handle-extern ()
  (let ((prototype (parse-extern)))
    (if prototype
      (let ((function (codegen prototype)))
        (when function
          (format *output?* "Read extern: ")
          (write-string (llvm:print-value-to-string function) *output?*)))
      (get-next-token))))

(defun handle-top-level-expression ()
  "Evaluate a top-level expression into an anonymous function."
  (handler-case 
      (let* ((lf (codegen (parse-top-level-expression)))
             (ptr (llvm:pointer-to-global *execution-engine* lf)))
        (format *output?* "Evaluated to ~fD0"
                ;; NOTE: The C version of the tutorial only has the JIT side
                ;;       of this, so if you have an interpreter, it breaks.
                (if (cffi:pointer-eq ptr lf)	; we have an interpreter
                    (llvm:generic-value-to-float
                     (llvm:double-type)
                     (llvm:run-function *execution-engine* ptr ()))
                    (cffi:foreign-funcall-pointer ptr () :double))))
    (kaleidoscope-error (e)
      (get-next-token)
      (format *output?* "error: ~a~%" e))))

(define-condition kaleidoscope-error (error)
  ((message :initarg :message :reader message))
  (:report (lambda (condition stream)
             (write-string (message condition) stream))))

(defun main-loop (exit)
  (do ()
      ((main-loop-end))
    (per-loop exit)))

(defun main-loop-end ()
  (eql *current-token* ':tok-eof))

(defun per-loop (exit)
  (format *output?* "~&ready> ")
  (handler-case (case *current-token*
		  (#\; (get-next-token))
		  (:tok-def (handle-definition))
		  (:tok-extern (handle-extern))
		  (:tok-quit (funcall exit))
		  (otherwise (handle-top-level-expression)))
    (kaleidoscope-error (e) (format *output?* "error: ~a~%" e))))

;;; "Library" functions that can be "extern'd" from user code.

;;; NOTE: These functions are defined in kaleidoscope-extern.c

;;; driver

(defun toplevel ()
  ;; install standard binary operators
  ;; 1 is lowest precedence
  (setf (gethash #\= *binop-precedence*) 2
        (gethash #\< *binop-precedence*) 10
        (gethash #\+ *binop-precedence*) 20
        (gethash #\- *binop-precedence*) 30
        (gethash #\* *binop-precedence*) 40)
  (reset-token-reader)
  (llvm:with-objects ((*builder* llvm:builder)
		      (*module* llvm:module "my cool jit")
		      (*execution-engine* llvm:execution-engine *module*)
		      (*fpm* llvm:function-pass-manager *module*))
    (llvm:add-target-data (llvm:target-data *execution-engine*) *fpm*)

    ;;passes
    (llvm:add-promote-memory-to-register-pass *fpm*)
    (llvm:add-instruction-combining-pass *fpm*)
    (llvm:add-reassociate-pass *fpm*)
    (llvm:add-gvn-pass *fpm*)
    (llvm:add-cfg-simplification-pass *fpm*)
    ;;new
    (llvm:add-constant-propagation-pass *fpm*)
    (llvm:add-dead-store-elimination-pass *fpm*)
    (llvm:add-independent-variable-simplification-pass *fpm*)
    ;;
    (llvm:initialize-function-pass-manager *fpm*)

    (format *output?* "~&ready> ")
    (get-next-token)
    (callcc (function main-loop))
    (dump-module *module*)
    (values)))
