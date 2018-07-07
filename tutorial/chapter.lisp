(in-package :k-shared)

;;;;lexer

(defvar +whitespace+ '(#\space #\tab nil #\linefeed #\return))
(defvar *identifier-string*)
(defvar *number-value*)

(defun chap-tokens (chap)
  (append
   (case chap
     ((2 3 4 5 6 7)
      '(("quit" . :tok-quit)
	("def" . :tok-def)
	("extern" . :tok-extern))))
   (case chap
     ((5 6 7)
      '(("if" . :tok-if)
	("then" . :tok-then)
	("else" . :tok-else)
	("for" . :tok-for)
	("in" . :tok-in))))
   (case chap
     ((6 7)
      '(("binary" . :tok-binary)
	("unary" . :tok-unary))))
   (case chap
     ((7)
      '(("var" . :tok-var))))))

(defvar *token-types*)

(defmacro %with-tokens (chap &body body)
  `(let ((*token-types* (chap-tokens ,chap)))
     ,@body))

(defun identifier-string-to-enum (&optional (identifier-string *identifier-string*)
				    (token-types *token-types*))
  (let ((cell (assoc identifier-string token-types :test (function string=))))
    (if cell
	(cdr cell)
	':tok-identifier)))

(defparameter *last-char* #\space)
(defun reset-token-reader ()
  (setf *last-char* #\space))
(defun read-token (&optional (token-types *token-types*) (stream *standard-input*))
  "Returns either a character or one of ':tok-eof, ':tok-def, ':tok-extern,
     ':tok-identifier, or ':tok-number."
  (flet ((get-char () (read-char stream nil nil)))
    (loop while (find *last-char* +whitespace+)
       do (setf *last-char* (get-char)))
    (cond ((eql *last-char* nil) ; check for EOF, do not eat
	   ':tok-eof)
	  ((alpha-char-p *last-char*)
	   (setf *identifier-string*
		 (coerce (cons *last-char*
			       (loop do (setf *last-char* (get-char))
				  while (alphanumericp *last-char*)
				  collecting *last-char*))
			 'string))
	   ;;cond goes here!!
	   (identifier-string-to-enum *identifier-string* token-types)
	   )
	  ((or (digit-char-p *last-char*) (char= *last-char* #\.))
	   (setf *number-value*
		 (let ((*read-eval* nil))
		   (read-from-string
		    (coerce (cons *last-char*
				  (loop do (setf *last-char* (get-char))
				     while (or (digit-char-p *last-char*)
					       (char= *last-char* #\.))
				     collecting *last-char*))
			    'string))))
	   ':tok-number)
	  ((eql *last-char* #\#) ; comment until end of line
	   (loop do (setf *last-char* (get-char))
	      until (find *last-char* '(nil #\linefeed #\return)))
	   (if (null *last-char*) ':tok-eof (read-token token-types stream)))
	  (t
	   (let ((this-char *last-char*))
	     (setf *last-char* (get-char))
	     this-char)))))

(defvar *current-token*)
(defun get-next-token (&optional (token-types *token-types*) (stream *input?*))
  (setf *current-token* (read-token token-types stream)))

;;;;AST

;;; (2 3 4 5 6 7)
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
(defclass function-definition ()
  ((prototype :initarg :prototype :reader prototype)
   (body :initarg :body :reader body))
  (:documentation "This class represents a function definition itself."))

;;(2 3 4 5)
#+nil
(defclass prototype ()
  ((name :initform "" :initarg :name :reader name)
   (arguments :initform (make-array 0) :initarg :arguments :reader arguments))
  (:documentation
   "This class represents the “prototype” for a function, which captures its
    name, and its argument names (thus implicitly the number of arguments the
    function takes)."))

;;;6 7
(defclass prototype ()
  ((name :initform "" :initarg :name :reader name)
   (arguments :initform (make-array 0) :initarg :arguments :reader arguments)
   (operatorp :initform nil :initarg :operatorp :reader operatorp)
   (precedence :initform 0 :initarg :precedence :reader precedence))
  (:documentation
   "This class represents the “prototype” for a function, which captures its
    name, and its argument names (thus implicitly the number of arguments the
    function takes)."))

;;;5 6 7
(defclass if-expression (expression)
  ((_condition :initarg :_condition :reader _condition)
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

;;;;6 7
(defclass unary-expression (expression)
  ((opcode :initarg :opcode :reader opcode)
   (operand :initarg :operand :reader operand))
  (:documentation "Expression class for a unary operator."))
(defmethod unary-operator-p ((expression prototype))
  (and (operatorp expression) (= (length (arguments expression)) 1)))
(defmethod binary-operator-p ((expression prototype))
  (and (operatorp expression) (= (length (arguments expression)) 2)))
(defmethod operator-name ((expression prototype))
  (assert (or (unary-operator-p expression) (binary-operator-p expression)))
  (elt (name expression) (1- (length (name expression)))))

;;;7
(defclass var-expression (expression)
  ((var-names :initarg :var-names :reader var-names)
   (body :initarg :body :reader body))
  (:documentation "Expression class for var/in"))

;;;;Parser

;;; parser 2
(defvar *binop-precedence* (make-hash-table :size 4))
(defun get-precedence (token)
  (gethash token *binop-precedence* -1))
(defun parse-identifier-expression ()
  (let ((id-name *identifier-string*))
    (if (eql (get-next-token) #\()
	(prog2 (get-next-token) ; eat (
	    (make-instance
	     'call-expression
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
  (ecase *chapter*
    ((2 3 4) (parse-primary234))
    ((5 6) (parse-primary56))
    ((7) (parse-primary7))))
;;;;2 3 4
(defun parse-primary234 ()
  (case *current-token*
    (:tok-identifier (parse-identifier-expression))
    (:tok-number (parse-number-expression))
    (#\( (parse-paren-expression))
    (otherwise (error 'kaleidoscope-error
		      :message "unknown token when expecting an expression"))))
;;;;5 6
(defun parse-primary56 ()
  (case *current-token*
    (:tok-identifier (parse-identifier-expression))
    (:tok-number (parse-number-expression))
    (#\( (parse-paren-expression))
    (:tok-if (parse-if-expression))
    (:tok-for (parse-for-expression))
    (otherwise (error 'kaleidoscope-error
		      :message "unknown token when expecting an expression"))))
;;;7
(defun parse-primary7 ()
  (case *current-token*
    (:tok-identifier (parse-identifier-expression))
    (:tok-number (parse-number-expression))
    (#\( (parse-paren-expression))
    (:tok-if (parse-if-expression))
    (:tok-for (parse-for-expression))
    (:tok-var (parse-var-expression))
    (otherwise (error 'kaleidoscope-error
                      :message "unknown token when expecting an expression"))))

(defun parse-dispatch ()
  (ecase *chapter*
    ((2 3 4 5) (parse-primary))
    ((6 7) (parse-unary))))
(defun parse-prototype ()
  (ecase *chapter*
    ((2 3 4 5) (parse-prototype2345))
    ((6 7) (parse-prototype67))))


(defun parse-bin-op-rhs (expression-precedence lhs)
  (do () (nil)
    (let ((token-precedence (get-precedence *current-token*)))
      (if (< token-precedence expression-precedence)
	  (return-from parse-bin-op-rhs lhs)
	  (let ((binary-operator *current-token*))
	    (get-next-token)
	    (let ((rhs (parse-dispatch)))
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
  (let ((lhs (parse-dispatch)))
    (when lhs
      (parse-bin-op-rhs 0 lhs))))

;;;;2 3 4 5
(defun parse-prototype2345 ()
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
;;;6 7
(defun parse-prototype67 ()
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
		       :prototype (make-instance 'prototype
						 :name "__anon_expr")
		       :body expression))))

(defun parse-extern ()
  (get-next-token) ; eat extern
  (parse-prototype))

;;; 5 6 7
(defun parse-if-expression ()
  (get-next-token) ; eat the if
  (let ((_condition (parse-expression)))
    (when _condition
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
			     :_condition _condition :then then :else else))))))))
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

;;; 6 7
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

;;; 7

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

;;;;code-generation

;;; code generation 3
(defvar *module*)
(defvar *builder*)
(defvar *named-values*)

(defparameter *function-protos* (make-hash-table :test 'equal))
(defun get-function (name)
  (block nil
    (let ((f llvm:named-function *module* name))
      (unless (cffi:null-pointer-p f)
	(return f)))
    (let ((previously-defined-fun (gethash name *function-protos*)))
      (when previously-defined-fun
	(return previously-defined-fun)))
    (return (cffi:null-pointer))))

(defmethod codegen ((expression number-expression))
   (llvm:const-real (llvm:double-type) (value expression)))

(defmethod codegen ((expression variable-expression))
  (let ((v (gethash (name expression) *named-values*)))
    (if v
	(ecase *chapter*
	  ((3 4 5 6) v)
	  ((7) (llvm:build-load *builder* v (name expression))))
	(error 'kaleidoscope-error :message "unknown variable name"))))

(defun codegen-binary=expression (expression)
  (let ((lhse (lhs expression))
	(val (codegen (rhs expression))))
    (when val
      (let ((variable (gethash (name lhse) *named-values*)))
	(unless variable
	  (error 'kaleidoscope-error :message "Unknown variable name"))
	(llvm:build-store *builder* val variable)
	val))))

(defmethod codegen ((expression binary-expression))
  (if (and (= *chapter* 7)
	   (eql (operator expression) #\=))
      ;; TODO: can we typecheck (lhs expression) here?
      (codegen-binary=expression expression)
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
	     (ecase *chapter*
	       ((3 4 5)
		(error 'kaleidoscope-error
		       :message "invalid binary operators"))
	       ((6 7)
		(let ((f (llvm:named-function *module*
					      (format nil "binary~a"
						      (operator expression)))))
		  (assert f () "binary operator not found!")
		  (llvm:build-call *builder* f (list l r) "binop"))))))))))

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
	     (when (or (= *chapter* 7)
		       (boundp '*named-values*))
	       ;; Set names for all arguments.
	       (map nil
		    (lambda (argument name)
		      (setf (llvm:value-name argument) name)
		      (ecase *chapter*
			((3 4 5 6) (setf (gethash name *named-values*) argument))
			((7))))
		    (llvm:params function)
		    (arguments expression)))
	     (error 'kaleidoscope-error
		    :message "redefinition of function with different # args"))
	 (error 'kaleidoscope-error :message "redefinition of function"))
     function))

(defmethod codegen ((expression function-definition))
  (ecase *chapter*
    ((3) (codegenfundef3 expression))
    ((4 5) (codegenfundef45 expression))
    ((6) (codegenfundef6 expression))
    ((7) (codegenfundef7 expression))))

;;;;3
(defun codegenfundef3 (expression)
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
;;;;4 5
(defun codegenfundef45 (expression)
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
	      #+nil
	      (when *fpm?*
		(llvm:run-function-pass-manager *fpm* function))
	      function)
	    (llvm:delete-function function))))))
;;;;6
(defun codegenfundef6 (expression)
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
      (let ((retval (codegen (body expression))))
	(if retval
	    (progn
	      (llvm:build-ret *builder* retval)
	      (unless (llvm:verify-function function)
		(error 'kaleidoscope-error
		       :message "Function verification failure."))
	      #+nil
	      (when *fpm?*
		(llvm:run-function-pass-manager *fpm* function))
	      function)
	    (progn
	      (llvm:delete-function function)
	      (when (binary-operator-p (prototype expression))
		(remhash (operator-name (prototype expression))
			 *binop-precedence*))))))))
;;;;7
(defun codegenfundef7 (expression)
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
	      #+nil
	      (when *fpm?*
		(llvm:run-function-pass-manager *fpm* function))
	      function)
	    (progn
	      (llvm:delete-function function)
	      (when (binary-operator-p (prototype expression))
		(remhash (operator-name (prototype expression))
			 *binop-precedence*))))))))

;;; code generation 4

(defvar *fpm*)

;;; code generation 5

;;;5 6 7
(defmethod codegen ((expression if-expression))
  (let ((cond-v (codegen (_condition expression))))
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
  (ecase *chapter*
    ((5 6) (codegen56 expression))
    ((7) (codegen7 expression))))
;;;;5 6
(defun codegen56 (expression)
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

;;;;7
(defun codegen7 (expression)
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


;;; code generation 6
;;;;6 7
(defmethod codegen ((expression unary-expression))
  (let ((operand-v (codegen (operand expression))))
    (when operand-v
      (let ((f (llvm:named-function *module*
				    (format nil "unary~a"
					    (opcode expression)))))
	(unless f
	  (error 'kaleidoscope-error :message "Unknown unary operator"))
	(llvm:build-call *builder* f (list operand-v) "unop")))))

;;; code generation 7

(defun create-entry-block-alloca (function var-name)
  "Create an alloca instruction in the entry block of the function. This is used
   for mutable variables etc."
  (let ((tmp-b (llvm:make-builder)))
    ;; FIXME: this doesn't set the proper insertion point
    (llvm:position-builder tmp-b (llvm:entry-basic-block function))
    (llvm:build-alloca tmp-b (llvm:double-type) var-name)))

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

(defmethod create-argument-allocas ((expression prototype) f)
  (map nil
       (lambda (parameter argument)
         (let ((alloca (create-entry-block-alloca f argument)))
           (llvm:build-store *builder* parameter alloca)
           (setf (gethash argument *named-values*) alloca)))
       (llvm:params f) (arguments expression)))

(defparameter *fucking-modules* nil)
;;;;Toplevel
(defun initialize-module-and-pass-manager ()
  (let ((module (llvm:make-module "fuck you")))
    (setf (llvm::data-layout *module*)
	  (get-target-machine-data (kaleidoscope-get-target-machine)))
    (push module *fucking-modules*)
    (setf *module* module)))


(defun handle-definition ()
  (let ((function (parse-definition)))
    (if function
	(ecase *chapter*
	  ((2)
	   (format *output?* "Parsed a function definition~%"))
	  ((3 4 5 6 7)
	   (let ((lf (codegen function)))
	     (when lf
	       (format *output?* "Read function definition:")
	       (dump-value lf)))))
	(get-next-token))))

(defun handle-extern ()
  (let ((prototype (parse-extern)))
    (if prototype
	(ecase *chapter*
	  ((2)
	   (format *output?* "Parsed an extern~%"))
	  ((3 4 5 6 7)
	   (let ((function (codegen prototype)))
	     (when function
	       (format *output?* "Read extern: ")
	       (dump-value function)))))
	(get-next-token))))

(defun handle-top-level-expression ()
  "Evaluate a top-level expression into an anonymous function."
  (handler-case
      (let ((func-ast (parse-top-level-expression)))
	(ecase *chapter*
	  ((2)
	   (format *output?* "Parsed a top-level expr~%"))
	  ((3 4 5 6 7)
	   (let ((lf (codegen func-ast)))
	     (case *chapter*
	       ((3)
		(format *output?* "Read top-level expression:")
		(dump-value lf))
	       ((4 5 6 7)
		(case *chapter*
		  ((4 5)
		   (dump-value lf)))

		(let ((handle (kaleidoscope-add-module *module*)))
		  (initialize-module-and-pass-manager)
		  (let ((expr-symbol (kaleidoscope-find-symbol "__anon_expr")))
		    (when (cffi:null-pointer-p expr-symbol)
		      (error 'kaleidoscop-error :message "function not found"))
		    
		    (format *output?* "Evaluated to ~fD0"
			    (cffi:foreign-funcall-pointer
			     (get-symbol-address expr-symbol)
			     () :double)))
		  (kaleidoscope-remove-module handle))
		#+nil
		(let ((ptr (llvm:pointer-to-global *execution-engine* lf)))
		  (format *output?* "Evaluated to ~fD0"
			  (if (cffi:pointer-eq ptr lf) ; we have an interpreter
			      (llvm:generic-value-to-float
			       (llvm:double-type)
			       (llvm:run-function *execution-engine* ptr ()))
			      (cffi:foreign-funcall-pointer ptr () :double)))))
	       )))))
    (kaleidoscope-error (e)
      (get-next-token)
      (format *output?* "error: ~a~%" e))))

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

;;; top-level 4 5 6 7
(defvar *execution-engine*)

(defmacro with-chapter (n &body body)
  (once-only (n)
    `(let ((*chapter* ,n))
       (%with-tokens ,n ,@body))))

(defun resetstuff ()
  (setf *fucking-modules* nil)
  (clrhash *function-protos*))

(defun toplevel (n)
  (resetstuff)
  (with-chapter n
    (labels ((%start ()
	       (unwind-protect
		    (progn
		      (format *output?* "~&ready> ")
		      (reset-token-reader)
		      (get-next-token)
		      (set-binop-precedence)
		      (callcc (function main-loop)))
		 (dolist (module *fucking-modules*)
		   (llvm:dispose-module module))
		 (resetstuff)))
	     (start ()
	       (if *jit?*
		   (progn
		     (llvm::initialize-native-target?)
		     (llvm::initialize-native-Asm-parser)
		     (llvm::initialize-native-asm-printer)
		     (unwind-protect (kaleidoscope-create)
		       (%start)
		       (kaleidoscope-destroy)))
		   (%start))))
      (case *chapter*
	((2) (start))
	((3 4 5 6 7)x
	 (llvm:with-objects
	     ((*builder* llvm:builder))
	   (case *chapter*
	     ((3)
	      (start)
	      (dump-module *module*))
	     ((4 5 6 7)
	      (flet ((start2 ()
			 (start)
			 (dump-module *module*)))
		(start2)
		#+nil
		(llvm:with-objects ((*execution-engine* llvm:execution-engine *module*)
				    ;;(*myjit* llvm:jit-compiler *module*)
				    )
		  (if *fpm?*
		      (llvm:with-objects ((*fpm* llvm:function-pass-manager *module*))
			(llvm:add-target-data (llvm:target-data *execution-engine*) *fpm*)
			;;passes    
			(progn
			  (unless (= *chapter* 4)
			    (llvm:add-promote-memory-to-register-pass *fpm*))
			  (llvm:add-instruction-combining-pass *fpm*)
			  (llvm:add-reassociate-pass *fpm*)
			  (llvm:add-gvn-pass *fpm*)
			  (llvm:add-cfg-simplification-pass *fpm*))
			(llvm:initialize-function-pass-manager *fpm*)
			(start2))
		      (start2)))))))))))
  (values))
