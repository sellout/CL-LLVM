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
;;;;number-expression
(defmethod value ((sexp cons))
  (ecase (car sexp)
    ((number-expression)
     (second sexp))))
(defun make-number-expression (num)
  "for numeric literals like “1.0”."
  (list 'number-expression num))

;;;;variable expression
(defun make-variable-expression (num)
  "for referencing a variable, like “a”."
  (list 'variable-expression num))
(defmethod name ((expression cons))
  (ecase (car expression)
    ((variable-expression)
     (second expression))
    ((prototype)
     (second expression))))

;;;;binary expressions
(defun make-binary-expression (operator lhs rhs)
  "for a binary operator."
  (list 'binary-expression operator lhs rhs))
(defmethod operator ((expression cons))
  (ecase (car expression)
    ((binary-expression)
     (second expression))))
(defmethod lhs ((expression cons))
  (ecase (car expression)
    ((binary-expression)
     (third expression))))
(defmethod rhs ((expression cons))
  (ecase (car expression)
    ((binary-expression)
     (fourth expression))))

;;;;function calls
(defmethod callee ((expression cons))
  (ecase (car expression)
    ((call-expression)
     (second expression))))
(defmethod arguments ((expression cons))
  (ecase (car expression)
    ((call-expression)
     (third expression))
    ((prototype)
     (third expression))))
(defun make-call-expression (callee arguments)
  "for function calls."
  (list 'call-expression callee arguments))

;;;function definition
(defmethod prototype ((expression cons))
  (ecase (car expression)
    ((function-definition)
     (second expression))))
(defmethod body ((expression cons))
  (ecase (car expression)
    ((function-definition)
     (third expression))
    ((for-expression)
     (sixth expression))
    ((var-expression)
     (third expression))))
(defun make-function-definition (prototype body)
  "A function definition itself."
  (list 'function-definition prototype body))

;;;;prototype
(defun operatorp (sexp)
  (assert (eq 'prototype (car sexp)))
  (fourth sexp))
(defun precedence (sexp)
  (assert (eq 'prototype (car sexp))))
(defun make-prototype (&optional
			 (name "")
			 (arguments (make-array 0))
			 (operatorp nil) ;;;;added from 6 onward
			 (precedence 0) ;;;;added from 6 onward
			 )
  "The “prototype” for a function, which captures its
    name, and its argument names (thus implicitly the number of arguments the
    function takes)."
  (list 'prototype name arguments operatorp precedence))

;;;5 6 7
;;;;;if
(defclass if-expression (;expression
			 )
  ((_condition :initarg :_condition :reader _condition)
   (then :initarg :then :reader then)
   (else :initarg :else :reader else))
  (:documentation "Expression class for if/then/else."))
(defun make-if-expression (_condition then else)
  (list 'if-expression _condition then else))
(defclass for-expression (;expression
			  )
  ((var-name :initarg :var-name :reader var-name)
   (start :initarg :start :reader start)
   (end :initarg :end :reader end)
   ;; FIXME: why is CCL's conflicting STEP visible here?
   (step :initarg :step :reader step*)
   (body :initarg :body :reader body))
  (:documentation "Expression class for for/in."))
(defun make-for-expression (var-name start end step body)
  (list 'for-expression var-name start end step body))

;;;;6 7
(defclass unary-expression (;expression
			    )
  ((opcode :initarg :opcode :reader opcode)
   (operand :initarg :operand :reader operand))
  (:documentation "Expression class for a unary operator."))
(defun make-unary-expression (opcode operand)
  (list 'unary-expression opcode operand))

;;;for prototypes
(defun unary-operator-p (expression)
  (assert (eq 'prototype
	      (car expression)))
  (and (operatorp expression)
       (= (length (arguments expression))
	  1)))
;;;for prototypes
(defun binary-operator-p (expression)
  (assert (eq 'prototype
	      (car expression)))
  (and (operatorp expression)
       (= (length (arguments expression))
	  2)))
;;;for prototypes
(defun operator-name (expression)
  (assert (eq 'prototype
	      (car expression)))
  (assert (or (unary-operator-p expression)
	      (binary-operator-p expression)))
  (elt (name expression)
       (1- (length (name expression)))))

;;;7
#+nil
(defclass var-expression (;expression
			  )
  ((var-names :initarg :var-names :reader var-names)
   (body :initarg :body :reader body))
  (:documentation "Expression class for var/in"))
(defmethod var-names ((expression cons))
  (ecase (car expression)
    ((var-expression)
     (second expression))))
(defun make-var-expression (var-names body)
  (list 'var-expression var-names body))

;;;;Parser

;;; parser 2
(defvar *binop-precedence* (make-hash-table :size 4))
(defun get-precedence (token)
  (gethash token *binop-precedence* -1))
(defun parse-identifier-expression ()
  (let ((id-name *identifier-string*))
    (if (eql (get-next-token) #\()
	(prog2 (get-next-token) ; eat (
	    (make-call-expression 
	     id-name
	     (if (not (eql *current-token* #\)))
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
	(make-variable-expression id-name))))
(defun parse-number-expression ()
  (prog1 (make-number-expression *number-value*)
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
  (let ((token *current-token*))
    (cond
      ((eql token :tok-identifier)
       (parse-identifier-expression))
      ((eql token :tok-number)
       (parse-number-expression))
      ((eql token #\()
       (parse-paren-expression))
      ((and (eql token :tok-if)
	    (member *chapter* '(5 6 7) :test 'eql))
       (parse-if-expression))
      ((and (eql token :tok-for)
	    (member *chapter* '(6 7) :test 'eql))
       (parse-for-expression))
      ((and (eql token :tok-var)
	    (= *chapter* 7))
       (parse-var-expression))
      (t
       (error 'kaleidoscope-error
	      :message "unknown token when expecting an expression")))))

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
		      (make-binary-expression
		       binary-operator
		       lhs
		       rhs)))))))))

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
	  (make-prototype function-name arg-names)))
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
      (make-prototype
       function-name
       arg-names
       operator-arity
       binary-precedence))))

(defun parse-definition ()
  (get-next-token) ; eat def
  (let ((prototype (parse-prototype)))
    (if prototype
	(let ((expression (parse-expression)))
	  (if expression
	      (make-function-definition
	       prototype
	       expression))))))

(defparameter *name-counter* -1)
(defparameter *name* nil)
(defun update-name-counter ()
  (setf *name* (write-to-string (incf *name-counter*))))
(defun parse-top-level-expression ()
  (update-name-counter)
  (let ((expression (parse-expression)))
    (if expression
	(make-function-definition
	 (make-prototype *name*)
	 expression))))

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
	      (make-if-expression
	       _condition
	       then
	       else))))))))
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
		  (make-for-expression
		   id-name
		   start
		   end
		   step
		   body))))))))))

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
	    (make-unary-expression
	     opcode
	     operand))))))

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
        (make-var-expression var-names body)))))

;;;;code-generation

;;; code generation 3
(defparameter *module* nil)
(defvar *builder*)
(defvar *named-values*)
(defvar *depth* :top)

(defparameter *function-protos* (make-hash-table :test 'equal))
(defun get-function (name)
  (block nil
    (let ((f
	   (cffi:with-foreign-string (str name)
	     (llvm::-get-named-function *module* str))))
      (unless (cffi:null-pointer-p f)
	(return f)))
    (let ((previously-defined-fun (gethash name *function-protos*)))
      (when previously-defined-fun
	(let ((ir (codegen previously-defined-fun)))
	  (return ir))))
    (return nil)))


;;;(defmethod codegen ((expression number-expression)))
;;;(defmethod codegen ((expression variable-expression)))
;;;(defmethod codegen ((expression binary-expression))
;;;(defmethod codegen ((expression call-expression)))
;;;(defmethod codegen ((expression function-definition)))
;;;(defmethod codegen ((expression var-expression)))
;;;(defmethod codegen ((expression prototype)))
(defun codegen-number-expression (sexp)
  (llvm::-const-real (llvm::-double-type)
		     (coerce (value sexp)
			     'double-float)))
(defun codegen-variable-expression (sexp)
  (let ((v (gethash (name sexp)
		    *named-values*)))
    (if v
	(ecase *chapter*
	  ((3 4 5 6) v)
	  ((7)
	   (cffi:with-foreign-string (str (name sexp))
	     (llvm::-build-load *builder* v str))))
	(error 'kaleidoscope-error :message "unknown variable name"))))
(defun codegen-binary-expression (sexp)
  (if (and (= *chapter* 7)
	   (eql (operator sexp)
		#\=))
      ;; TODO: can we typecheck (lhs expression) here?
      (codegen-binary=expression sexp)
      (let ((l (codegen (lhs sexp)))
	    (r (codegen (rhs sexp))))
	(when (and l r)
	  (case (operator sexp)
	    (#\+ (cffi:with-foreign-string (str "addtmp")
		   (llvm::-build-f-add *builder* l r str)))
	    (#\- (cffi:with-foreign-string (str "subtmp")
		   (llvm::-build-f-sub *builder* l r str)))
	    (#\* (cffi:with-foreign-string (str "multmp")
		   (llvm::-build-f-mul *builder* l r str)))
	    (#\<
	     (cffi:with-foreign-strings ((cmptmp "cmptmp")
					 (booltmp "booltmp"))
	       (llvm::-build-u-i-to-f-p
		*builder*
		(llvm::-build-f-cmp
		 *builder*
		 :unordered-< l r
		 cmptmp)
		(llvm::-double-type)
		booltmp)))
	    (otherwise
	     (ecase *chapter*
	       ((3 4 5)
		(error 'kaleidoscope-error
		       :message "invalid binary operators"))
	       ((6 7)
		(let ((f (cffi:with-foreign-string (str (format nil "binary~a"
								(operator sexp)))
			   (llvm::-get-named-function *module* str))))
		  (assert f () "binary operator not found!")
		  (build-call *builder* f (list l r) "binop"))))))))))

(defmethod codegen ((expression cons))
  (ecase (car expression)
    ((number-expression)
     (codegen-number-expression expression))
    ((variable-expression)
     (codegen-variable-expression expression))
    ((binary-expression)
     (codegen-binary-expression expression))
    ((call-expression)
     (let ((callee (let ((*depth* :not-top))
		     (get-function					  
		      (callee expression)))))
       (if callee
	   (if (= (llvm::-count-params callee)
		  (length (arguments expression)))
					;(cffi:with-foreign-string (calltmp ))
	       (build-call
		*builder*
		callee
		(map 'vector #'codegen (arguments expression))
		"calltmp")
	       (error 'kaleidoscope-error :message "incorrect # arguments passed"))
	   (error 'kaleidoscope-error :message "unknown function referenced"))))
    ((var-expression)
     (let* ((function (llvm::-get-basic-block-parent
		       (llvm::-get-insert-block *builder*)))
	    (old-bindings (map 'vector
			       (lambda (var-binding)
				 (destructuring-bind (var-name . init) var-binding
				   (let ((alloca
					  (create-entry-block-alloca function
								     var-name)))
				     (llvm::-build-store
				      *builder*
				      (if init
					  ;; FIXME: handle error
					  (codegen init)
					  (llvm::-const-real
					   (llvm::-double-type)
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
    ((prototype)
     (let* ((doubles (make-array (length (arguments expression))
				 :initial-element (llvm::-double-type)))
	    (f-type (function-type (llvm::-double-type) doubles))
	    (function
	     (cffi:with-foreign-string (str (name expression))
	       (llvm::-add-function *module* str f-type))))
       ;;??? If F conflicted, there was already something named 'Name'.  If it has a
       ;;??? body, don't allow redefinition or reextern.
       #+nil
       (when (not (string= (cffi:foreign-string-to-lisp
			    (llvm::-get-value-name function))
			   (name expression)))
	 (llvm::-delete-function function)
	 (setf function
	       (cffi:with-foreign-string (str (name expression))
		 (llvm::-get-named-function *module* str))))
       ;; (inspect expression)
       ;; (print (name expression))
       ;; (terpri)
       (progn
	 ;;if (= (llvm::-count-basic-blocks function) 0)
	 (if (= (llvm::-count-params function)
		(length (arguments expression)))
	     (when (or (= *chapter* 7)
		       (boundp '*named-values*))
	       ;; Set names for all arguments.
	       (map nil
		    (lambda (argument name)
		      (cffi:with-foreign-string
			  (str name
			       #+nil
			       (concatenate 'string
					    (write-to-string
					     (incf
					      (car (load-time-value (list 0))))) name))
			(llvm::-set-value-name
			 argument
			 str))
		      (when (eq *depth* :top)
			(ecase *chapter*
			  ((3 4 5 6)
			   (setf (gethash name *named-values*)
				 argument))
			  ((7)))))
		    (params function)
		    (let ((a (arguments expression)))
					;(format t "~&~a~&" a)
		      a)))
	     (error 'kaleidoscope-error
		    :message "redefinition of function with different # args"))
	 #+nil
	 (error 'kaleidoscope-error :message "redefinition of function"))
       function))
    ((function-definition)
     (let ((prototype (prototype expression)))
       (setf (gethash (name prototype) *function-protos*)
	     prototype)
       (let* ((*named-values* (make-hash-table :test #'equal))
	      (function (get-function (name prototype))))
	 (when function
	   (ecase *chapter*
	     ((3 4 5)
	      (llvm::-position-builder-at-end
	       *builder*
	       (cffi:with-foreign-string (str "entry")
		 (llvm::-append-basic-block function str)))
	      (flet ((remove-function ()
		       (llvm::-delete-function function)
		       (format t "fuck me harder ~a" (name prototype))
		       (terpri)
		       (remhash (name prototype) *function-protos*)))
		(block nil
		  (let ((retval (codegen (body expression))))
		    (when retval
		      (build-ret *builder* retval)

		      (when (llvm::-verify-function
			     function
			     (cffi:foreign-enum-value
			      'llvm::|LLVMVerifierFailureAction|
			      'llvm::|LLVMPrintMessageAction|))
			(dump-value function)
			(remove-function)
			(error 'kaleidoscope-error
			       :message "Function verification failure."))
		      (unless (= *chapter* 3)
					;		     #+nil
			(when *fpm?*
			  (llvm::-run-function-pass-manager *fpm* function)))
		      (return function))
		    (remove-function)
		    nil))))
	     ((6)
	      ;; If this is an operator, install it.
	      (when (binary-operator-p (prototype expression))
		(setf (gethash (operator-name (prototype expression))
			       *binop-precedence*)
		      (precedence (prototype expression))))
	      (llvm::-position-builder-at-end
	       *builder*
	       (cffi:with-foreign-string (str "entry")
		 (llvm::-append-basic-block function str)))
	      (let ((retval (codegen (body expression))))
		(if retval
		    (progn
		      (build-ret *builder* retval)
		      (when (llvm::-verify-function
			     function
			     (cffi:foreign-enum-value
			      'llvm::|LLVMVerifierFailureAction|
			      'llvm::|LLVMPrintMessageAction|))
			(error 'kaleidoscope-error
			       :message "Function verification failure."))
					;		   #+nil
		      (when *fpm?*
			(llvm::-run-function-pass-manager *fpm* function))
		      function)
		    (progn
		      (llvm::-delete-function function)
		      (when (binary-operator-p (prototype expression))
			(remhash (operator-name (prototype expression))
				 *binop-precedence*))))))
	     ((7)
	      ;; If this is an operator, install it.
	      (when (binary-operator-p (prototype expression))
		(setf (gethash (operator-name (prototype expression))
			       *binop-precedence*)
		      (precedence (prototype expression))))
	      (llvm::-position-builder-at-end
	       *builder*
	       (cffi:with-foreign-string (str "entry")
		 (llvm::-append-basic-block function str)))
	      (create-argument-allocas (prototype expression) function)
	      (let ((retval (codegen (body expression))))
		(if retval
		    (progn
		      (build-ret *builder* retval)
		      (when (llvm::-verify-function
			     function
			     (cffi:foreign-enum-value
			      'llvm::|LLVMVerifierFailureAction|
			      'llvm::|LLVMPrintMessageAction|))
			(error 'kaleidoscope-error
			       :message "Function verification failure."))
					;		   #+nil
		      (when *fpm?*
			(llvm::-run-function-pass-manager *fpm* function))
		      function)
		    (progn
		      (llvm::-delete-function function)
		      (when (binary-operator-p (prototype expression))
			(remhash (operator-name (prototype expression))
				 *binop-precedence*)))))))))))))

(defun codegen-binary=expression (expression)
  (let ((lhse (lhs expression))
	(val (codegen (rhs expression))))
    (when val
      (let ((variable (gethash (name lhse) *named-values*)))
	(unless variable
	  (error 'kaleidoscope-error :message "Unknown variable name"))
	(llvm::-build-store *builder* val variable)
	val))))

;;; code generation 4

(defvar *fpm*)

;;; code generation 5

;;;5 6 7
(defmethod codegen ((expression if-expression))
  (let ((cond-v (codegen (_condition expression))))
    (when cond-v
      (setf cond-v
	    (cffi:with-foreign-string (str "ifcond")
	      (llvm::build-f-cmp
	       *builder* 
	       'llvm::|LLVMIntNE|
	       cond-v
	       (const-real (llvm::-double-type) 0)
	       str)))
      (let* ((function (llvm::-get-basic-block-parent  
                        (llvm::-get-insert-block *builder*)))
             (then-bb
	      (cffi:with-foreign-string (str "then")
		(llvm::-append-basic-block function str)))
             ;; FIXME: not sure if we can append these at this point
             (else-bb
	      (cffi:with-foreign-string (str "else")
		(llvm::-append-basic-block function str)))
             (merge-bb
	      (cffi:with-foreign-string (str "ifcont")
		(llvm::-append-basic-block function str))))
        (llvm::-build-cond-br *builder* cond-v then-bb else-bb)
        (position-builder *builder* then-bb)
        (let ((then-v (codegen (then expression))))
          (when then-v
            (llvm::-build-br *builder* merge-bb)
            ;; Codegen of 'Then' can change the current block, update THEN-BB
            ;; for the PHI.
            (setf then-bb (llvm::-get-insert-block *builder*))
            (position-builder *builder* else-bb)
            (let ((else-v (codegen (else expression))))
              (when else-v
                (llvm::-build-br *builder* merge-bb)
                ;; Codegen of 'Else' can change the current block, update
                ;; ELSE-BB for the PHI.
                (setf else-bb (llvm::-get-insert-block *builder*))
                ;; Emit merge block.
                (position-builder *builder* merge-bb)
                (let ((pn (cffi:with-foreign-string (str "iftmp")
			    (llvm::-build-phi
			     *builder*
			     (llvm::-double-type)
			     str))))
                  (add-incoming pn
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
      (let* ((preheader-bb (llvm::-get-insert-block *builder*))
	     (function (llvm::-get-basic-block-parent preheader-bb))
	     (loop-bb
		(cffi:with-foreign-string (str "loop")
		  (llvm::-append-basic-block function str))))
	(llvm::-build-br *builder* loop-bb)
	(position-builder *builder* loop-bb)
	(let ((variable
	       (cffi:with-foreign-string (str (var-name expression))
		 (llvm::-build-phi
		  *builder*
		  (llvm::-double-type)
		  str))))
	  (add-incoming variable
			(list start-val)
			(list preheader-bb))
	  (let ((old-val (gethash (var-name expression) *named-values*)))
	    (setf (gethash (var-name expression) *named-values*) variable)
	    (when (codegen (body expression))
	      (let ((step-val (if (step* expression)
				  (codegen (step* expression))
				  (const-real (llvm::-double-type) 1))))
		(when step-val
		  (let ((next-var
			 (cffi:with-foreign-string (str "nextvar")
			   (llvm::-build-f-add
			    *builder*
			    variable
			    step-val
			    str)))
			(end-cond (codegen (end expression))))
		    (when end-cond
		      (setf end-cond
			    (llvm::-build-f-cmp
			     *builder*
			     (cffi:foreign-enum-value 'llvm::|LLVMRealPredicate|
						      'llvm::|LLVMRealONE|)
			     end-cond
			     (const-real
			      (llvm::-double-type)
			      0)
			     "loopcond"))
		      (let ((loop-end-bb (llvm::-get-insert-block *builder*))
			    (after-bb (cffi:with-foreign-string (str "afterloop")
					  (llvm::-append-basic-block
					   function
					   str))))
			(llvm::-build-cond-br *builder* end-cond loop-bb after-bb)
			(position-builder *builder* after-bb)
			(add-incoming variable
				      (list next-var)
				      (list loop-end-bb))
			(if old-val
			    (setf (gethash (var-name expression)
					   *named-values*)
				  old-val)
			    (remhash (var-name expression)
				     *named-values*))
			;; for expr always returns 0.
			(llvm::-const-null (llvm::-double-type))))))))))))))

;;;;7
(defun codegen7 (expression)
  (let* ((function (llvm::-get-basic-block-parent
		    (llvm::-get-insert-block *builder*)))
	 (alloca (create-entry-block-alloca function (var-name expression)))
	 (start-val (codegen (start expression))))
    (when start-val
      (llvm::-build-store *builder* start-val alloca)
      ;; Make the new basic block for the loop header, inserting after current
      ;; block.
      (let* ((loop-bb
		(cffi:with-foreign-string (str "loop")
		  (llvm::-append-basic-block function str))))
	(llvm::-build-br *builder* loop-bb)
	(position-builder *builder* loop-bb)
	(let ((old-val (gethash (var-name expression) *named-values*)))
	  (setf (gethash (var-name expression) *named-values*) alloca)
	  (when (codegen (body expression))
	    (let ((step-val (if (step* expression)
				(codegen (step* expression))
				(llvm::-const-real (llvm::-double-type) 1))))
	      (when step-val
		(let ((end-cond (codegen (end expression))))
		  (when end-cond
		    (let* ((cur-var
			    (cffi:with-foreign-string (str (var-name expression))
			      (llvm::-build-load
			       *builder*
			       alloca
			       str)))
			   (next-var
			    (cffi:with-foreign-string (str "nextvar")
			      (llvm::-build-f-add
			       *builder*
			       cur-var
			       step-val
			       str))))
		      (llvm::-build-store *builder* next-var alloca)
		      (setf end-cond
			    (llvm::-build-f-cmp
			     *builder*
			     (cffi:foreign-enum-value
			      '|LLVMRealPredicate|
			      '|LLVMRealONE|)
			     end-cond
			     (llvm::-const-real
			      (llvm::-double-type)
			      0)
			     "loopcond"))
		      (let ((after-bb
			     (cffi:with-foreign-string (str "afterloop")
			       (llvm::-append-basic-block
				function
				str))))
			(llvm::-build-cond-br *builder* end-cond loop-bb after-bb)
			(position-builder *builder* after-bb)
			(if old-val
			    (setf (gethash (var-name expression) *named-values*)
				  old-val)
			    (remhash (var-name expression) *named-values*))
			;; for expr always returns 0.
			(llvm::-const-null (llvm::-double-type))))))))))))))


;;; code generation 6
;;;;6 7
(defmethod codegen ((expression unary-expression))
  (let ((operand-v (codegen (operand expression))))
    (when operand-v
      (let ((f (cffi:with-foreign-string (str (format nil "unary~a"
						      (opcode expression)))
		 (llvm::-get-named-function
		  *module*
		  ))))
	(unless f
	  (error 'kaleidoscope-error :message "Unknown unary operator"))
	(build-call *builder* f (list operand-v) "unop")))))

;;; code generation 7

(defun create-entry-block-alloca (function var-name)
  "Create an alloca instruction in the entry block of the function. This is used
   for mutable variables etc."
  (let ((tmp-b (llvm::-create-builder)))
    ;; FIXME: this doesn't set the proper insertion point
    (position-builder tmp-b (llvm::-get-entry-basic-block function))
    (cffi:with-foreign-string (str var-name)
      (llvm::-build-alloca tmp-b (llvm::-double-type) str))))


(defun create-argument-allocas (expression f)
  (assert (eq 'prototype
	      (car expression)))
  (map nil
       (lambda (parameter argument)
         (let ((alloca (create-entry-block-alloca f argument)))
           (llvm::-build-store *builder* parameter alloca)
           (setf (gethash argument *named-values*) alloca)))
       (params f)
       (arguments expression)))

(defparameter *fucking-modules* nil)
;;;;Toplevel
(defun initialize-module-and-pass-manager ()
  (let ((module (cffi:with-foreign-string (str "fuck you")
		  (llvm::-module-create-with-name str))))
    (setf *module* module)
    (let ((target (kaleidoscope-get-target-machine)))
      #+nil
      (let ((msg (get-target-machine-triple target)))
	(print (cffi:foreign-string-to-lisp msg))
	(dispose-message msg))
      (llvm::-set-data-layout
       module
       (llvm::-get-target-machine-data
	target)))
    (push module *fucking-modules*)

    (let ((fpm (llvm::-create-function-pass-manager-for-module module)))
      (progn
	(unless (= *chapter* 4)
	  (llvm::-add-promote-memory-to-register-pass fpm))
	(llvm::-add-instruction-combining-pass fpm)
	(llvm::-add-reassociate-pass fpm)
	(llvm::-add-g-v-n-pass fpm)
	(llvm::-add-c-f-g-simplification-pass fpm))
     (llvm::-initialize-function-pass-manager fpm)
     (setf *fpm* fpm))))


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
	       (dump-value lf)
	       (kaleidoscope-add-module *module*)
	       (initialize-module-and-pass-manager)))))
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
	       (dump-value function)
	       (setf (gethash (name prototype) *function-protos*)
		     prototype)))))
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
		(let ((old *module*))
		  (pop *fucking-modules*)
		  (let ((handle (kaleidoscope-add-module old)))
					;		  (print 123)
		    (let ((expr-symbol
			   (cffi:with-foreign-string (str *name*)
			     (kaleidoscope-find-symbol str))))
					;		    (print expr-symbol)
		      (when (cffi:null-pointer-p expr-symbol)
			(error 'kaleidoscope-error :message "function not found"))

					;		    (print 34234)
		      (let ((ptr (kaleidoscope-get-symbol-address expr-symbol)))
					;		      (print ptr)
					;		      (print 234234)
			(if (= 0 ptr)
			    (error 'kaleidoscope-error :message "function no body???")
			    (let ((result
				   (cffi:foreign-funcall-pointer
				    (cffi:make-pointer ptr) 
				    () :double)))
			      (format *output?* "Evaluated to ~fD0"
				      result)))))
					;(print 2323234242342434)
		    (llvm::-dispose-module old)
		    (kaleidoscope-remove-module handle)
		    (remhash *name* *function-protos*)
		    (initialize-module-and-pass-manager)
		    ))
		#+nil
		(let ((ptr (llvm::-get-pointer-to-global *execution-engine* lf)))
		  (format *output?* "Evaluated to ~fD0"
			  (if (cffi:pointer-eq ptr lf) ; we have an interpreter
			      (llvm::-generic-value-to-float
			       (llvm::-double-type)
			       (let ((args ()))
				 (let ((len (length args)))
				   (cffi:with-foreign-object (var 'llvm::|LLVMGenericValueRef| len)
				     (dotimes (index len)
				       (setf (cffi:mem-aref var 'llvm::|LLVMGenericValueRef| index)
					     (elt args index)))
				     (llvm::-run-function *execution-engine* ptr len var)))))
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
  (clrhash *function-protos*)
  (setf *name-counter* -1))

(defun toplevel (n)
  (resetstuff)
  (with-chapter n
    (labels ((%start ()
	       (unwind-protect
		    (progn
		      (setf *builder* (llvm::-create-builder))
		      (format *output?* "~&ready> ")
		      (reset-token-reader)
		      (get-next-token)
		      (set-binop-precedence)
		      (when *jit?*
			(initialize-module-and-pass-manager))
		      (callcc (function main-loop)))
		 ;;destroyed on jit destruction?
		 #+nil
		 (dolist (module *fucking-modules*)
		   (llvm::-dispose-module module))
		 (llvm::-dispose-builder *builder*)
					;(resetstuff)
		 )))
      (if *jit?*
	  (progn
	    (llvm::initialize-native-target?)
	    (llvm::initialize-native-Asm-parser)
	    (llvm::initialize-native-asm-printer)
	    (unwind-protect
		 (progn (kaleidoscope-create)
			(%start)
			(dump-module *module*))
	      (kaleidoscope-destroy)))
	  (%start))
      (values))))
