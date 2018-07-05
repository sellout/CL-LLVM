(in-package :k-shared)

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
		       :prototype (make-instance 'prototype)
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
