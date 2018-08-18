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

(defun identifier-string-to-enum (&optional (identifier-string *identifier-string*)
				    (token-types *token-types*))
  (let ((cell (assoc identifier-string token-types :test (function string=))))
    (if cell
	(cdr cell)
	':tok-identifier)))

(defparameter *last-char* #\space)
(defparameter *comment-buffer* (make-array 0 :adjustable t :fill-pointer 0))
(defparameter *comments* nil)
(defun reset-token-reader ()
  (setf *last-char* #\space))
(defparameter *comment-strings* t)
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
	   (when *comment-strings*
	     (setf (fill-pointer *comment-buffer*) 0))
	   (block out
	     (loop 
		(setf *last-char* (get-char))
		(when 
		    (find *last-char* '(nil #\linefeed #\return))
		  (return-from out))
		(when *comment-strings*
		  (vector-push-extend *last-char* *comment-buffer*))))
	   (push
	    (map 'string #'identity *comment-buffer*)
	     *comments*)
	   (if (null *last-char*)
	       ':tok-eof
	       (read-token token-types stream)))
	  (t
	   (let ((this-char *last-char*))
	     (setf *last-char* (get-char))
	     this-char)))))

(defvar *current-token*)
(defun get-next-token (&optional (token-types *token-types*) (stream *input?*))
  (setf *current-token* (read-token token-types stream)))

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
	    (member *chapter* '(5 6 7) :test 'eql))
       (parse-for-expression))
      ((and (eql token :tok-var)
	    (= *chapter* 7))
       (parse-var-expression))
      (t
       (error 'kaleidoscope-error
	      :message
	      (format nil "unknown token when expecting an expression: ~s" token))))))

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

(progn
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
	   expression)))))

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

;;;The old main loop which does not separate reading from compiling and executing
(defparameter *doing-stuff* nil)
(defparameter *interactive* nil)
(progn
  (defun main-loop (exit)
    (do ()
	((main-loop-end))
      (per-loop exit)))
  (defun main-loop-end ()
    (eql *current-token* ':tok-eof))
  (defun per-loop (exit)
    (when (and *interactive*
	       (not *compile-to-object-code?*))
      (format *output?* "~&ready> "))
    (when (and (not *interactive*)
	       *comments*)
      (dump-ast2 `(%comment ,(nreverse *comments*)))
      (setf *comments* nil))
    (handler-case
	(case *current-token*
	  (#\; (get-next-token))
	  (:tok-def
	   ;;handle-definition
	   (let ((function-ast (parse-definition)))
	     (if function-ast
		 (cond (*doing-stuff*
			(dump-ast function-ast)
			(%handle-definition function-ast))
		       (t (dump-ast2 `(%defun ,function-ast))))
		 (get-next-token))))
	  (:tok-extern
	   ;;handle-extern
	   (let ((prototype (parse-extern)))
	     (if prototype
		 (cond (*doing-stuff*
			(dump-ast prototype)
			(%handle-extern prototype))
		       (t (dump-ast2 `(%extern ,prototype))))
		 (get-next-token))))
	  (:tok-quit (funcall exit))
	  (otherwise
	   ;;handle-top-level-expression
	   (handler-case
	       (let ((ast (parse-top-level-expression)))
		 (cond (*doing-stuff*
			(dump-ast ast)
			(%handle-top-level-expression ast))
		       (t (dump-ast2 `(%toplevel ,ast)))))
	     (kaleidoscope-error (e)
	       (get-next-token)
	       (format *output?* "error: ~a~%" e)))
	   ))
      (kaleidoscope-error (e) (format *output?* "error: ~a~%" e)))))

;;;;automatically convert "kaleidoscope" syntax code into asts
(defun test2 (&optional (n *chapter*))
  (let ((str (write-to-string n)))
    (flet ((%test (out in toplevel)
	     (let ((k-shared::*ast2-stuff* nil))
	       (with-output-to-string (stream)
		 (let ((input-file-name (merge-pathnames in *test-directory*))
		       (output-file-name (merge-pathnames out *test-directory*)))
		   (with-open-file (file input-file-name)
		     (let ((*input?* file))
		       (funcall toplevel)))
		   (with-open-file (file output-file-name :direction :output :if-exists :append)
		     (let ((*print-case* :downcase))
		       (print `(define-chapter-test ,n ,(nreverse k-shared::*ast2-stuff*)) file))))))))
      (%test
       "testcases.lisp"
       (concatenate 'string "chapter" str ".k")
       (lambda () (toplevel n)))))
  (values))

;;;;reset toplevel name counter -> used to run every initialization of the toplevel
(setf *name-counter* -1)
(*token-types* (chap-tokens n)) ;;;; -> bound at toplevel initialization
(reset-token-reader)
(get-next-token)
(set-binop-precedence)

;;; install standard binary operators
;;; 1 is lowest precedence
(defun set-binop-precedence (&optional (n *chapter*))
  (case n
    ((7) (setf (gethash #\= *binop-precedence*) 2)))
  (setf (gethash #\< *binop-precedence*) 10
	(gethash #\+ *binop-precedence*) 20
	(gethash #\- *binop-precedence*) 30
	(gethash #\* *binop-precedence*) 40))

;;;;for removing binop function definitions in codegen-function-definition
(case *chapter*
  ((6 7)
   (when (binary-operator-p prototype)
     (remhash (operator-name prototype)
	      *binop-precedence*))))

;;;;for install binop functions
(case *chapter*
  ((6 7)
   ;; If this is an operator, install it.
   (when (binary-operator-p prototype)
     (setf (gethash (operator-name prototype)
		    *binop-precedence*)
	   (prototype.precedence prototype)))))


;;;;symbols exported from k-shared
;;;lexer
(:export
 :*identifier-string*
 :*number-value*
 :*current-token*
 :*token-types*
 :get-next-token
 :reset-token-reader
 :chap-tokens
 :with-tokens)

;;;parser
(:export
 :*binop-precedence*
 :get-precedence
 :parse-identifier-expression
 :parse-number-expression
 :parse-paren-expression
 :parse-primary
 :parse-prototype
 :parse-bin-op-rhs
 :parse-expression
 :parse-definition
 :parse-top-level-expression
 :parse-extern
 :parse-if-expression
 :parse-for-expression
 :parse-unary
 :parse-var-expression)

:set-binop-precedence
