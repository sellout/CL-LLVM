(in-package :k-shared)

(declaim (optimize (speed 0) (safety 3) (debug 3)))

(defun doublify (x)
  (coerce x 'double-float))

;;;;AST

(defmacro define-ast-node
    ((name &optional (conc-name nil given?)) &rest parameters)
  `(defstruct
       (,name
	 (:type list)
	 :named
	 (:conc-name
	  ,(if given?
	       conc-name
	       (symbolicate2 (list name "."))))
	 (:constructor
	  ,(symbolicate2 (list 'make- name))
	  (&optional
	   ,@(mapcar (lambda (x)
		       (cond ((and x (symbolp x)) x)
			     ((listp x)
			      (list (first x)
				    (second x)))
			     (t (error "not legal"))))
		     parameters))))
     ,@parameters))

;;; (2 3 4 5 6 7)
;;;;number-expression
(define-ast-node (number-expression) value)

;;;;variable expression
(define-ast-node (variable-expression) name)

;;;;binary expressions
;;;  "for a binary operator."
(define-ast-node (binary-expression) operator lhs rhs)

;;;;function calls
;;; "for function calls."
(define-ast-node (call-expression) callee arguments)

;;;function definition
;;;"A function definition itself."
(define-ast-node (function-definition) prototype body)

;;;;prototype
#+nil
"The “prototype” for a function, which captures its
    name, and its argument names (thus implicitly the number of arguments the
    function takes)."
(define-ast-node (prototype)
    (name "")
  (arguments (make-array 0))
  (operatorp nil) ;;;;added from 6 onward
  (precedence 0) ;;;;added from 6 onward
  )

;;;for prototypes
(defun unary-operator-p (expression)
  (assert (prototype-p expression))
  (and (prototype.operatorp expression)
       (= (length (prototype.arguments expression))
	  1)))
;;;for prototypes
(defun binary-operator-p (expression)
  (assert (prototype-p expression))
  (and (prototype.operatorp expression)
       (= (length (prototype.arguments expression))
	  2)))
;;;for prototypes
(defun operator-name (expression)
  (assert (prototype-p expression))
  (assert (or (unary-operator-p expression)
	      (binary-operator-p expression)))
  (elt (prototype.name expression)
       (1- (length (prototype.name expression))))) ;;;;works with characters?

;;;5 6 7
;;;;;if
(define-ast-node (if-expression) _condition then else)

;;;;for
;;;"for for/in."
(define-ast-node (for-expression) var-name start end step body)

;;;;6 7
;;;;unary expression
;;;;"for a unary operator."
(define-ast-node (unary-expression) opcode operand)

;;;7
;;;;mutable variables?
;;;;"for var/in"
(define-ast-node (var-expression) var-names body)

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
;;;(defmethod codegen ((expression if-expression)))
;;;(defmethod codegen ((expression for-expression)))
(defun codegen (expression)
  (let ((token (car expression)))
    (cond
      ((eql token 'number-expression)
       (codegen-number-expression expression))
      ((eql token 'variable-expression)
       (codegen-variable-expression expression))
      ((eql token 'binary-expression)
       (codegen-binary-expression expression))
      ((eql token 'call-expression)
       (codegen-call-expression expression))
      ((eql token 'var-expression)
       (codegen-var-expression expression))
      ((eql token 'prototype)
       (codegen-prototype expression))
      ((eql token 'function-definition)
       (codegen-function-definition expression))
      ((and (eql token 'if-expression)
	    (member *chapter* '(5 6 7) :test 'eql))
       (codegen-if-expression expression))
      ((and (eql token 'for-expression)
	    (member *chapter* '(5 6 7) :test 'eql))
       (codegen-for-expression expression))
      ((and (eql token 'unary-expression)
	    (member *chapter* '(6 7) :test 'eql))
       (codegen-unary-expression expression))
      (t (error "codegen not applicaple")))))

(defun codegen-number-expression (sexp)
  (llvm::-const-real (llvm::-double-type)
		     (doublify (number-expression.value sexp))))
(defun codegen-variable-expression (sexp)
  ;;// Look this variable up in the function.
  (let ((name (variable-expression.name sexp)))
    (let ((value (gethash name *named-values*)))
      (if (and value
	       (not (cffi:null-pointer-p value)))
	  (ecase *chapter*
	    ((3 4 5 6) value)
	    ((7)
	     ;;(print name *output?*)
	     ;;// Load the value.
	     (cffi:with-foreign-string (str name)
	       (llvm::-build-load *builder* value str))))
	  (error 'kaleidoscope-error :message "unknown variable name")))))
(defun codegen-binary-expression (sexp)
  (if (and (= *chapter* 7)
	   (eql (binary-expression.operator sexp)
		#\=))
      ;; TODO: can we typecheck (lhs expression) here?
      (codegen-binary=expression sexp)
      (let ((l (codegen (binary-expression.lhs sexp)))
	    (r (codegen (binary-expression.rhs sexp))))
	(when (and l r)
	  (case (binary-expression.operator sexp)
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
		 (cffi:foreign-enum-value
		  'llvm::|LLVMRealPredicate|
		  'llvm::|LLVMRealULT|)
		 ;;:unordered-<
		 l r
		 cmptmp)
		(llvm::-double-type)
		booltmp)))
	    (otherwise
	     (ecase *chapter*
	       ((3 4 5)
		(error 'kaleidoscope-error
		       :message "invalid binary operators"))
	       ((6 7)
		(let ((fun-name (format nil "binary~a"
					(binary-expression.operator sexp))))
		  ;;		  (print fun-name)
		  (let ((f
			 (get-function fun-name)
			  #+nil
			 (cffi:with-foreign-string
			     (str fun-name)
			   (llvm::-get-named-function *module* str))))
		    
		    (assert (and f
				 (not (cffi:null-pointer-p f)))
			    () "binary operator not found!")
		    (build-call *builder* f (list l r) "binop")))))))))))
(defun codegen-binary=expression (expression)
  (let ((lhse (binary-expression.lhs expression)))
    (if (variable-expression-p lhse)

	;;// Codegen the RHS.
	(let ((val (codegen (binary-expression.rhs expression))))
	  (when val
	    ;;; // Look up the name.
	    (let ((variable
		   (gethash (variable-expression.name lhse)
			    *named-values*)))
	      (unless variable
		(error 'kaleidoscope-error :message "Unknown variable name"))
	      (llvm::-build-store *builder* val variable)
	      val)))
	(error 'kaleidoscope-error :message "destination of '=' must be a variable"))))

(defun codegen-call-expression (sexp)
  (let ((callee (let ((*depth* :not-top))
		  (get-function					  
		   (call-expression.callee sexp)))))
    (if callee
	(if (= (llvm::-count-params callee)
	       (length (call-expression.arguments sexp)))
	    (build-call
	     *builder*
	     callee
	     (map 'vector #'codegen (call-expression.arguments sexp))
	     "calltmp")
	    (error 'kaleidoscope-error :message "incorrect # arguments passed"))
	(error 'kaleidoscope-error :message "unknown function referenced"))))

(defun codegen-var-expression (sexp)
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
					(doublify 0)))
				   alloca)
				  (prog1 (gethash var-name *named-values*)
				    (setf (gethash var-name *named-values*)
					  alloca)))))
			    (var-expression.var-names sexp)))
	 (body-val (codegen (var-expression.body sexp))))
    (when body-val
      (map 'vector
	   (lambda (var-binding old-binding)
	     (setf (gethash (car var-binding) *named-values*) old-binding))
	   (var-expression.var-names sexp) old-bindings)
      body-val)))

(defun codegen-prototype (sexp)
  (let* ((doubles (make-array (length (prototype.arguments sexp))
			      :initial-element (llvm::-double-type)))
	 (f-type (function-type (llvm::-double-type) doubles))
	 (function
	  (cffi:with-foreign-string (str (prototype.name sexp))
	    (llvm::-add-function *module* str f-type))))
    (map nil
	 (lambda (argument name)
	   (cffi:with-foreign-string
	       (str name)
	     (llvm::-set-value-name
	      argument
	      str)))
	 (params function)
	 (let ((a (prototype.arguments sexp)))
					;(format t "~&~a~&" a)
	   a))

    ;;;FIXME: does the commented code below have any use?
    
    ;;??? If F conflicted, there was already something named 'Name'.  If it has a
    ;;??? body, don't allow redefinition or reextern.
    #+nil
    (when (not (string= (cffi:foreign-string-to-lisp
			 (llvm::-get-value-name function))
			(prototype.name sexp)))
      (llvm::-delete-function function)
      (setf function
	    (cffi:with-foreign-string (str (prototype.name sexp))
	      (llvm::-get-named-function *module* str))))
    ;; (inspect sexp
    ;; (print (prototype.name sexp
    ;; (terpri)
    (progn
      ;;if (= (llvm::-count-basic-blocks function) 0)
      (progn
       #+nil if
	     #+nil
	     (= (llvm::-count-params function)
		(length (prototype.arguments sexp)))
	     ;; Set names for all arguments.

	  #+nil
	  (error 'kaleidoscope-error
		 :message "redefinition of function with different # args"))
      #+nil
      (error 'kaleidoscope-error :message "redefinition of function"))
    function))

(defun codegen-function-definition (sexp)
  (let* ((prototype (function-definition.prototype sexp))
	 (name (prototype.name prototype)))
    (setf (gethash name *function-protos*)
	  prototype)
    (let* ((*named-values* (make-hash-table :test #'equal))
	   (function (get-function name)))
      (when function
	;;// Create a new basic block to start insertion into.
	(llvm::-position-builder-at-end
	 *builder*
	 (cffi:with-foreign-string (str "entry")
	   (llvm::-append-basic-block function str)))
	(ecase *chapter*
	  ((3 4 5 6 7)
	   (flet ((remove-function ()
		    (llvm::-delete-function function)
		    (format t "fuck me harder ~a" name)
		    (terpri)
		    (remhash name *function-protos*)))
	     ;;// Record the function arguments in the NamedValues map.
	     (case *chapter*
	       ((2 3 4 5 6)
		(map nil
		     (lambda (argument name)
		       (setf (gethash name *named-values*)
			     argument))
		     (params function)
		     (let ((a (prototype.arguments prototype)))
					;(format t "~&~a~&" a)
		       a)))
	       ((7)
		 (create-argument-allocas prototype function)))
	     (block nil
	       (let ((retval (codegen (function-definition.body sexp))))
		 (when retval
		   ;;// Finish off the function.
		   (build-ret *builder* retval)
		   
		     ;;// Validate the generated code, checking for consistency.
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
		     ;;// Run the optimizer on the function.
		     (when (and *fpm?*
				(not *compile-to-object-code?*))
		       (llvm::-run-function-pass-manager *fpm* function)))
		   (return function))
		 (remove-function)
		 nil)))))))))

;;; code generation 4

(defvar *fpm*)

;;; code generation 5

;;;5 6 7
(defun codegen-if-expression (expression)
  (let ((cond-v (codegen (if-expression._condition expression))))
    (when cond-v

      ;; Convert condition to a bool by comparing equal to 0.0.
      (setf cond-v
	    (cffi:with-foreign-string (str "ifcond")
	      (llvm::-build-f-cmp
	       *builder* 
	       (cffi:foreign-enum-value
		'llvm::|LLVMRealPredicate|
		'llvm::|LLVMRealONE|)
	       cond-v
	       (const-real (llvm::-double-type) (doublify 0))
	       str)))

      ;; Create blocks for the then and else cases.  Insert the 'then' block at the
      ;; end of the function.
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

	;;Emit then value.
        (position-builder *builder* then-bb)
        (let ((then-v (codegen (if-expression.then expression))))
          (when then-v
            (llvm::-build-br *builder* merge-bb)
            ;; Codegen of 'Then' can change the current block, update THEN-BB
            ;; for the PHI.
            (setf then-bb (llvm::-get-insert-block *builder*))

	    ;;Emit else block.
            (position-builder *builder* else-bb)
            (let ((else-v (codegen (if-expression.else expression))))
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

(defun codegen-for-expression (expression)
  (ecase *chapter*
    ((5 6) (codegen56 expression))
    ((7) (codegen7 expression))))
;;;;5 6
(defun codegen56 (expression)
  (let ((start-val (codegen (for-expression.start expression))))
    (when start-val
      ;; Make the new basic block for the loop header, inserting after current
      ;; block.
      (let* ((preheader-bb (llvm::-get-insert-block *builder*))
	     (function (llvm::-get-basic-block-parent preheader-bb))
	     (loop-bb
		(cffi:with-foreign-string (str "loop")
		  (llvm::-append-basic-block function str))))
	;;Insert an explicit fall through from the current block to the LoopBB.
	(llvm::-build-br *builder* loop-bb)

	(position-builder *builder* loop-bb)

	;;Start the PHI node with an entry for Start.
	(let ((variable
	       (cffi:with-foreign-string (str (for-expression.var-name expression))
		 (llvm::-build-phi
		  *builder*
		  (llvm::-double-type)
		  str))))
	  (add-incoming variable
			(list start-val)
			(list preheader-bb))

	  ;; Within the loop, the variable is defined equal to the PHI node.  If it
	  ;; shadows an existing variable, we have to restore it, so save it now.
	  (let ((old-val (gethash (for-expression.var-name expression) *named-values*)))
	    (setf (gethash (for-expression.var-name expression) *named-values*) variable)

	    ;; Emit the body of the loop.  This, like any other expr, can change the
	    ;; current BB.  Note that we ignore the value computed by the body, but don't
	    ;; allow an error.
	    (when (codegen (for-expression.body expression))

	      ;;Emit the step value.
	      (let ((step-val
		     (if (for-expression.step expression)
			 (codegen (for-expression.step expression))
			 ;;// If not specified, use 1.0.
			 (const-real (llvm::-double-type) 1))))
		(when step-val
		  (let ((next-var
			 (cffi:with-foreign-string (str "nextvar")
			   (llvm::-build-f-add
			    *builder*
			    variable
			    step-val
			    str)))
			;;// Compute the end condition.
			(end-cond (codegen (for-expression.end expression))))
		    (when end-cond
		      (cffi:with-foreign-string (str "loopcond")
			(setf end-cond
			      (llvm::-build-f-cmp
			       *builder*
			       (cffi:foreign-enum-value 'llvm::|LLVMRealPredicate|
							'llvm::|LLVMRealONE|)
			       end-cond
			       (const-real
				(llvm::-double-type)
				(doublify 0))
			       str)))

		      ;;// Create the "after loop" block and insert it.
		      (let ((loop-end-bb (llvm::-get-insert-block *builder*))
			    (after-bb (cffi:with-foreign-string (str "afterloop")
					  (llvm::-append-basic-block
					   function
					   str))))

			;;// Insert the conditional branch into the end of LoopEndBB.
			(llvm::-build-cond-br *builder* end-cond loop-bb after-bb)

			;;// Any new code will be inserted in AfterBB.
			(position-builder *builder* after-bb)

			;;// Add a new entry to the PHI node for the backedge.
			(add-incoming variable
				      (list next-var)
				      (list loop-end-bb))

			;;// Restore the unshadowed variable.
			(if old-val
			    (setf (gethash (for-expression.var-name expression)
					   *named-values*)
				  old-val)
			    (remhash (for-expression.var-name expression)
				     *named-values*))
			;;// for expr always returns 0.0
			(llvm::-const-null (llvm::-double-type))))))))))))))

;;;;7
(defun codegen7 (expression)
  (let* ((function (llvm::-get-basic-block-parent
		    (llvm::-get-insert-block *builder*)))
	 (alloca (create-entry-block-alloca
		  function
		  (for-expression.var-name expression)))
	 (start-val (codegen (for-expression.start expression))))
    (when start-val
      (llvm::-build-store *builder* start-val alloca)
      ;; Make the new basic block for the loop header, inserting after current
      ;; block.
      (let* ((loop-bb
		(cffi:with-foreign-string (str "loop")
		  (llvm::-append-basic-block function str))))
	(llvm::-build-br *builder* loop-bb)
	(position-builder *builder* loop-bb)
	(let ((old-val (gethash (for-expression.var-name expression) *named-values*)))
	  (setf (gethash (for-expression.var-name expression) *named-values*) alloca)
	  (when (codegen (for-expression.body expression))
	    (let ((step-val (if (for-expression.step expression)
				(codegen (for-expression.step expression))
				(llvm::-const-real (llvm::-double-type)
						   (doublify 1)))))
	      (when step-val
		(let ((end-cond (codegen (for-expression.end expression))))
		  (when end-cond
		    (let* ((cur-var
			    (cffi:with-foreign-string (str (for-expression.var-name expression))
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
			    (cffi:with-foreign-string (str "loopcond")
			      (llvm::-build-f-cmp
			       *builder*
			       (cffi:foreign-enum-value
				'llvm::|LLVMRealPredicate|
				'llvm::|LLVMRealONE|)
			       end-cond			       
			       (llvm::-const-real
				(llvm::-double-type)
				(doublify 0))
			       str)))
		      (let ((after-bb
			     (cffi:with-foreign-string (str "afterloop")
			       (llvm::-append-basic-block
				function
				str))))
			(llvm::-build-cond-br *builder* end-cond loop-bb after-bb)
			(position-builder *builder* after-bb)
			(if old-val
			    (setf (gethash (for-expression.var-name expression) *named-values*)
				  old-val)
			    (remhash (for-expression.var-name expression) *named-values*))
			;; for expr always returns 0.
			(llvm::-const-null (llvm::-double-type))))))))))))))


;;; code generation 6
;;;;6 7
(defun codegen-unary-expression (expression)
  (let ((operand-v (codegen (unary-expression.operand expression))))
    (when operand-v
      (let ((name (format nil "unary~a"
			  (unary-expression.opcode expression))))
	(let ((f
	       (get-function name)
		#+nil
		(cffi:with-foreign-string (str name)
		  (llvm::-get-named-function
		   *module*
		   str))))
	  (unless f
	    (error 'kaleidoscope-error :message "Unknown unary operator"))
	  (build-call *builder* f (list operand-v) "unop"))))))

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
       (prototype.arguments expression)))


;;;;END CODE GENERATION

;;;;Toplevel
(defun %initialize-module-and-pass-manager (&optional (string "my cool jit"))
  "return (values [NEW MODULE] [NEW FUNCTION PASS MANAGER])"
  (let ((module (cffi:with-foreign-string (str string)
		  (llvm::-module-create-with-name str))))
    (values
     module
     (when (not *compile-to-object-code?*)
       #+nil
       (let ((target (kaleidoscope-get-target-machine)))
	 #+nil
	 (let ((msg (llvm::-get-target-machine-triple target)))
	   (print (cffi:foreign-string-to-lisp msg))
	   (llvm::-dispose-message msg))
      ;;;;FIXME -potential bug? why set the layout of the module itself?
	 (llvm::-set-data-layout
	  module
	  (llvm::-get-data-layout-str module)
	  #+nil
	  (llvm::-get-target-machine-data
	   target)))
       (when *fpm?*
	 (let ((fpm (llvm::-create-function-pass-manager-for-module module)))
	   (progn
	     (unless (= *chapter* 4)
	       (llvm::-add-promote-memory-to-register-pass fpm))
	     (llvm::-add-instruction-combining-pass fpm)
	     (llvm::-add-reassociate-pass fpm)
	     (llvm::-add-g-v-n-pass fpm)
	     (llvm::-add-c-f-g-simplification-pass fpm))
	   (llvm::-initialize-function-pass-manager fpm)
	   fpm))))))

(defparameter *fucking-modules* nil)
(defun initialize-module-and-pass-manager ()
  (multiple-value-bind (module fpm) (%initialize-module-and-pass-manager)
    (setf *module* module)
    (push module *fucking-modules*)
    (setf *fpm* fpm)))

(defun %handle-definition (function-ast)
  (ecase *chapter*
    ((2)
     (format *output?* "Parsed a function definition~%"))
    ((3 4 5 6 7)
     (let ((lf (codegen function-ast)))
       (when lf
	 (format *output?* "~&~%Read function definition:")
	 (dump-value lf)
	 (when (not *compile-to-object-code?*)
	   (kaleidoscope-add-module *module*)
	   (initialize-module-and-pass-manager)))))))

(defun %handle-extern (prototype-ast)
  (ecase *chapter*
    ((2)
     (format *output?* "Parsed an extern~%"))
    ((3 4 5 6 7)
     (let ((function (codegen prototype-ast)))
       (when function
	 (format *output?* "Read extern: ")
	 (dump-value function)
	 (setf (gethash (prototype.name prototype-ast) *function-protos*)
	       prototype-ast))))))

(defun %handle-top-level-expression (ast)
  "Evaluate a top-level expression into an anonymous function."
  (declare (optimize (speed 0) (debug 3)))
  (ecase *chapter*
    ((2)
     (format *output?* "Parsed a top-level expr~%"))
    ((3 4 5 6 7)
     (let ((lf (codegen ast)))
       (when (not *compile-to-object-code?*)
	 (case *chapter*
	   ((3)
	    (format *output?* "Read top-level expression:")
	    (dump-value lf))
	   ((4 5 6 7)
	    (case *chapter*
	      ((4 5)
	       (dump-value lf)))
	    (let ((old *module*)
		  (module-abnormal? nil))
	      (pop *fucking-modules*)
	      ;;(format t "~&module??: ~s" old)
	      (cffi:with-foreign-object (foo :pointer 1)
		(setf module-abnormal?
		      (llvm::-verify-module
		       old
		       (cffi:foreign-enum-value
			'llvm::|LLVMVerifierFailureAction|
			'llvm::|LLVMPrintMessageAction|)
		       foo))
		(when module-abnormal?
		  (with-llvm-message (ptr) (cffi:mem-ref foo :pointer)
		    (print (cffi:foreign-string-to-lisp ptr) *output?*))
		  (llvm::-dispose-module old)))
	      (let ((name (prototype.name (function-definition.prototype ast))))
		(unless module-abnormal?
		  (let ((handle (kaleidoscope-add-module old)))
		    (let ((expr-symbol
			   (cffi:with-foreign-string (str name)
			     (kaleidoscope-find-symbol str))))
		      ;;(print expr-symbol)
		      (when (cffi:null-pointer-p expr-symbol)
			(error 'kaleidoscope-error :message "function not found"))

		      ;;(print 34234)
		      (let ((ptr (kaleidoscope-get-symbol-address expr-symbol)))
			;;(print ptr)
			;;(print 234234)
			(if (= 0 ptr)
			    (error 'kaleidoscope-error :message "function no body???")
			    (let ((result
				   (cffi:foreign-funcall-pointer
				    (cffi:make-pointer ptr) 
				    () :double)))
			      (format *output?* "~%Evaluated to ~fD0~%"
				      result)))))
		    (kaleidoscope-remove-module handle)))
		(remhash name *function-protos*)
		(initialize-module-and-pass-manager))
	      )
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
	   ))))))

;;; top-level 4 5 6 7
(defvar *execution-engine*)

(defmacro with-kaleidoscope-jit (&body body)
  `(progn
     (llvm::initialize-native-target?)
     (llvm::initialize-native-Asm-parser)
     (llvm::initialize-native-asm-printer)
     (let ((*jit?* t))
       (clrhash *function-protos*)
       (unwind-protect
	    (progn (kaleidoscope-create)
		   ,@body)
	 (kaleidoscope-destroy)))))

(defmacro with-builder (var &body body)
  (with-gensyms (completed? value)
    `(let ((,var nil)
	   (,completed? nil))
       (unwind-protect (progn
			 (let ((,value (llvm::-create-builder)))
			   (setf ,completed? t)
			   (setf ,var ,value))
			 ,@body)
	 (when ,completed?
	   (llvm::-dispose-builder ,var))))))

(defun chapter-test-sexps (&optional (n *chapter*))
  (let ((cell (aref *chapter-test-cases* n)))
    (lambda ()
      (if (consp cell)
	  (multiple-value-prog1 (values (car cell) t)
	    (setf cell (cdr cell)))
	  (values "error" nil)))))

(defun input-reader ()
  (lambda ()
    (let* ((eof (load-time-value (gensym)))
	   (value (read *standard-input* nil eof)))
      (if (or (eq eof value)
	      (eq value :quit))
	  (values "error" nil)
	  (values value t)))))

(defmacro do-provider ((value-var form) body)
  (with-gensyms (exists out)
    (once-only (form)
      `(block ,out
	 (loop
	    (multiple-value-bind (,value-var ,exists) (funcall ,form)
	      (if ,exists
		  ,body
		  (return-from ,out))))))))

(defun toplevel (n &optional (sexp-generator (input-reader)))
  ;;;reset allocated modules list
  (setf *fucking-modules* nil)  
  (let ((*chapter* n))
    (labels ((%start ()
	       (initialize-module-and-pass-manager)
	       (do-provider
		   (sexp sexp-generator)
		   (handler-case
		       (destructuring-bind (type ast) sexp
			 (case type
			   (%defun
			    ;;handle-definition
			    (%handle-definition ast))
			   (%extern
			    ;;handle-extern
			    (%handle-extern ast))
			   (%toplevel
			    ;;handle-top-level-expression
			    (%handle-top-level-expression ast))
			   (%comment)))
		     (kaleidoscope-error (e) (format *output?* "error: ~a~%" e))))
	       ;;from 6.0.0/docs/tutorial/BuildingAJIT1.html
	       ;;All resources will be cleaned up when your JIT class is destructed
	       ))
      (with-builder *builder*
	(cond (*jit?*
	       (with-kaleidoscope-jit
		 (%start)))
	      (*compile-to-object-code?*
	       (%start)
	       (dump-object-file *object-filename*))
	      (t
	       (%start)))))
    (values)))

(defparameter *object-filename* "/home/imac/install/other/output.o")
(defun dump-object-file (filename)
  (llvm::initialize-all-target-infos)
  (llvm::initialize-all-targets)
  (llvm::initialize-all-target-m-cs)
  (llvm::initialize-all-asm-parsers)
  (llvm::initialize-all-asm-printers)
  (with-llvm-message (triple) (llvm::-get-default-target-triple)
    ;;(print (cffi:foreign-string-to-lisp ptr))
    (llvm::-set-target *module* triple)
    (cffi:with-foreign-object (target-ref 'llvm::|LLVMTargetRef|)
      (let ((not-success? (llvm::-get-target-from-triple
			   triple
			   target-ref
			   (cffi:null-pointer))))
	(format t "get target: ~s" (not not-success?))
	(unless not-success?
	  (let ((target-machine-ref
		 (cffi:with-foreign-string (cpu "generic")
		   (cffi:with-foreign-string (features "")
		     (llvm::-create-target-machine
		      (cffi:mem-ref target-ref 'llvm::|LLVMTargetRef|)
		      triple
		      cpu
		      features
		      (cffi:foreign-enum-value
		       'llvm::|LLVMCodeGenOptLevel|
		       'llvm::|LLVMCodeGenLevelAggressive|)
		      (cffi:foreign-enum-value
		       'llvm::|LLVMRelocMode|
		       'llvm::|LLVMRelocDefault|)
		      (cffi:foreign-enum-value
		       'llvm::|LLVMCodeModel|
		       'llvm::|LLVMCodeModelDefault|)
		      )))))
	    (print "target machine ref created")
	    (cffi:with-foreign-object (error-message :pointer)
	      (cffi:with-foreign-string (str filename)
		(let ((not-success?
		       (llvm::-target-machine-emit-to-file
			target-machine-ref
			*module*
			str
			(cffi:foreign-enum-value
			 'llvm::|LLVMCodeGenFileType|
			 ;;'llvm::|LLVMAssemblyFile|
			 'llvm::|LLVMObjectFile|)
			error-message)))
		  (format t "dump file: ~s ~%~a"
			  (not not-success?)
			  filename))))))))))
