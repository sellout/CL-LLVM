(in-package :k-shared)

;;; code generation 3
(defvar *module*)
(defvar *builder*)
(defvar *named-values*)

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
	      (llvm:run-function-pass-manager *fpm* function)
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
	      (llvm:run-function-pass-manager *fpm* function)
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
	      (llvm:run-function-pass-manager *fpm* function)
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
