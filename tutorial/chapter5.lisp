(in-package :kaleidoscope.chapter5)

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
      (let* ((lf (codegen (parse-top-level-expression)))
             (ptr (llvm:pointer-to-global *execution-engine* lf)))
        (dump-value lf)
        (format *output?* "Evaluated to ~f"
                ;; NOTE: The C version of the tutorial only has the JIT side
                ;;       of this, so if you have an interpreter, it breaks.
                (if (cffi:pointer-eq ptr lf)        ; we have an interpreter
                    (llvm:generic-value-to-float
                     (llvm:double-type)
                     (llvm:run-function *execution-engine* ptr ()))
                    (cffi:foreign-funcall-pointer ptr () :double))))
    (kaleidoscope-error (e)
      (get-next-token)
      (format *output?* "error: ~a~%" e))))

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
  (llvm:with-objects ((*builder* llvm:builder)
                      (*module* llvm:module "my cool jit")
                      (*execution-engine* llvm:execution-engine *module*)
                      (*fpm* llvm:function-pass-manager *module*))
    (llvm:add-target-data (llvm:target-data *execution-engine*) *fpm*)
    (progn
      (llvm:add-promote-memory-to-register-pass *fpm*)
      (llvm:add-instruction-combining-pass *fpm*)
      (llvm:add-reassociate-pass *fpm*)
      (llvm:add-gvn-pass *fpm*)
      (llvm:add-cfg-simplification-pass *fpm*))
    
    (llvm:initialize-function-pass-manager *fpm*)

    (with-chapter 5
      (reset-token-reader)
      (get-next-token)
      (set-binop-precedence)
      (format *output?* "~&ready> ")
      (callcc (function main-loop)))
    (dump-module *module*))
  (values))
