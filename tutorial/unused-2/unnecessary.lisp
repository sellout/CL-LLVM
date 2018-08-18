(defun eh? (n)
  (mapcar (lambda (x)
	    (tree-equal x (car n) :test #'equalp))
	  n))
(defmacro eh (&rest body)
  `(quote ,(eh? body)))

(:export
 #:*output?*
 #:*input?*)
(:export
 #:callcc)
(:export
 #:dump-value
 #:dump-module)
(:export
 #:*this-directory*)
(:export
 #:chap-package
 #:with-chapter)

  ;;;;ast
(:export
 :expression
 :number-expression
 :value
 :variable-expression
 :name
 :binary-expression
 :operator
 :lhs
 :rhs
 :call-expression
 :callee
 :arguments
 :function-definition
 :prototype
 :body
 :arguments
 :precedence
 :if-expression
 :_condition
 :then
 :else
 :for-expression
 :var-name
 :start
 :end
 :step
 :body
 :step*
 :unary-expression
 :opcode
 :operand
 :unary-operator-p
 :binary-operator-p
 :operator-name
 :var-expression
 :var-names
 )

  ;;;;code-generation
(:export
 :codegen
 :*module*
 :*builder*
 :*fpm*
 :*execution-engine*)
(:export
 :kaleidoscope-error
 :main-loop
 :*chapter*
 :toplevel)
