(in-package :k-shared)

(eval-always
  (defparameter *chapcodes* (make-hash-table :test 'equalp)))
(defmacro define-codes (name (&rest chapters) &body codes)
  (setf (gethash name *chapcodes*)
	(cons chapters codes))
  nil)

(eval-always
  (defun %writecodes (chap &optional (package (chap-package chap)))
    (let ((acc nil))
      (dohash (name value) *chapcodes*
	      (declare (ignore name))
	      (when (find chap (car value))
		(mapc (lambda (x) (push x acc))
		      (cdr value))))
      (repackage
       (nreverse acc)
       package))))

(defmacro writecodes (chap)
  (cons 'progn (%writecodes chap)))

(define-codes "base?" (2 3 4 5 6 7)
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
    (:documentation "Expression class for function calls.")))

(define-codes "functions" (2 3 4 5)
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
   (:documentation "This class represents a function definition itself.")))

(define-codes "more" (5 6 7)
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
   (:documentation "Expression class for for/in.")))

(define-codes "more fun" (6 7)
 (defclass prototype ()
   ((name :initform "" :initarg :name :reader name)
    (arguments :initform (make-array 0) :initarg :arguments :reader arguments)
    (operatorp :initform nil :initarg :operatorp :reader operatorp)
    (precedence :initform 0 :initarg :precedence :reader precedence))
   (:documentation
    "This class represents the “prototype” for a function, which captures its
    name, and its argument names (thus implicitly the number of arguments the
    function takes)."))
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
 (defclass function-definition ()
   ((prototype :initarg :prototype :reader prototype)
    (body :initarg :body :reader body))
   (:documentation "This class represents a function definition itself.")))

(define-codes "mutable variables" (7) 
 (defclass var-expression (expression)
   ((var-names :initarg :var-names :reader var-names)
    (body :initarg :body :reader body))
   (:documentation "Expression class for var/in")))

(etouq
  (cons 'progn
	(loop for i from 2 to 7 collecting (list 'writecodes i))))
