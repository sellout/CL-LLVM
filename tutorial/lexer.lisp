(in-package :k-shared)

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

(defmacro with-tokens (chap &body body)
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
;;;;
(defvar *current-token*)
(defun get-next-token (&optional (token-types *token-types*) (stream *input?*))
  (setf *current-token* (read-token token-types stream)))
