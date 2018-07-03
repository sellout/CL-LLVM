(defpackage k-lexer
  (:use #:cl)
  (:export
   :*identifier-string*
   :*number-value*
   :read-token
   :*current-token*
   :get-next-token))

;;; lexer 2
(defvar +whitespace+ '(#\space #\tab nil #\linefeed #\return))
(defvar *identifier-string*)
(defvar *number-value*)
(let ((last-char #\space))
  (defun read-token ()
    "Returns either a character or one of 'tok-eof, 'tok-def, 'tok-extern,
     'tok-identifier, or 'tok-number."
    (flet ((get-char () (read-char *standard-input* nil nil)))
      (loop while (find last-char +whitespace+)
        do (setf last-char (get-char)))
      (cond ((eql last-char nil) ; check for EOF, do not eat
             'tok-eof)
            ((alpha-char-p last-char)
             (setf *identifier-string*
                   (coerce (cons last-char
                                 (loop do (setf last-char (get-char))
                                   while (alphanumericp last-char)
                                   collecting last-char))
                           'string))
             (cond ((string= *identifier-string* "def") 'tok-def)
                   ((string= *identifier-string* "extern") 'tok-extern)
                   (t 'tok-identifier)))
            ((or (digit-char-p last-char) (char= last-char #\.))
             (setf *number-value*
                   (let ((*read-eval* nil))
                     (read-from-string
                      (coerce (cons last-char
                                    (loop do (setf last-char (get-char))
                                      while (or (digit-char-p last-char)
                                                (char= last-char #\.))
                                      collecting last-char))
                              'string))))
             'tok-number)
            ((eql last-char #\#) ; comment until end of line
             (loop do (setf last-char (get-char))
               until (find last-char '(nil #\linefeed #\return)))
             (if (null last-char) 'tok-eof (read-token)))
            (t
             (let ((this-char last-char))
               (setf last-char (get-char))
               this-char))))))
(defvar *current-token*)
;;; FIXME: can this function go away?
(defun get-next-token ()
  (setf *current-token* (read-token)))

;;; lexer 3
(defvar +whitespace+ '(#\space #\tab nil #\linefeed #\return))
(defvar *identifier-string*)
(defvar *number-value*)
(let ((last-char #\space))
  (defun read-token ()
    "Returns either a character or one of 'tok-eof, 'tok-def, 'tok-extern,
     'tok-identifier, or 'tok-number."
    (flet ((get-char () (read-char *standard-input* nil nil)))
      (loop while (find last-char +whitespace+)
        do (setf last-char (get-char)))
      (cond ((eql last-char nil) ; check for EOF, do not eat
             'tok-eof)
            ((alpha-char-p last-char)
             (setf *identifier-string*
                   (coerce (cons last-char
                                 (loop do (setf last-char (get-char))
                                   while (alphanumericp last-char)
                                   collecting last-char))
                           'string))
             (cond ((string= *identifier-string* "def") 'tok-def)
                   ((string= *identifier-string* "extern") 'tok-extern)
                   (t 'tok-identifier)))
            ((or (digit-char-p last-char) (char= last-char #\.))
             (setf *number-value*
                   (let ((*read-eval* nil))
                     (read-from-string
                      (coerce (cons last-char
                                    (loop do (setf last-char (get-char))
                                      while (or (digit-char-p last-char)
                                                (char= last-char #\.))
                                      collecting last-char))
                              'string))))
             'tok-number)
            ((eql last-char #\#) ; comment until end of line
             (loop do (setf last-char (get-char))
               until (find last-char '(nil #\linefeed #\return)))
             (if (null last-char) 'tok-eof (read-token)))
            (t
             (let ((this-char last-char))
               (setf last-char (get-char))
               this-char))))))
(defvar *current-token*)
;;; FIXME: can this function go away?
(defun get-next-token ()
  (setf *current-token* (read-token)))

;;; lexer 4
(defvar +whitespace+ '(#\space #\tab nil #\linefeed #\return))
(defvar *identifier-string*)
(defvar *number-value*)
(let ((last-char #\space))
  (defun read-token ()
    "Returns either a character or one of 'tok-eof, 'tok-def, 'tok-extern,
     'tok-identifier, or 'tok-number."
    (flet ((get-char () (read-char *standard-input* nil nil)))
      (loop while (find last-char +whitespace+)
        do (setf last-char (get-char)))
      (cond ((eql last-char nil) ; check for EOF, do not eat
             'tok-eof)
            ((alpha-char-p last-char)
             (setf *identifier-string*
                   (coerce (cons last-char
                                 (loop do (setf last-char (get-char))
                                   while (alphanumericp last-char)
                                   collecting last-char))
                           'string))
             (cond ((string= *identifier-string* "def") 'tok-def)
                   ((string= *identifier-string* "extern") 'tok-extern)
                   (t 'tok-identifier)))
            ((or (digit-char-p last-char) (char= last-char #\.))
             (setf *number-value*
                   (let ((*read-eval* nil))
                     (read-from-string
                      (coerce (cons last-char
                                    (loop do (setf last-char (get-char))
                                      while (or (digit-char-p last-char)
                                                (char= last-char #\.))
                                      collecting last-char))
                              'string))))
             'tok-number)
            ((eql last-char #\#) ; comment until end of line
             (loop do (setf last-char (get-char))
               until (find last-char '(nil #\linefeed #\return)))
             (if (null last-char) 'tok-eof (read-token)))
            (t
             (let ((this-char last-char))
               (setf last-char (get-char))
               this-char))))))
(defvar *current-token*)
;;; FIXME: can this function go away?
(defun get-next-token ()
  (setf *current-token* (read-token)))

;;; lexer 5
(defvar +whitespace+ '(#\space #\tab nil #\linefeed #\return))
(defvar *identifier-string*)
(defvar *number-value*)
(let ((last-char #\space))
  (defun read-token ()
    "Returns either a character or one of 'tok-eof, 'tok-def, 'tok-extern,
     'tok-identifier, or 'tok-number."
    (flet ((get-char () (read-char *standard-input* nil nil)))
      (loop while (find last-char +whitespace+)
        do (setf last-char (get-char)))
      (cond ((eql last-char nil) ; check for EOF, do not eat
             'tok-eof)
            ((alpha-char-p last-char)
             (setf *identifier-string*
                   (coerce (cons last-char
                                 (loop do (setf last-char (get-char))
                                   while (alphanumericp last-char)
                                   collecting last-char))
                           'string))
             (cond ((string= *identifier-string* "def") 'tok-def)
                   ((string= *identifier-string* "extern") 'tok-extern)
                   ((string= *identifier-string* "if") 'tok-if)
                   ((string= *identifier-string* "then") 'tok-then)
                   ((string= *identifier-string* "else") 'tok-else)
                   ((string= *identifier-string* "for") 'tok-for)
                   ((string= *identifier-string* "in") 'tok-in)
                   (t 'tok-identifier)))
            ((or (digit-char-p last-char) (char= last-char #\.))
             (setf *number-value*
                   (let ((*read-eval* nil))
                     (read-from-string
                      (coerce (cons last-char
                                    (loop do (setf last-char (get-char))
                                      while (or (digit-char-p last-char)
                                                (char= last-char #\.))
                                      collecting last-char))
                              'string))))
             'tok-number)
            ((eql last-char #\#) ; comment until end of line
             (loop do (setf last-char (get-char))
               until (find last-char '(nil #\linefeed #\return)))
             (if (null last-char) 'tok-eof (read-token)))
            (t
             (let ((this-char last-char))
               (setf last-char (get-char))
               this-char))))))
(defvar *current-token*)
;;; FIXME: can this function go away?
(defun get-next-token ()
  (setf *current-token* (read-token)))

;;; lexer 6
(defvar +whitespace+ '(#\space #\tab nil #\linefeed #\return))
(defvar *identifier-string*)
(defvar *number-value*)
(let ((last-char #\space))
  (defun read-token ()
    "Returns either a character or one of 'tok-eof, 'tok-def, 'tok-extern,
     'tok-identifier, or 'tok-number."
    (flet ((get-char () (read-char *standard-input* nil nil)))
      (loop while (find last-char +whitespace+)
        do (setf last-char (get-char)))
      (cond ((eql last-char nil) ; check for EOF, do not eat
             'tok-eof)
            ((alpha-char-p last-char)
             (setf *identifier-string*
                   (coerce (cons last-char
                                 (loop do (setf last-char (get-char))
                                   while (alphanumericp last-char)
                                   collecting last-char))
                           'string))
             (cond ((string= *identifier-string* "def") 'tok-def)
                   ((string= *identifier-string* "extern") 'tok-extern)
                   ((string= *identifier-string* "if") 'tok-if)
                   ((string= *identifier-string* "then") 'tok-then)
                   ((string= *identifier-string* "else") 'tok-else)
                   ((string= *identifier-string* "for") 'tok-for)
                   ((string= *identifier-string* "in") 'tok-in)
                   ((string= *identifier-string* "binary") 'tok-binary)
                   ((string= *identifier-string* "unary") 'tok-unary)
                   (t 'tok-identifier)))
            ((or (digit-char-p last-char) (char= last-char #\.))
             (setf *number-value*
                   (let ((*read-eval* nil))
                     (read-from-string
                      (coerce (cons last-char
                                    (loop do (setf last-char (get-char))
                                      while (or (digit-char-p last-char)
                                                (char= last-char #\.))
                                      collecting last-char))
                              'string))))
             'tok-number)
            ((eql last-char #\#) ; comment until end of line
             (loop do (setf last-char (get-char))
               until (find last-char '(nil #\linefeed #\return)))
             (if (null last-char) 'tok-eof (read-token)))
            (t
             (let ((this-char last-char))
               (setf last-char (get-char))
               this-char))))))
(defvar *current-token*)
;;; FIXME: can this function go away?
(defun get-next-token ()
  (setf *current-token* (read-token)))

;;; lexer 7
(defvar +whitespace+ '(#\space #\tab nil #\linefeed #\return))
(defvar *identifier-string*)
(defvar *number-value*)
(let ((last-char #\space))
  (defun read-token ()
    "Returns either a character or one of 'tok-eof, 'tok-def, 'tok-extern,
     'tok-identifier, or 'tok-number."
    (flet ((get-char () (read-char *standard-input* nil nil)))
      (loop while (find last-char +whitespace+)
        do (setf last-char (get-char)))
      (cond ((eql last-char nil) ; check for EOF, do not eat
             'tok-eof)
            ((alpha-char-p last-char)
             (setf *identifier-string*
                   (coerce (cons last-char
                                 (loop do (setf last-char (get-char))
                                   while (alphanumericp last-char)
                                   collecting last-char))
                           'string))
             (cond ((string= *identifier-string* "def") 'tok-def)
                   ((string= *identifier-string* "extern") 'tok-extern)
                   ((string= *identifier-string* "if") 'tok-if)
                   ((string= *identifier-string* "then") 'tok-then)
                   ((string= *identifier-string* "else") 'tok-else)
                   ((string= *identifier-string* "for") 'tok-for)
                   ((string= *identifier-string* "in") 'tok-in)
                   ((string= *identifier-string* "binary") 'tok-binary)
                   ((string= *identifier-string* "unary") 'tok-unary)
                   ((string= *identifier-string* "var") 'tok-var)
                   (t 'tok-identifier)))
            ((or (digit-char-p last-char) (char= last-char #\.))
             (setf *number-value*
                   (let ((*read-eval* nil))
                     (read-from-string
                      (coerce (cons last-char
                                    (loop do (setf last-char (get-char))
                                      while (or (digit-char-p last-char)
                                                (char= last-char #\.))
                                      collecting last-char))
                              'string))))
             'tok-number)
            ((eql last-char #\#) ; comment until end of line
             (loop do (setf last-char (get-char))
               until (find last-char '(nil #\linefeed #\return)))
             (if (null last-char) 'tok-eof (read-token)))
            (t
             (let ((this-char last-char))
               (setf last-char (get-char))
               this-char))))))
(defvar *current-token*)
;;; FIXME: can this function go away?
(defun get-next-token ()
  (setf *current-token* (read-token)))
