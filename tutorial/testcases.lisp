(in-package :k-test)

(k-test::define-chapter-test 2
 ((%defun
   (function-definition (prototype "foo" #("x" "y") nil 0)
    (binary-expression #\+ (variable-expression "x")
     (call-expression "foo"
      ((variable-expression "y") (number-expression 4.0))))))
  (%defun
   (function-definition (prototype "foo" #("x" "y") nil 0)
    (binary-expression #\+ (variable-expression "x")
     (variable-expression "y"))))
  (%toplevel
   (function-definition (prototype "0" #() nil 0) (variable-expression "y")))
  (%defun
   (function-definition (prototype "foo" #("x" "y") nil 0)
    (binary-expression #\+ (variable-expression "x")
     (variable-expression "y"))))
  (%extern (prototype "sin" #("a") nil 0)))) 
(k-test::define-chapter-test 3
 ((%toplevel
   (function-definition (prototype "0" #() nil 0)
    (binary-expression #\+ (number-expression 4) (number-expression 5))))
  (%defun
   (function-definition (prototype "foo" #("a" "b") nil 0)
    (binary-expression #\+
     (binary-expression #\+
      (binary-expression #\* (variable-expression "a")
       (variable-expression "a"))
      (binary-expression #\*
       (binary-expression #\* (number-expression 2) (variable-expression "a"))
       (variable-expression "b")))
     (binary-expression #\* (variable-expression "b")
      (variable-expression "b")))))
  (%defun
   (function-definition (prototype "bar" #("a") nil 0)
    (binary-expression #\+
     (call-expression "foo"
      ((variable-expression "a") (number-expression 4.0)))
     (call-expression "bar" ((number-expression 31337))))))
  (%extern (prototype "cos" #("x") nil 0))
  (%toplevel
   (function-definition (prototype "1" #() nil 0)
    (call-expression "cos" ((number-expression 1.234))))))) 
(k-test::define-chapter-test 4
 ((%toplevel
   (function-definition (prototype "0" #() nil 0)
    (binary-expression #\+ (number-expression 4) (number-expression 5))))
  (%defun
   (function-definition (prototype "testfunc" #("x" "y") nil 0)
    (binary-expression #\+ (variable-expression "x")
     (binary-expression #\* (variable-expression "y") (number-expression 2)))))
  (%toplevel
   (function-definition (prototype "1" #() nil 0)
    (call-expression "testfunc"
     ((number-expression 4) (number-expression 10)))))
  (%extern (prototype "sin" #("x") nil 0))
  (%extern (prototype "cos" #("x") nil 0))
  (%toplevel
   (function-definition (prototype "2" #() nil 0)
    (call-expression "sin" ((number-expression 1.0)))))
  (%defun
   (function-definition (prototype "foo" #("x") nil 0)
    (binary-expression #\+
     (binary-expression #\* (call-expression "sin" ((variable-expression "x")))
      (call-expression "sin" ((variable-expression "x"))))
     (binary-expression #\* (call-expression "cos" ((variable-expression "x")))
      (call-expression "cos" ((variable-expression "x")))))))
  (%toplevel
   (function-definition (prototype "3" #() nil 0)
    (call-expression "foo" ((number-expression 4.0))))))) 
(k-test::define-chapter-test 5
 ((%defun
   (function-definition (prototype "fib" #("x") nil 0)
    (if-expression
     (binary-expression #\< (variable-expression "x") (number-expression 3))
     (number-expression 1)
     (binary-expression #\+
      (call-expression "fib"
       ((binary-expression #\- (variable-expression "x")
         (number-expression 1))))
      (call-expression "fib"
       ((binary-expression #\- (variable-expression "x")
         (number-expression 2))))))))
  (%extern (prototype "foo" #() nil 0)) (%extern (prototype "bar" #() nil 0))
  (%defun
   (function-definition (prototype "baz" #("x") nil 0)
    (if-expression (variable-expression "x") (call-expression "foo" nil)
     (call-expression "bar" nil))))
  (%extern (prototype "putchard" #("char") nil 0))
  (%defun
   (function-definition (prototype "printstar" #("n") nil 0)
    (for-expression "i" (number-expression 1)
     (binary-expression #\< (variable-expression "i")
      (variable-expression "n"))
     (number-expression 1.0)
     (call-expression "putchard" ((number-expression 42))))))
  (%comment (" ascii 42 = '*'" " print 100 '*' characters"))
  (%toplevel
   (function-definition (prototype "0" #() nil 0)
    (call-expression "printstar" ((number-expression 100))))))) 
(k-test::define-chapter-test 6
 ((%extern (prototype "printd" #("x") nil 30))
  (%defun
   (function-definition (prototype "binary:" #("x" "y") 2 1)
    (number-expression 0)))
  (%comment (" Low-precedence operator that ignores operands."))
  (%toplevel
   (function-definition (prototype "0" #() nil 0)
    (binary-expression #\:
     (binary-expression #\:
      (call-expression "printd" ((number-expression 123)))
      (call-expression "printd" ((number-expression 456))))
     (call-expression "printd" ((number-expression 789))))))
  (%comment (" Logical unary not."))
  (%defun
   (function-definition (prototype "unary!" #("v") 1 30)
    (if-expression (variable-expression "v") (number-expression 0)
     (number-expression 1))))
  (%comment (" Unary negate."))
  (%defun
   (function-definition (prototype "unary-" #("v") 1 30)
    (binary-expression #\- (number-expression 0) (variable-expression "v"))))
  (%comment (" Define > with the same precedence as <."))
  (%defun
   (function-definition (prototype "binary>" #("LHS" "RHS") 2 10)
    (binary-expression #\< (variable-expression "RHS")
     (variable-expression "LHS"))))
  (%comment (" Binary logical or, which does not short circuit. "))
  (%defun
   (function-definition (prototype "binary|" #("LHS" "RHS") 2 5)
    (if-expression (variable-expression "LHS") (number-expression 1)
     (if-expression (variable-expression "RHS") (number-expression 1)
      (number-expression 0)))))
  (%comment (" Binary logical and, which does not short circuit. "))
  (%defun
   (function-definition (prototype "binary&" #("LHS" "RHS") 2 6)
    (if-expression (unary-expression #\! (variable-expression "LHS"))
     (number-expression 0)
     (unary-expression #\!
      (unary-expression #\! (variable-expression "RHS"))))))
  (%comment (" Define = with slightly lower precedence than relationals."))
  (%defun
   (function-definition (prototype "binary=" #("LHS" "RHS") 2 9)
    (unary-expression #\!
     (binary-expression #\|
      (binary-expression #\< (variable-expression "LHS")
       (variable-expression "RHS"))
      (binary-expression #\> (variable-expression "LHS")
       (variable-expression "RHS"))))))
  (%extern (prototype "putchard" #("char") nil 30))
  (%defun
   (function-definition (prototype "printdensity" #("d") nil 30)
    (if-expression
     (binary-expression #\> (variable-expression "d") (number-expression 8))
     (call-expression "putchard" ((number-expression 32)))
     (if-expression
      (binary-expression #\> (variable-expression "d") (number-expression 4))
      (call-expression "putchard" ((number-expression 46)))
      (if-expression
       (binary-expression #\> (variable-expression "d") (number-expression 2))
       (call-expression "putchard" ((number-expression 43)))
       (call-expression "putchard" ((number-expression 42))))))))
  (%comment (" ' '" " '.'" " '+'")) (%comment (" '*'"))
  (%toplevel
   (function-definition (prototype "1" #() nil 0)
    (binary-expression #\:
     (binary-expression #\:
      (binary-expression #\:
       (binary-expression #\:
        (binary-expression #\:
         (binary-expression #\:
          (call-expression "printdensity" ((number-expression 1)))
          (call-expression "printdensity" ((number-expression 2))))
         (call-expression "printdensity" ((number-expression 3))))
        (call-expression "printdensity" ((number-expression 4))))
       (call-expression "printdensity" ((number-expression 5))))
      (call-expression "printdensity" ((number-expression 9))))
     (call-expression "putchard" ((number-expression 10))))))
  (%comment
   (" determine whether the specific location diverges."
    " Solve for z = z^2 + c in the complex plane."))
  (%defun
   (function-definition
    (prototype "mandleconverger" #("real" "imag" "iters" "creal" "cimag") nil
     30)
    (if-expression
     (binary-expression #\|
      (binary-expression #\> (variable-expression "iters")
       (number-expression 255))
      (binary-expression #\>
       (binary-expression #\+
        (binary-expression #\* (variable-expression "real")
         (variable-expression "real"))
        (binary-expression #\* (variable-expression "imag")
         (variable-expression "imag")))
       (number-expression 4)))
     (variable-expression "iters")
     (call-expression "mandleconverger"
      ((binary-expression #\+
        (binary-expression #\-
         (binary-expression #\* (variable-expression "real")
          (variable-expression "real"))
         (binary-expression #\* (variable-expression "imag")
          (variable-expression "imag")))
        (variable-expression "creal"))
       (binary-expression #\+
        (binary-expression #\*
         (binary-expression #\* (number-expression 2)
          (variable-expression "real"))
         (variable-expression "imag"))
        (variable-expression "cimag"))
       (binary-expression #\+ (variable-expression "iters")
        (number-expression 1))
       (variable-expression "creal") (variable-expression "cimag"))))))
  (%comment
   (" return the number of iterations required for the iteration to escape"))
  (%defun
   (function-definition (prototype "mandleconverge" #("real" "imag") nil 30)
    (call-expression "mandleconverger"
     ((variable-expression "real") (variable-expression "imag")
      (number-expression 0) (variable-expression "real")
      (variable-expression "imag")))))
  (%comment
   (" compute and plot the mandlebrot set with the specified 2 dimensional range"
    " info."))
  (%defun
   (function-definition
    (prototype "mandelhelp" #("xmin" "xmax" "xstep" "ymin" "ymax" "ystep") nil
     30)
    (for-expression "y" (variable-expression "ymin")
     (binary-expression #\< (variable-expression "y")
      (variable-expression "ymax"))
     (variable-expression "ystep")
     (binary-expression #\:
      (for-expression "x" (variable-expression "xmin")
       (binary-expression #\< (variable-expression "x")
        (variable-expression "xmax"))
       (variable-expression "xstep")
       (call-expression "printdensity"
        ((call-expression "mandleconverge"
          ((variable-expression "x") (variable-expression "y"))))))
      (call-expression "putchard" ((number-expression 10)))))))
  (%comment
   (" mandel - This is a convenient helper function for ploting the mandelbrot set"
    " from the specified position with the specified Magnification."))
  (%defun
   (function-definition
    (prototype "mandel" #("realstart" "imagstart" "realmag" "imagmag") nil 30)
    (call-expression "mandelhelp"
     ((variable-expression "realstart")
      (binary-expression #\+ (variable-expression "realstart")
       (binary-expression #\* (variable-expression "realmag")
        (number-expression 78)))
      (variable-expression "realmag") (variable-expression "imagstart")
      (binary-expression #\+ (variable-expression "imagstart")
       (binary-expression #\* (variable-expression "imagmag")
        (number-expression 40)))
      (variable-expression "imagmag")))))
  (%toplevel
   (function-definition (prototype "2" #() nil 0)
    (call-expression "mandel"
     ((unary-expression #\- (number-expression 2.3))
      (unary-expression #\- (number-expression 1.3)) (number-expression 0.05)
      (number-expression 0.07)))))
  (%toplevel
   (function-definition (prototype "3" #() nil 0)
    (call-expression "mandel"
     ((unary-expression #\- (number-expression 2))
      (unary-expression #\- (number-expression 1)) (number-expression 0.02)
      (number-expression 0.04)))))
  (%toplevel
   (function-definition (prototype "4" #() nil 0)
    (call-expression "mandel"
     ((unary-expression #\- (number-expression 0.9))
      (unary-expression #\- (number-expression 1.4)) (number-expression 0.02)
      (number-expression 0.03))))))) 
(k-test::define-chapter-test 7
 ((%comment
   (" Define ':' for sequencing: as a low-precedence operator that ignores operands"
    " and just returns the RHS."))
  (%defun
   (function-definition (prototype "binary:" #("x" "y") 2 1)
    (variable-expression "y")))
  (%comment (" Recursive fib, we could do this before."))
  (%defun
   (function-definition (prototype "fib" #("x") nil 30)
    (if-expression
     (binary-expression #\< (variable-expression "x") (number-expression 3))
     (number-expression 1)
     (binary-expression #\+
      (call-expression "fib"
       ((binary-expression #\- (variable-expression "x")
         (number-expression 1))))
      (call-expression "fib"
       ((binary-expression #\- (variable-expression "x")
         (number-expression 2))))))))
  (%comment (" Iterative fib."))
  (%defun
   (function-definition (prototype "fibi" #("x") nil 30)
    (var-expression (("a" number-expression 1) ("b" number-expression 1) ("c"))
     (binary-expression #\:
      (for-expression "i" (number-expression 3)
       (binary-expression #\< (variable-expression "i")
        (variable-expression "x"))
       nil
       (binary-expression #\:
        (binary-expression #\:
         (binary-expression #\= (variable-expression "c")
          (binary-expression #\+ (variable-expression "a")
           (variable-expression "b")))
         (binary-expression #\= (variable-expression "a")
          (variable-expression "b")))
        (binary-expression #\= (variable-expression "b")
         (variable-expression "c"))))
      (variable-expression "b")))))
  (%comment (" Call it. "))
  (%toplevel
   (function-definition (prototype "0" #() nil 0)
    (call-expression "fibi" ((number-expression 10))))))) 
