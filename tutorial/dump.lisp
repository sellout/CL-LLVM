(in-package :k-shared)

(etouq
  (cons 'progn
	(loop for i from 2 to 7 collecting (list 'writecodes i))))
