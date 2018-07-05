(in-package :k-shared)

(defmacro with-chapter (n &body body)
  `(let ((*chapter* ,n))
     (%with-tokens ,n ,@body)))
