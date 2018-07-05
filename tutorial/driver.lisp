(in-package :k-shared)
(defmacro with-chapter (n &body body)
  (once-only (n)
    `(let ((*chapter* ,n))
       (%with-tokens ,n ,@body))))

(defun toplevel (n)
  (with-chapter n
    (flet ((start ()
	     (format *output?* "~&ready> ")
	     (reset-token-reader)
	     (get-next-token)
	     (set-binop-precedence)
	     (callcc (function main-loop))))
      (case *chapter*
	((2) (start))
	((3 4 5 6 7)
	 (llvm:with-objects
	     ((*builder* llvm:builder)
	      (*module* llvm:module "my cool jit"))
	   (case *chapter*
	     ((3)
	      (start)
	      (dump-module *module*))
	     ((4 5 6 7)
	      #+nil
	      (when *jit?*
		(llvm::initialize-native-target?)
		(llvm::initialize-native-Asm-parser)
		(llvm::initialize-native-asm-printer))
	      (llvm:with-objects ((*execution-engine* llvm:execution-engine *module*)
				  ;;(*myjit* llvm:jit-compiler *module*)
				  (*fpm* llvm:function-pass-manager *module*))    
		(llvm:add-target-data (llvm:target-data *execution-engine*) *fpm*)
		;;passes    
		(progn
		  (unless (= *chapter* 4)
		    (llvm:add-promote-memory-to-register-pass *fpm*))
		  (llvm:add-instruction-combining-pass *fpm*)
		  (llvm:add-reassociate-pass *fpm*)
		  (llvm:add-gvn-pass *fpm*)
		  (llvm:add-cfg-simplification-pass *fpm*))

		(llvm:initialize-function-pass-manager *fpm*)
		(start)
		(dump-module *module*)))))))))
  (values))
