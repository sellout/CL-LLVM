CL-LLVM provides Common Lisp bindings for [LLVM](http://llvm.org/). It takes the FFI approach, rather than attempting to output LLVM assembly or bitcode directly.

**Note**: This library is available via [Quicklisp](https://quicklisp.org/).

A description of the differences between [the C API](http://llvm.org/doxygen/dir_ba5bdc16f452288d1429bb9e178a5965.html) and the Lisp API can be found in `src/package.lisp`.

There is also a CL implementation of [the LLVM tutorial](http://llvm.org/docs/tutorial/) in `tutorial/`. The `.lisp` files for each chapter contain the implementation used in that chapter, while the `.k` files contain the examples included with that chapter and the `.out` files contain the results of running those examples. The `.k` and `.out` files primarily exist for testing the Lisp implementation. 