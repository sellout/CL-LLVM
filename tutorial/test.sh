#!/bin/sh

## No output means success, otherwise you see a diff between the expected output
## and the actual output.

dx86cl64 --quiet --batch \
         --eval '(asdf:load-system :kaleidoscope :verbose nil)' \
         --eval '(kaleidoscope.chapter2:toplevel)' \
         <tutorial/chapter2.k \
         2>&1 \
         | diff -ub tutorial/chapter2.out -

dx86cl64 --quiet --batch \
         --eval '(asdf:load-system :kaleidoscope :verbose nil)' \
         --eval '(kaleidoscope.chapter3:toplevel)' \
         <tutorial/chapter3.k \
         2>&1 \
         | diff -ub tutorial/chapter3.out -

dx86cl64 --quiet --batch \
         --eval '(asdf:load-system :kaleidoscope :verbose nil)' \
         --eval '(kaleidoscope.chapter4:toplevel)' \
         <tutorial/chapter4.k \
         2>&1 \
         | diff -ub tutorial/chapter4.out -

dx86cl64 --quiet --batch \
         --eval '(asdf:load-system :kaleidoscope :verbose nil)' \
         --eval '(kaleidoscope.chapter5:toplevel)' \
         <tutorial/chapter5.k \
         2>&1 \
         | diff -ub tutorial/chapter5.out -

dx86cl64 --quiet --batch \
         --eval '(asdf:load-system :kaleidoscope :verbose nil)' \
         --eval '(kaleidoscope.chapter6:toplevel)' \
         <tutorial/chapter6.k \
         2>&1 \
         | diff -ub tutorial/chapter6.out -

dx86cl64 --quiet --batch \
         --eval '(asdf:load-system :kaleidoscope :verbose nil)' \
         --eval '(kaleidoscope.chapter7:toplevel)' \
         <tutorial/chapter7.k \
         2>&1 \
         | diff -ub tutorial/chapter7.out -
