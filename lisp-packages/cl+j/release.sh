#!/usr/bin/bash

#set -x

REL_DIR=../releases/cl+j-$1

REL_FILES="cl_j.jar \
           Copyright \
           reference.lisp \
           jni.lisp \
           vtable.lisp \
           java_callback.lisp \
           cl+j.lisp \
           sbcl_repl.lisp \
           cl+j_pkg.lisp \
           cl+j.asd"

sh clean_up.sh
mkdir $REL_DIR
cp -pR demos cl_j $REL_DIR
cp -p $REL_FILES $REL_DIR

