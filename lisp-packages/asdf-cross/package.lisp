(org.tfeb.conduit-packages:defpackage cross-cmp
  (:use :cl :compiler)
  (:export
   #:*cross-compilers*
   #:cross-compiler
   #:get-compiler
   #:registered-compilers
   #:cross-compile
   #:user-cc-flags
   #:user-ld-flags))

(org.tfeb.conduit-packages:defpackage asdf-cross
  (:use :cl :cross-cmp)
  (:clones :asdf)
  (:export #:make-cross-build
	   #:build-ios
	   #:lipo))
