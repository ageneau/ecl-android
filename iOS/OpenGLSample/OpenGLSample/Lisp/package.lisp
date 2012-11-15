(in-package :cl-user)

(defpackage :openglsample
  (:use :cl :cl-opengl)
  (:export
   #:shader-vao-window
   #:setup
   #:tick
   #:display
   #:reshape
   #:cleanup))

