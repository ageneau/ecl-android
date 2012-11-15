(asdf:defsystem openglsample
  :components
  ((:file "package")
   #+cross
   (:file "shader-vao" :depends-on ("package")))
  :depends-on (iphone cl-opengl))
