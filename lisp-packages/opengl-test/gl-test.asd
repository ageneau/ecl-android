(defsystem gl-test
  :description "Examples using cl-opengl, cl-glu and cl-glut."
  :depends-on (cffi cl-opengl iphone)
  :components
  ((:file "package")
   ;; (:file "opengl-array" :depends-on ("package"))
   ))

