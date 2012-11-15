(asdf:defsystem iphone
  :components
  ((:file "util")
   (:file "package" :depends-on ("util"))
   #+cross
   (:file "foundation" :depends-on ("util" "package"))
   #+cross
   (:file "uikit" :depends-on ("foundation" "util" "package"))
   #+cross
   (:file "glkit" :depends-on ("util" "foundation" "package")))
  :depends-on ())
