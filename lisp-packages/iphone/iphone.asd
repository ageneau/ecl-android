(asdf:defsystem iphone
  :components
  ((:file "util")
   (:file "package" :depends-on ("util"))
   #+cross
   (:file "eclffi" :depends-on ("util" "package"))
   #+cross
   (:file "cocoa" :depends-on ("util" "package")))
  :depends-on ())
