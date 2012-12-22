(asdf:defsystem nacl
  :components
  ((:file "package")
   #+cross
   (:file "nacl" :depends-on ("package")))
  :depends-on ())

