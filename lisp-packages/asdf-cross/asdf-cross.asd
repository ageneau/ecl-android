(asdf:defsystem asdf-cross
  :components
  ((:file "conduit-packages")
   (:file "package" :depends-on ("conduit-packages"))
   (:file "setup-compilers" :depends-on ("package"))
   (:file "asdf-cross" :depends-on ("package" "setup-compilers")))
  :depends-on (asdf))

