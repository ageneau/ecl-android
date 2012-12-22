(in-package :cl-user)

#+ecl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (gray::redefine-cl-functions))

(defpackage :nacl
  (:use :cl :ffi :gray)
  (:export
   :get-browser
   :post-message
   :test-stream))

