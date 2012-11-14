;;;
;;;
;;; Copyright (c) 2009, Jean-Claude Beaudoin
;;; All rights reserved by the author.
;;;
;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without
;;; restriction, including without limitation the rights to use, copy,
;;; modify, merge, publish, distribute, sublicense, and/or sell copies
;;; of the Software, and to permit persons to whom the Software is
;;; furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.
;;;
;;;
;;;


(in-package :cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find (find-package :cl+j) (package-use-list *package*))
    (use-package :cl+j)))

;(java-init) ;; if not already done.

(java-import "java.util.*")

(defvar p (jnew "Properties"))

(#_setProperty p (jstring "fred") (jstring "ethel"))
(#_setProperty p (jstring "ricky") (jstring "lucy"))

(defmacro doenum ((var enum) &body body)
  (let ((genum (gensym)))
    `(let ((,genum ,enum))
       (do ()
	   ((jfalse-p (#_hasMoreElements ,genum)))
	 (let ((,var (#_nextElement ,genum)))
	   ,@body)))))

(doenum (prop (#_elements p))
  (print (jtol (#_toString prop))))

