;;; Copyright (c) 2011, Sylvain Ageneau
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


(require :asdf)

(defparameter *here* (merge-pathnames "./"))
(defparameter *cache* (merge-pathnames "./cache/" *here*))

(asdf:initialize-source-registry
 `(:source-registry
   :ignore-inherited-configuration
   (:tree ,(namestring (merge-pathnames "../../../lisp-packages/" *here*)))))

(asdf:oos 'asdf:load-op :asdf-cross)

(defun setup-asdf ()
  (ensure-directories-exist *cache*)
  (setf (symbol-value (read-from-string "asdf::*user-cache*"))
	(list *cache* :implementation)))

(setup-asdf)

;;(setq c::*user-ld-flags* "-framework OpenGL")
(asdf:oos 'asdf:load-op :cffi)
(cross-cmp::setup-compilers)

(let* ((system :nacl)
       (nacl32-compiler (cross-cmp:get-compiler "i686-pc-nacl"))
       (nacl64-compiler (cross-cmp:get-compiler "x86_64-pc-nacl"))
       ;;(cc-flags (format nil "-g -I~a" (namestring (merge-pathnames "libiphone" (asdf:component-pathname (asdf:find-system :iphone))))))
       (cffi-sys:*cffi-ecl-method* :c/c++)
       )
  ;; (setf (cross-cmp:user-cc-flags sim-compiler) cc-flags)
  ;; (setf (cross-cmp:user-cc-flags ios-compiler) cc-flags)
  (asdf-cross:make-cross-build system
			       :type :lib
			       :monolithic t
			       :arch-list '("i686-pc-nacl" "x86_64-pc-nacl")))
