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

(defun setup-asdf ()
  (ensure-directories-exist *cache*)
  (setf (symbol-value (read-from-string "asdf::*user-cache*"))
	(list *cache* :implementation)))

(setup-asdf)
(load "/opt/ecl/iPhoneSimulator/lib/crosscmp")
(load "asdf-cross")


;; (trace asdf::bundle-sub-operations)
;; (trace asdf::perform)
;; ;; (trace asdf::cross-compile-op)
;; (trace asdf::component-depends-on)
;; (trace asdf::output-files)
;; (trace asdf::input-files)
;; (trace asdf::operation-done-p)

;; (trace asdf::gather-components)
;; (trace asdf::safe-file-write-date)

;; (ext::package-lock (find-package :cl) nil)
;; (trace c::compile-file)
;; (trace c::compile)
;; (ext::package-lock (find-package :cl) t)

;; alexandria-20120520-git babel-20120407-git bordeaux-threads-0.8.1 cffi_0.10.7.1 cl+j-0.2 trivial-features-20120407-git trivial-garbage-20120107-git

(asdf:initialize-source-registry
 `(:source-registry
   (:tree ,(namestring (merge-pathnames "../../lisp-packages/" *here*)))
   :inherit-configuration))

(asdf:defsystem app-bundle
  :depends-on (:deflate
	       :profile
	       :ql-minitar
	       :rt		 
	       :sockets
	       :ecl-cdb
	       :ecl-help
	       ;;	       :usocket
	       :cl-opengl
	       ;; :cffi
	       ))


(asdf:oos 'asdf::load-op :app-bundle)
(asdf:oos 'asdf::cross-monolithic-lib-op :app-bundle)
;; (asdf:oos 'asdf::load-op :trivial-features)
;; (asdf:oos 'asdf::cross-monolithic-lib-op :trivial-features)


(si:quit)
