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
(load "/opt/ecl/iPhoneOS/lib/crosscmp")
(load "../../utils/asdf_cross/asdf-cross")

(asdf:initialize-source-registry
 `(:source-registry
   :ignore-inherited-configuration
   (:tree ,(namestring (merge-pathnames "../../lisp-packages/" *here*)))))

(in-package :cross-cmp)

(loop
   for arch being the hash-keys of (registered-compilers)
   using (hash-value compiler)
   when (search "apple-darwin" arch)
   do (progn 
	(setf (cc-flags compiler)
	      (concatenate 'string
			   "-x objective-c -fobjc-abi-version=2 -fobjc-legacy-dispatch "
			   (cc-flags compiler)))
	(setf (ld-flags compiler)
	      (concatenate 'string
			   "-x objective-c -fobjc-abi-version=2 -fobjc-legacy-dispatch "
			   (ld-flags compiler)))))

(in-package :cl-user)

(defun lipo (builds)
  (let ((arm-lib (find-if #'(lambda (x) (search "arm-apple-darwin" (pathname-name x))) builds))
	(x86-lib (find-if #'(lambda (x) (search "i686-apple-darwin" (pathname-name x))) builds)))
    (if (and arm-lib x86-lib)
	(let ((output-file
	       (format nil "~a_ios_universal.a"
		       (subseq (namestring arm-lib) 0 (search "_arm-apple-darwin" (namestring arm-lib))))))
	  (system:system
	   (format nil "/usr/bin/lipo -arch armv7 ~A -arch i386 ~A -create -output ~a ~%"
		   arm-lib
		   x86-lib
		   output-file))
	  output-file)
	(error "Error: no valid builds found for lipo"))))

(let ((builds (asdf:make-cross-build :iphone :type :lib :monolithic t :move-here #P"./")))
  (let ((build (lipo builds)))
    (format t "Successfully created: ~A ~%" build)))

(si:quit)
