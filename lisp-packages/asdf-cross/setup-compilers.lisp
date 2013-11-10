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


(in-package :cross-cmp)

(defvar *ecl-host-root* (or (sys:getenv "ECL_INSTALL_ROOT_DIR")
			    (merge-pathnames "../../" (translate-logical-pathname "SYS:"))))
(defvar *ecl-iphonesim-root* (or (sys:getenv "ECL_IPHONESIM_ROOT_DIR")
				 (merge-pathnames "../iPhoneSimulator/" *ecl-host-root*)))
(defvar *ecl-iphoneos-root* (or (sys:getenv "ECL_IPHONEOS_ROOT_DIR")
				(merge-pathnames "../iPhoneOS/" *ecl-host-root*)))
(defvar *ecl-android-arm-root* (or (sys:getenv "ECL_ANDROID_ARM_ROOT_DIR")
				   (merge-pathnames "../android/" *ecl-host-root*)))
(defvar *ecl-android-x86-root* (or (sys:getenv "ECL_ANDROID_X86_ROOT_DIR")
				   (merge-pathnames "../androidx86/" *ecl-host-root*)))
(defvar *ecl-nacl32-root* (or (sys:getenv "ECL_NACL32_ROOT_DIR")
				   (merge-pathnames "../nacl32/" *ecl-host-root*)))
(defvar *ecl-nacl64-root* (or (sys:getenv "ECL_NACL64_ROOT_DIR")
				   (merge-pathnames "../nacl64/" *ecl-host-root*)))


(defvar *cross-compilers* (make-hash-table :test #'equalp))

(defun get-compiler (arch)
  (gethash arch *cross-compilers*))

(defun registered-compilers ()
  *cross-compilers*)

(defclass cross-compiler ()
  ((features :accessor features :initarg :features)
   (sys-pathname-translation :accessor sys-pathname-translation :initarg :sys-pathname-translation)
   (cmpinclude :accessor cmpinclude :initarg :cmpinclude)
   (cc :accessor cc :initarg :cc)
   (ld :accessor ld :initarg :ld)
   (ranlib :accessor ranlib :initarg :ranlib)
   (ar :accessor ar :initarg :ar)
   (cc-flags :accessor cc-flags :initarg :cc-flags)
   (cc-optimize :accessor cc-optimize :initarg :cc-optimize)
   (ld-format :accessor ld-format :initarg :ld-format)
   (cc-format :accessor cc-format :initarg :cc-format)
   (ld-flags :accessor ld-flags :initarg :ld-flags)
   
   (ld-shared-flags :accessor ld-shared-flags :initarg :ld-shared-flags)
   (ld-bundle-flags :accessor ld-bundle-flags :initarg :ld-bundle-flags)
   
   (shared-library-prefix :accessor shared-library-prefix :initarg :shared-library-prefix)
   (shared-library-extension :accessor shared-library-extension :initarg :shared-library-extension)
   (shared-library-format :accessor shared-library-format :initarg :shared-library-format)
   (static-library-prefix :accessor static-library-prefix :initarg :static-library-prefix)
   (static-library-extension :accessor static-library-extension :initarg :static-library-extension)
   (static-library-format :accessor static-library-format :initarg :static-library-format)
   (object-file-extension :accessor object-file-extension :initarg :object-file-extension)
   (executable-file-format :accessor executable-file-format :initarg :executable-file-format)
   
   (ecl-include-directory :accessor ecl-include-directory :initarg :ecl-include-directory)
   (ecl-library-directory :accessor ecl-library-directory :initarg :ecl-library-directory)
   
   (ld-rpath :accessor ld-rpath :initarg :ld-rpath)
   (user-cc-flags :accessor user-cc-flags :initarg :user-cc-flags)
   (user-ld-flags :accessor user-ld-flags :initarg :user-ld-flags)))


(defmethod cross-compile ((compiler cross-compiler) func)
  (let ((old-translation (si::pathname-translations "SYS")))
    (unwind-protect
	 (progn
	   (si::pathname-translations "SYS" (slot-value compiler 'sys-pathname-translation))
	   (let ((common-lisp:*features* (slot-value compiler 'features))
		 (c::*user-cc-flags* (slot-value compiler 'user-cc-flags))
		 (c::*user-ld-flags* (slot-value compiler 'user-ld-flags))
		 (c::*cmpinclude* (slot-value compiler 'cmpinclude))
		 (c::*cc* (slot-value compiler 'cc))
		 (c::*ld* (slot-value compiler 'ld))
		 (c::*ranlib* (slot-value compiler 'ranlib))
		 (c::*ar* (slot-value compiler 'ar))
		 (c::*cc-flags* (slot-value compiler 'cc-flags))
		 (c::*cc-optimize* (slot-value compiler 'cc-optimize))
		 (c::*ld-format* (slot-value compiler 'ld-format))
		 (c::*cc-format* (slot-value compiler 'cc-format))
		 
		 (c::*ld-flags* (slot-value compiler 'ld-flags))
		 
		 (c::*ld-shared-flags* (slot-value compiler 'ld-shared-flags))
		 (c::*ld-bundle-flags* (slot-value compiler 'ld-bundle-flags))
		 
		 (c::+shared-library-prefix+ (slot-value compiler 'shared-library-prefix))
		 (c::+shared-library-extension+ (slot-value compiler 'shared-library-extension))
		 (c::+shared-library-format+ (slot-value compiler 'shared-library-format))
		 (c::+static-library-prefix+ (slot-value compiler 'static-library-prefix))
		 (c::+static-library-extension+ (slot-value compiler 'static-library-extension))
		 (c::+static-library-format+ (slot-value compiler 'static-library-format))
		 (c::+object-file-extension+ (slot-value compiler 'object-file-extension))
		 (c::+executable-file-format+ (slot-value compiler 'executable-file-format))
		 
		 (c::*ecl-include-directory* (slot-value compiler 'ecl-include-directory))
		 (c::*ecl-library-directory* (slot-value compiler 'ecl-library-directory))
		 
		 (c::*ld-rpath* (slot-value compiler 'ld-rpath)))
	     (funcall func)))
      (si::pathname-translations "SYS" old-translation))))

(defun setup-compilers ()
  (loop
     for dir in (list *ecl-iphonesim-root*
		      *ecl-iphoneos-root*
		      *ecl-android-arm-root*
		      *ecl-android-x86-root*
		      *ecl-nacl32-root*
		      *ecl-nacl64-root*)
     do (if (probe-file dir)
	    (let* ((platform-dir (namestring (truename dir)))
		   (crosscmp-host (enough-namestring
				   (truename (translate-logical-pathname "SYS:crosscmp.lsp"))
				   (truename *ecl-host-root*)))
		   (cross-cmp (merge-pathnames crosscmp-host
					       platform-dir)))
	      (if (probe-file cross-cmp)
		  (load cross-cmp)
		  (warn "No cross-compiler found at ~a" cross-cmp)))
	    (warn "Directory not found ~a" dir)))

  (loop
     for arch being the hash-keys of (registered-compilers)
     using (hash-value compiler)
     do (cond ((search "apple-darwin" arch)
	       (progn 
		 (setf (cc-flags compiler)
		       (concatenate 'string
				    "-x objective-c -fobjc-abi-version=2 -fobjc-legacy-dispatch "
				    (cc-flags compiler)))
		 (setf (ld-flags compiler)
		       (concatenate 'string
				    "-x objective-c -fobjc-abi-version=2 -fobjc-legacy-dispatch "
				    (ld-flags compiler))))))))

(setup-compilers)
