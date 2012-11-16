;;; -*- mode: common-lisp; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
;;;
;;; (c) 2011, Juan Jose Garcia-Ripoll
;;;
;;; Set up the test environment.
;;;

(defpackage :ecl-tests
  (:use :cl))

(in-package :ecl-tests)

(setf *load-verbose* nil *load-print* nil)

(defvar *here* (merge-pathnames "./"))

(defvar *cache* (merge-pathnames "./cache/" *here*))

(defvar *output-directory*
  (merge-pathnames "output/" *here*))

(defvar *quicklisp-sandbox* (merge-pathnames "quicklisp/" *here*))

(defvar *quicklisp-install-file* (merge-pathnames "quicklisp.lsp" *cache*))

(defvar *quicklisp-setup-file* (merge-pathnames "setup.lisp" *quicklisp-sandbox*))

(defvar *wild-inferiors* (make-pathname :name :wild
					:type :wild
					:version :wild
					:directory '(:relative :wild-inferiors)))

(defvar *cleanup-extensions* '("fasl" "fasb" "c" "h" "obj" "o" "a" "lib" "dll" "dylib" "data"))

(require :ecl-curl)
(require :deflate)
(require :ql-minitar)

;;;
;;; PREPARATION OF DIRECTORIES AND FILES
;;;

(defun setup-asdf ()
  (require :asdf)
  (ensure-directories-exist *cache*)
  (setf (symbol-value (read-from-string "asdf::*user-cache*"))
	(list *cache* :implementation)))
		      

(defun delete-everything (path)
  ;; Recursively run through children
  (labels ((recursive-deletion (path)
             (mapc #'delete-everything
                   (directory (merge-pathnames
                               (make-pathname :name nil
                                              :type nil
                                              :directory '(:relative :wild)
                                              :defaults path)
                               path)))
             ;; Delete files
             (loop for f in (directory (make-pathname :name :wild
                                                      :type :wild
                                                      :defaults path))
                do (delete-file f)
                finally (delete-file path))))
    (and (probe-file path)
         (recursive-deletion path))))

(defun safe-download (url filename)
  (ensure-directories-exist filename)
  (handler-case
      (ecl-curl:download-url-to-file url filename)
    (ecl-curl:download-error (c)
      (format t "~&;;;~%;;; Unable to download quicklisp. Aborting. ~%;;;")
      (ext:quit 1)))
  filename)

(defun download-quicklisp-install ()
  (safe-download "http://beta.quicklisp.org/quicklisp.lisp"
		 *quicklisp-install-file*))

(defun download-and-setup-quicklisp ()
  (when (probe-file *quicklisp-sandbox*)
    (delete-everything *quicklisp-sandbox*))
  (handler-case
      (progn
	(load (download-quicklisp-install))
	(let ((function (read-from-string "quicklisp-quickstart:install")))
	  (eval (list function :path *quicklisp-sandbox*))))
    (error (c)
      (format t "~&;;;~%;;; Unable to setup quicklisp. Aborting.~%;;;")
      (delete-everything *quicklisp-sandbox*))))

(defun ensure-quicklisp ()
  (unless (find-package "QL")
    (unless (probe-file *quicklisp-sandbox*)
      (setup-asdf)
      (download-and-setup-quicklisp))
    (load *quicklisp-setup-file*))
  t)

(defun copy-directory (orig dest)
  (loop for f in (directory (merge-pathnames *wild-inferiors* orig))
     for f2 = (enough-namestring f orig)
     for f3 = (merge-pathnames f2 dest)
     unless (probe-file f3)
     do (ensure-directories-exist f3)
     do (ext:copy-file f f3)))

(defun extract-tarball (filename)
  (format t "~&;;;~%;;; Extracting ~a~%;;;" filename)
  (if (string-equal (pathname-type filename) "gz")
      (let ((temp-filename (ext:mkstemp "fooXXXXXXX")))
	(unwind-protect
	     (progn
	       (deflate:gunzip filename temp-filename)
	       (extract-tarball temp-filename))
	  (delete-file temp-filename)))
      (ql-minitar:unpack-tarball filename)))

(defun extract-distribution (filename url)
  (let ((distribution (loop for base in (list *cache*
					      *here*
					      *test-sources*)
			 for file = (merge-pathnames filename base)
			 when (probe-file file)
			 do (return file)
			 finally (let ((tmp (merge-pathnames filename *cache*)))
				   (return (safe-download url tmp))))))
    (extract-tarball distribution)))


(defun cleanup-directory (path)
  (loop for i in (directory (merge-pathnames *wild-inferiors*
					     path))
     when (member (pathname-type i) *cleanup-extensions* :test #'string-equal)
     do (delete-file i)))

(defvar *quicklisp-library-list*
  '(trivial-features
    alexandria
    babel
    cffi
    cl-ppcre
    cl-unicode
    iterate
    trivial-gray-streams
    trivial-garbage
    flexi-streams
    lift
    metabang-bind
    swank
    stefil
    sqlite
    chunga
    cl+ssl
    cl-base64
    cl-fad
    cl-python
    md5
    rfc2388
    trivial-backtrace
    trivial-gray-streams
    usocket
    hunchentoot))

(defconstant +quicklisp-build-template+ "
(require 'asdf)
(setf (symbol-value (read-from-string \"asdf::*user-cache*\"))
      (list ~s :implementation))
(load ~s)
(ql:use-only-quicklisp-systems)
(handler-case
  (progn
    (ql:quickload ~s)
    (princ \"ECL-BUILD-OK\"))
  (serious-condition (c) (princ c)))
#+ecl
(ext:quit)
#+sbcl
(sb-ext:quit)
")

(defconstant +quicklisp-test-template+ "
(require 'asdf)
(setf (symbol-value (read-from-string \"asdf::*user-cache*\"))
      (list ~s :implementation))
(load ~s)
(ql:use-only-quicklisp-systems)
(handler-case
  (progn
    (ql:quickload ~s)
    (princ \"ECL-BUILD-OK\")
    (asdf:oos 'asdf:test-op ~:*~s)
    (princ \"ECL-TEST-OK\"))
  (serious-condition (c) (princ c)))
#+ecl
(ext:quit)
#+sbcl
(sb-ext:quit)
")

(defun run-quicklisp-tests (&optional (output (merge-pathnames "quicklisp.log"
							       *output-directory*)))
  (mapcar #'delete-everything (directory (merge-pathnames "*/" *cache*)))
  (let ((quicklisp-logs (merge-pathnames "quicklisp.logs/" *output-directory*)))
    (labels ((build-or-test-job (name suffix template)
	       (let* ((name (string-downcase name))
		      (log-name (concatenate 'string name suffix))
		      (build-log (ensure-directories-exist
				  (merge-pathnames log-name quicklisp-logs))))
		 (multiple-value-bind (stream status process)
		     (ext:run-program *test-image*
				      *test-image-args*
				      :input :stream
				      :output build-log
				      :error :output
				      :wait nil)
		   (unwind-protect
			(progn
			  (format stream template
				  (namestring *cache*)
				  (namestring *quicklisp-setup-file*)
				  name)
			  (format t template
				  (namestring *cache*)
				  (namestring *quicklisp-setup-file*)
				  name)
			  (force-output stream))
		     (close stream)
		     (ext:external-process-wait process t)
		     ))))
	     (build-job (name)
	       (build-or-test-job name "-build.log" +quicklisp-build-template+))
	     (test-job (name)
	       (build-or-test-job name "-test.log" +quicklisp-test-template+)))
      (mapc #'build-job *quicklisp-library-list*)
      (mapc #'test-job *quicklisp-library-list*))))
