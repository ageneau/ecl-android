(require 'asdf)

;; (setq *asdf-verbose* t)
;; (setf *load-verbose* t)
;; (setf *compile-verbose* t)

(ext:package-lock "COMMON-LISP" nil)
(ext:package-lock "C" nil)

;; (trace c::builder)
;; (trace c::compile)
;; (trace c::compile-file)
;; (trace c::build-fasl)

(push :cross *features*)


(defpackage :util
  (:use :cl)
  (:export
   :join))

(in-package :util)

(defun join (seq sep)
  "Concatenate the strings in `seq' delimited by the separator `sep'."
  (format nil (concatenate 'string "~{~a~^" sep "~}") seq))


(in-package "COMPILER")


(defmacro with-android-env (body)
  `(let* ((sdk "/opt/android/android-ndk-r5")
	  (sdk-ver "android-9")
	  (toolchain (format nil "~a/toolchains/~a" "/opt/android/android-ndk-r5" "arm-linux-androideabi-4.4.3/prebuilt/linux-x86"))
	  (sysroot (format nil "~a/platforms/~a" "/opt/android/android-ndk-r5" sdk-ver))
	  (compiler::*ecl-include-directory* "/opt/ecl/android/include/")
	  (compiler::*ecl-library-directory* "/opt/ecl/android/lib/")

	  (compiler::*cc* (format nil "~a/bin/arm-linux-androideabi-gcc" toolchain))
	  (compiler::*ld* (format nil "~a/bin/arm-linux-androideabi-gcc" toolchain))
	  (compiler::*ar* (format nil "~a/bin/arm-linux-androideabi-ar" toolchain))
	  (compiler::*ranlib* (format nil "~a/bin/arm-linux-androideabi-ranlib" toolchain))
	  (compiler::*cc-flags* (util:join (list "-g"
						 (format nil "--sysroot=~a" sysroot)
						 "-DANDROID  -DPLATFORM_ANDROID"
						 "-O2 -fPIC -fno-common -D_THREAD_SAFE"
						 "-I/opt/android/android-ndk-r5/platforms/android-9/arch-arm/usr/include"
						 "-I/opt/gmp/android/include")
                                          " "))
	  (compiler::*ld-flags* (util:join (list "-g"
						 (format nil "--sysroot=~a" sysroot))
					   " ")))
     (,@body)))


(setf (symbol-function 'builder-orig) (symbol-function 'builder))

(defun builder (target output-name &rest args &key lisp-files &allow-other-keys)
  (case target
    ((:library :static-library :lib)
     (let ((lisp-files-cross (mapcar #'(lambda (x) (merge-pathnames (format nil "~a-cross" (pathname-name x)) x)) lisp-files))
	   (output-name-cross (merge-pathnames (format nil "~a-cross" (pathname-name output-name)) output-name)))
       (with-android-env
	   (apply #'builder-orig target output-name :lisp-files lisp-files-cross args))
       (rename-file output-name output-name-cross))
     (apply #'builder-orig target output-name :lisp-files lisp-files args))
    (otherwise
     (apply #'builder-orig target output-name args))))

(in-package :asdf)
(require 'cmp)

(defun* compile-file* (input-file &rest keys &key output-file &allow-other-keys)
  (let* ((output-file (or output-file (apply 'compile-file-pathname* input-file keys)))
	 (output-file-cross (merge-pathnames (format nil "~a-cross" (pathname-name output-file)) output-file))
         (tmp-file (tmpize-pathname output-file))
         (tmp-file-cross (tmpize-pathname output-file-cross))
         (status :error))
    (multiple-value-bind (output-truename warnings-p failure-p)
	(compiler::with-android-env
	    (apply 'compiler::compile-file input-file :output-file tmp-file-cross :c-file t keys))
      (cond
        (failure-p
         (setf status *compile-file-failure-behaviour*))
        (warnings-p
         (setf status *compile-file-warnings-behaviour*))
        (t
         (setf status :success)))
      (ecase status
        ((:success :warn :ignore)
         (delete-file-if-exists output-file-cross)
         (when output-truename
           (rename-file output-truename output-file-cross)
           (setf output-truename output-file-cross)))
        (:error
         (delete-file-if-exists output-truename)
         (setf output-truename nil)))
      (values output-truename warnings-p failure-p))

    (multiple-value-bind (output-truename warnings-p failure-p)
        (apply 'compiler::compile-file input-file :output-file tmp-file :c-file t keys)
      (cond
        (failure-p
         (setf status *compile-file-failure-behaviour*))
        (warnings-p
         (setf status *compile-file-warnings-behaviour*))
        (t
         (setf status :success)))
      (ecase status
        ((:success :warn :ignore)
         (delete-file-if-exists output-file)
         (when output-truename
           (rename-file output-truename output-file)
           (setf output-truename output-file)))
        (:error
         (delete-file-if-exists output-truename)
         (setf output-truename nil)))
      (values output-truename warnings-p failure-p))))
