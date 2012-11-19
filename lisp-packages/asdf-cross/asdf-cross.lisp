;;;
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
;;; ECL SPECIFIC OPERATIONS FOR ASDF
;;;
;;; We're defining 3 new operations for cross compilation using ECL:
;;; cross-lib-op to build cross-compiled libs (see lib-op in ECL doc)
;;; monolithic-cross-lib-op to build cross-compiled monolithic library bundles (see monolithic-lib-op)
;;; monolithic
;;; You need to have both a host (the cross compiler) and cross compiled ECL for this to work
;;; When building the cross-compiled ECL, a file "crosscomp.lsp" will be generated that allows configuring the host ECL as a cross-compiler. This file needs to be loaded before using these operations.
;;; See build.lisp for usage.
;;;

(in-package :asdf-cross)

(defun compile-cross (input-file &rest keys &key output-file arch &allow-other-keys)
  (declare (ignore output-file))
  (funcall
   #'cross-compile
   (get-compiler arch)
   #'(lambda ()
       (apply 'compile-file* input-file :system-p t (remove-keys '(arch) keys)))))

(defclass cross-compile-op (compile-op)
  ((arch :initarg :arch)))

(defmethod component-depends-on ((o cross-compile-op) (c system))
  (mapcar #'(lambda (x)
	      (if (string= (car x) 'compile-op)
		  (cons 'cross-compile-op (cdr x))
		  x))
	  (component-depends-on (make-instance 'compile-op) c)))

(defmethod perform ((operation cross-compile-op) (c cl-source-file))
  (let ((source-file (component-pathname c))
	(output-file (first (output-files operation c)))
	(*compile-file-warnings-behaviour* (operation-on-warnings operation))
	(*compile-file-failure-behaviour* (operation-on-failure operation)))
    (multiple-value-bind (output warnings-p failure-p)
        (apply 'compile-cross source-file
               :output-file output-file
	       :arch (slot-value operation 'arch)
	       (compile-op-flags operation))
      (unless output
        (error 'compile-error :component c :operation operation))
      (when failure-p
        (case (operation-on-failure operation)
          (:warn (warn
                  (compatfmt "~@<COMPILE-FILE failed while performing ~A on ~A.~@:>")
                  operation c))
          (:error (error 'compile-failed :component c :operation operation))
          (:ignore nil)))
      (when warnings-p
        (case (operation-on-warnings operation)
          (:warn (warn
                  (compatfmt "~@<COMPILE-FILE warned while performing ~A on ~A.~@:>")
                  operation c))
          (:error (error 'compile-warned :component c :operation operation))
          (:ignore nil))))))

(defmethod output-files ((operation cross-compile-op) (c cl-source-file))
  (declare (ignorable operation))
  (let* ((output-comp (output-files (make-instance 'compile-op) c))
	 (cross-object-files (remove "fas" output-comp
				     :key #'pathname-type :test #'string=)))
    cross-object-files))


(defclass cross-bundle-op (bundle-op)
  ((arch :initarg :arch)))

(defclass cross-monolithic-bundle-op (cross-bundle-op)
  ((prologue-code :accessor monolithic-op-prologue-code)
   (epilogue-code :accessor monolithic-op-epilogue-code)
   (arch :initarg :arch)))

(defclass cross-lib-op (cross-bundle-op)
  ((type :initform :lib)
   (arch :initarg :arch)))

(defclass cross-monolithic-lib-op (cross-monolithic-bundle-op)
  ((type :initform :lib)
   (arch :initarg :arch)))


(defmethod bundle-sub-operations ((o cross-lib-op) c)
  (gather-components 'cross-compile-op c
                     :filter-system (and (not (bundle-op-monolithic-p o)) c)
                     :filter-type '(not system)))

(defmethod bundle-sub-operations ((o cross-monolithic-lib-op) c)
  (gather-components 'cross-compile-op c
                     :filter-system nil
                     :filter-type '(not system)))


(defmethod component-depends-on ((o cross-lib-op) (c system))
  (declare (ignorable o))
  (list (list 'cross-compile-op (component-name c))))

(defmethod component-depends-on ((o cross-monolithic-lib-op) (c system))
  (declare (ignorable o))
  (list (list 'cross-compile-op (component-name c))))

(defmethod perform ((o cross-lib-op) (c system))
  (funcall
   #'cross-compile
   (get-compiler (slot-value o 'arch))
   #'call-next-method))

(defmethod perform ((o cross-monolithic-lib-op) (c system))
  (funcall
   #'cross-compile
   (get-compiler (slot-value o 'arch))
   #'call-next-method))

(defmethod initialize-instance :after ((instance cross-bundle-op) &rest initargs
                                       &key (arch)
                                       &allow-other-keys)
  (declare (ignorable initargs))
  (setf (bundle-op-build-args instance)
        (remove-keys '(arch name-suffix)
                     (slot-value instance 'original-initargs))))

(defmethod bundle-op-build-args :around ((op cross-bundle-op))
  (declare (ignorable op))
  (let ((args (call-next-method)))
    (remf args :arch)
    args))

(defclass cross-program-op (cross-monolithic-bundle-op)
  ((type :initform :program)
   (arch :initarg :arch)))

(defmethod perform ((o cross-program-op) (c system))
  (funcall
   'cross-compile
   (get-compiler (slot-value o 'arch))
   #'(lambda ()
       (let ((prebuilts (loop
			   :for (o . c) :in (bundle-sub-operations o c)
			   :when (typep c 'prebuilt-system)
			   :collect (component-name c))))
	 (setf (slot-value o 'epilogue-code)
	       `(progn
		  (asdf::register-pre-built-system "ASDF")
		  ,@(loop :for package in prebuilts
		       :collect `(asdf::register-pre-built-system ,package))
		  (si::top-level t)))
	 (call-next-method)))))

(defmethod bundle-sub-operations ((o cross-program-op) c)
  (mapcar #'(lambda (x) (cons (make-instance 'cross-lib-op) (cdr x)))
	  (bundle-sub-operations (make-instance 'program-op) c)))

;; Prebuilt systems

(defmethod output-files ((o cross-lib-op) (c prebuilt-system))
  (output-files (make-instance 'lib-op) c))


(defmethod perform ((o load-op) (c prebuilt-system))
  ())

;; (defmethod perform ((o load-op) (c compiled-file))
;;   ())

(defun make-cross-build (system &rest args &key (monolithic nil) (type :lib)
			 (move-here nil move-here-p)
			 (arch-list :all)
			 &allow-other-keys)  
  (oos 'load-op system)

  (unwind-protect
       (loop
	  for arch being the hash-keys of *cross-compilers*
	  when (or (eq arch-list :all) (member arch arch-list :test #'equalp))
	  nconc
	    (let ((common-lisp:*features* (cons :cross common-lisp:*features*))
		  (*user-cache* (list *user-cache* arch)))

	      (clear-output-translations)
	      ;; This forces reloading of system definitions forms
	      ;; Needed for :cross handling
	      (clrhash *defined-systems*)

	      (let* ((operation-name (ecase type
				       ((:lib :static-library)
					(if monolithic 'cross-monolithic-lib-op 'cross-lib-op))
				       ((:program)
					'cross-program-op)))
		     (move-here-path (if (and move-here
					      (typep move-here '(or pathname string)))
					 (pathname move-here)
					 (merge-pathnames "./asdf-output/")))
		     (keys (append
			    (remove-keys '(monolithic type move-here arch-list) args)
			    (list ;;:name-suffix nil
				  :arch arch)))
		     (operation (apply #'operate operation-name
				       system
				       keys))
		     (system (find-system system))
		     (files (and system (output-files operation system))))

		(if (or move-here (and (null move-here-p)
				       (member operation-name '(:program :binary))))
		    (loop with dest-path = (truename (ensure-directories-exist move-here-path))
		       for f in files
		       for new-f = (make-pathname :name (concatenate 'string
								     (pathname-name f)
								     "_"
								     arch)
						  :type (pathname-type f)
						  :defaults dest-path)
		       do (progn
			    (when (probe-file new-f)
			      (delete-file new-f))
			    (rename-file f new-f))
		       collect new-f)
		    files))))
    (progn
      (clear-output-translations)
      ;; Future compilations need to reload systems without :cross in features
      (clrhash *defined-systems*))))

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

(defun build-ios (system)
  (let ((builds (make-cross-build system
				  :type :lib
				  :monolithic t
				  :arch-list '("arm-apple-darwin" "i686-apple-darwin")
				  :move-here #P"./")))
    (let ((build (lipo builds)))
      (format t "Successfully created: ~A ~%" build))))
