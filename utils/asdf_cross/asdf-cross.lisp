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

(require :asdf)
(in-package :asdf)


(defun cross-pathname (x)
  (merge-pathnames (format nil "~a-cross" (pathname-name x)) x))

(defclass cross-compile-op (operation)
  ((proclamations :initarg :proclamations :accessor compile-op-proclamations :initform nil)
   (on-warnings :initarg :on-warnings :accessor operation-on-warnings
                :initform *compile-file-warnings-behaviour*)
   (on-failure :initarg :on-failure :accessor operation-on-failure
               :initform *compile-file-failure-behaviour*)
   (flags :initarg :flags :accessor compile-op-flags
          :initform nil)))

;; (defmethod component-depends-on ((o cross-compile-op) (c component))
;;   (list (list 'load-op (component-name c))))

(defmethod component-depends-on ((o cross-compile-op) (c system))
  (mapcar #'(lambda (x)
	      (if (string= (car x) 'compile-op)
		  (cons 'cross-compile-op (cdr x))
		  x))
	  (component-depends-on (make-instance 'compile-op) c)))

(defmethod perform ((o cross-compile-op) (c component))
  ())

(defun compile-cross (input-file &rest keys &key output-file &allow-other-keys)
  (declare (ignore output-file))
  (c::with-crosscomp-env
      (apply 'compile-file* input-file :system-p t keys)))

(defmethod perform :before ((operation cross-compile-op) (c source-file))
   (loop :for file :in (asdf:output-files operation c)
     :for pathname = (if (typep file 'logical-pathname)
                         (translate-logical-pathname file)
                         file)
     :do (ensure-directories-exist pathname)))

(defmethod perform ((operation cross-compile-op) (c cl-source-file))
  (let ((source-file (component-pathname c))
	(output-file (first (output-files operation c)))
	(*compile-file-warnings-behaviour* (operation-on-warnings operation))
	(*compile-file-failure-behaviour* (operation-on-failure operation)))
    (multiple-value-bind (output warnings-p failure-p)
        (apply 'compile-cross source-file
               :output-file output-file (compile-op-flags operation))
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


(defmethod output-files ((operation cross-compile-op) (c component))
  (declare (ignorable operation))
  (let ((output-comp (output-files (make-instance 'compile-op) c)))
    (mapcar #'cross-pathname output-comp)))

(defmethod output-files ((operation cross-compile-op) (c cl-source-file))
  (declare (ignorable operation))
  (let* ((output-comp (output-files (make-instance 'compile-op) c))
	 (cross-object-files (mapcar #'cross-pathname
				     (remove "fas" output-comp
					     :key #'pathname-type :test #'string=))))
    cross-object-files))

(defclass cross-lib-op (lib-op)
  ((type :initform :lib)))

(defclass cross-monolithic-lib-op (monolithic-lib-op)
  ((type :initform :lib)))


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


(defmethod output-files ((o cross-lib-op) (c system))
  (mapcar #'cross-pathname (call-next-method)))

(defmethod output-files ((o cross-monolithic-lib-op) (c system))
  (mapcar #'cross-pathname (call-next-method)))

(defmethod perform ((o cross-lib-op) (c system))
  (c::with-crosscomp-env
      (call-next-method)))

(defmethod perform ((o cross-monolithic-lib-op) (c system))
  (c::with-crosscomp-env
      (call-next-method)))

(defclass cross-program-op (program-op)
  ())

(defmethod perform ((o cross-program-op) (c system))
  (c::with-crosscomp-env
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
      	(call-next-method))))

(defmethod bundle-sub-operations ((o cross-program-op) c)
  (mapcar #'(lambda (x) (cons (make-instance 'cross-lib-op) (cdr x)))
	  (call-next-method)))

(defmethod output-files ((o cross-program-op) (c system))
  (mapcar #'cross-pathname (call-next-method)))

;; Prebuilt systems

(defmethod output-files ((o cross-lib-op) (c prebuilt-system))
  (output-files (make-instance 'lib-op) c))


(defmethod perform ((o load-op) (c prebuilt-system))
  ())

;; (defmethod perform ((o load-op) (c compiled-file))
;;   ())


(defun make-cross-build (system &rest args &key (monolithic nil) (type :lib)
			 (move-here nil move-here-p)
			 &allow-other-keys)  
  (oos 'load-op system)
  
  (let ((*features* (cons :cross *features*)))
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
	       (operation (apply #'operate operation-name
				 system
				 (remove-keys '(monolithic type move-here) args)))
	       (system (find-system system))
	       (files (and system (output-files operation system))))
	  
	  (if (or move-here (and (null move-here-p)
				 (member operation-name '(:program :binary))))
	      (loop with dest-path = (truename (ensure-directories-exist move-here-path))
		 for f in files
		 for new-f = (make-pathname :name (pathname-name f)
					    :type (pathname-type f)
					    :defaults dest-path)
		 do (progn
		      (when (probe-file new-f)
			(delete-file new-f))
		      (rename-file f new-f))
		 collect new-f)
	      files))))

(export 'make-cross-build)