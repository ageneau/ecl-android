(setq c::*compile-in-constants* t)
(load "/opt/ecl/android/lib/crosscmp")
(pushnew :cross *features*)

(in-package :asdf)

(defun cross-pathname (x)
  (merge-pathnames (format nil "~a-cross" (pathname-name x)) x))

(defun tmp-pathname (x)
  (merge-pathnames (format nil "~a-tmp" (pathname-name x)) x))

(defun compile-cross (input-file &rest keys &key output-file &allow-other-keys)
  (declare (ignore output-file))
  (if (member :ecl-bytecmp *features*)
      (apply 'compile-file input-file keys)
      (let ((ret
	     (multiple-value-bind (object-file flags1 flags2)
		 (apply 'compile-file* input-file :system-p t keys)
	       (ext:copy-file output-file
			      (tmp-pathname output-file))
	       (values (and object-file
			    (c::build-fasl (compile-file-pathname object-file :type :fasl)
					   :lisp-files (list object-file))
			    object-file)
		       flags1
		       flags2))))
	(c::with-crosscomp-env
	    (let ((*features* (remove :dlopen *features*)))
	      (apply 'compile-file* input-file :system-p t keys)))
	(rename-file output-file
		     (cross-pathname output-file) :if-exists :supersede)
	(rename-file (tmp-pathname output-file)
		     output-file :if-exists :supersede)
	ret)))

(setq *compile-op-compile-file-function* 'compile-cross)

(defmethod perform ((o bundle-op) (c system))
  (let* ((object-files (remove "fas" (input-files o c)
                               :key #'pathname-type :test #'string=))
	 (cross-object-files (mapcar #'cross-pathname object-files))
         (output (output-files o c))
	 (output-cross (mapcar 'cross-pathname output)))

    (ensure-directories-exist (first output))
    (let ((res
	   (apply #'c::builder (bundle-op-type o) (first output) :lisp-files object-files
		  (append (bundle-op-build-args o)
			  (when (and (typep o 'monolithic-bundle-op)
				     (monolithic-op-prologue-code o))
			    `(:prologue-code ,(monolithic-op-prologue-code o)))
			  (when (and (typep o 'monolithic-bundle-op)
				     (monolithic-op-epilogue-code o))
			    `(:epilogue-code ,(monolithic-op-epilogue-code o)))))))
    
      (c::with-crosscomp-env
	  (apply #'c::builder (bundle-op-type o) (first output-cross) :lisp-files cross-object-files
		 (append (bundle-op-build-args o)
			 (when (and (typep o 'monolithic-bundle-op)
				    (monolithic-op-prologue-code o))
			   `(:prologue-code ,(monolithic-op-prologue-code o)))
			 (when (and (typep o 'monolithic-bundle-op)
				    (monolithic-op-epilogue-code o))
			   `(:epilogue-code ,(monolithic-op-epilogue-code o))))))
      res)))
