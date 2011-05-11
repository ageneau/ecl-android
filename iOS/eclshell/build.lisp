(require 'cmp)

(setq *print-case* :downcase)

(defvar *ecl-root* "/opt/ecl/");#p"/opt/iphone/ecl/")
(defvar *gmp-root* "/opt/gmp/");#p"/opt/iphone/ecl/")
(defvar *default-sdk-ver* "3.0")

(defun compile-if-old (destdir sources &rest options)
  (unless (probe-file destdir)
    (si::mkdir destdir #o0777))
  (mapcar #'(lambda (source)
	      (let ((object (merge-pathnames
                             destdir
                             (compile-file-pathname source :type :object))))
		(unless (and (probe-file object)
			     (>= (file-write-date object) (file-write-date source)))
		  (format t "~&(compile-file ~S :output-file ~S~{ ~S~})~%"
			  source object options)
		  (apply #'compile-file source :output-file object options))
		object))
	  sources))

(defun safe-delete-file (f)
  (ignore-errors (delete-file f)))

(defun clean (sources)
  (dolist (source sources)
    (let ((source (pathname source)))
      (dolist (f (mapcar (lambda (x) (make-pathname :type x :defaults source))
                         '("c" "h" "o" "data")))
        (format t "~&;; deleting ~a" f)
        (safe-delete-file f))
      (let ((f (make-pathname :name (util:str "lib" (pathname-name source)) :type "a")))
        (format t "~&;; deleting ~a" f)
        (safe-delete-file f)))))

(defun build (target source-files &key ecl-include-dir gmp-include-dir cflags sdk sysroot)
  (let* ((compiler::*ecl-include-directory* ecl-include-dir)
         (compiler::*cc* (format nil "~a/usr/bin/gcc-4.2" sdk))
         (compiler::*cc-flags* (util:join (list* (format nil "-I~a" gmp-include-dir) 
						 "-g"
                                                 "-x objective-c"
                                                 "-D__IPHONE_OS_VERSION_MIN_REQUIRED=30000"
                                                 "-O2 -fPIC -fno-common -D_THREAD_SAFE"
                                                 "-Ddarwin -ObjC"
                                                 "-fobjc-abi-version=2"
                                                 "-fobjc-legacy-dispatch"
                                                 (format nil "-isysroot ~a" sysroot)
                                                 cflags)
                                          " "))
         (lisp-files (compile-if-old #p""
                                     source-files
                                     :system-p t
                                     :c-file t
                                     :data-file t
                                     :h-file t)))
    (compiler:build-static-library target :lisp-files lisp-files)))

(defun build-simulator (target source-files)
  (let* ((sdk "/Developer/Platforms/iPhoneSimulator.platform/Developer")
         (sdk-ver *default-sdk-ver*))
    (build target
           source-files
           :ecl-include-dir (concatenate 'string *ecl-root* "iPhoneSimulator/include/") 
	   :gmp-include-dir (concatenate 'string *gmp-root* "iPhoneSimulator/include/") 
           :cflags '("-arch i386")
           :sdk sdk
           :sysroot (format nil "~a/SDKs/iPhoneSimulator~a.sdk" sdk sdk-ver)))
  (let ((lib (util:str "lib" target "_simulator.a")))
   (safe-delete-file lib)
   (rename-file (util:str "lib" target ".a") lib)))


(defun build-device (target source-files)
  (let* ((sdk "/Developer/Platforms/iPhoneOS.platform/Developer")
         (sdk-ver *default-sdk-ver*)
         (sysroot (format nil "~a/SDKs/iPhoneOS~a.sdk" sdk sdk-ver)))
    (build target
           source-files
           :ecl-include-dir (concatenate 'string *ecl-root* "iPhoneOS/include/") 
	   :gmp-include-dir (concatenate 'string *gmp-root* "iPhoneOS/include/") 
           :cflags '("-arch armv6")
           :sdk "/Developer/Platforms/iPhoneOS.platform/Developer"
           :sysroot sysroot))
  (let ((lib (util:str "lib" target "_device.a")))
    (safe-delete-file lib)
    (rename-file (util:str "lib" target ".a") lib)))

(defun lipo (target &key (sdk "/Developer/Platforms/iPhoneOS.platform/Developer"))
  (system:system (util:join (list (util:str sdk "/usr/bin/lipo")
                                  "-arch arm"
                                  (util:str "lib" target "_device.a")
                                  "-arch i386"
                                  (util:str "lib" target "_simulator.a")
                                  "-create"
                                  "-output" (util:str "lib" target ".a"))
                            " ")))

(defun build-all (module
                  sources
                  &key
                  (ecl-root *ecl-root*)
                  (gmp-root *gmp-root*)
                  (sdk-ver *default-sdk-ver*))
  (let ((*ecl-root* ecl-root)
	(*gmp-root* gmp-root)
        (*default-sdk-ver* sdk-ver))
    (clean sources)
    (build-simulator module sources)
    (clean sources)
    (build-device module sources)
    (lipo module)))