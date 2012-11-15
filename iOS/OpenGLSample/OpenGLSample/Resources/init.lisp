(in-package :cl-user)

(format t "Current path: ~A ~%" (truename #P"./"))
(setq *default-pathname-defaults* (truename #P"./"))
(setq *default-directory* (truename #P"./"))

(defun sysinfo (&optional (out *standard-output*))
  "Print the current environment to a stream."
  (declare (stream out))
  (format out "~&~%~75~~%~75,,,'-:@<<{[ The current environment ]}>~>~%~
Implementation:~20t~a~%~7tversion:~20t~a~%Machine:  type:~20t~a
~7tversion:~20t~a~%~6tinstance:~20t~a~%Opeating System:~19t"
          (lisp-implementation-type) (lisp-implementation-version)
          (machine-type) (machine-version) (machine-instance))
  #+darwin (princ " Darwin")
  #+unix (princ " Unix")
  (format out "~%Software: type:~20t~a~%~7tversion:~20t~a~%Site:~20t~a (~a)
User home:~20t~a~%Current directory:~20t~a~%Default pathname:~20t~a~%"
          (software-type) (software-version) (long-site-name)
          (short-site-name) (user-homedir-pathname) *default-directory*
          *default-pathname-defaults*)
  (format out "Features: ~s.
Modules:~s.~%
Current package:~s~%"
           *features* *modules* *package*)
  (flet ((exdi (fl) (integer-length (nth-value 1 (decode-float fl)))))
    (format out "Fixnum length:~25t~3d bits
Short Floats:~25t~3d bits exponent, ~3d bits significand (mantissa)
Single Floats:~25t~3d bits exponent, ~3d bits significand (mantissa)
Double Floats:~25t~3d bits exponent, ~3d bits significand (mantissa)
Long Floats:~25t~3d bits exponent, ~3d bits significand (mantissa)~%"
            (integer-length most-positive-fixnum)
            (exdi most-positive-short-float)
            (float-digits most-positive-short-float)
            (exdi most-positive-single-float)
            (float-digits most-positive-single-float)
            (exdi most-positive-double-float)
            (float-digits most-positive-double-float)
            (exdi most-positive-long-float)
            (float-digits most-positive-long-float)))
  (dolist (sy '(array-total-size-limit array-rank-limit array-dimension-limit
                lambda-parameters-limit call-arguments-limit
                multiple-values-limit char-code-limit))
    (format out " ~a:~30t~15:d~%" sy (symbol-value sy)))
  (format out "lambda-list-keywords:~s~%"
          lambda-list-keywords)
  (format out "Internal time unit:~25t~f sec~%*gensym-counter*:~25t~:d
Current time:~25t" (/ internal-time-units-per-second) *gensym-counter*)
  (format out "~a" (get-universal-time))
  (format out "~%~75~~%") (room) (values))

(sysinfo)

(defun str (&rest args)
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))

(defun pathname-parent (pathname)
  (make-pathname :directory (or (pathname-directory pathname)
                                (list :relative))
                 :name nil
                 :type nil
                 :defaults pathname))

(require 'ASDF)

(asdf:initialize-source-registry
 `(:source-registry
   (:tree ,(namestring (merge-pathnames #P"slime/" (pathname-parent *load-pathname*))))
   :inherit-configuration))


(asdf:oos 'asdf:load-op :swank)
(in-package :swank-backend)

(defimplementation lisp-implementation-program ()
  "Return the argv[0] of the running Lisp process, or NIL."
  (lisp-implementation-type))

(in-package :swank)

(defun repl-input-stream-read (connection stdin)
  (loop
   (let* ((socket (connection.socket-io connection))
          (inputs (list socket #+nil stdin))
          (ready (wait-for-input inputs)))
     (cond ((eq ready :interrupt)
            (check-slime-interrupts))
           ((member socket ready)
            ;; A Slime request from Emacs is pending; make sure to
            ;; redirect IO to the REPL buffer.
            (with-simple-restart (process-input "Continue reading input.")
              (let ((*sldb-quit-restart* (find-restart 'process-input)))
                (with-io-redirection (connection)
                  (handle-requests connection t)))))
           ((member stdin ready)
            ;; User typed something into the  *inferior-lisp* buffer,
            ;; so do not redirect.
            (return (read-non-blocking stdin)))
           (t (assert (null ready)))))))

(in-package :cl-user)

;; (swank-loader:init :load-contribs nil)

(defun safe-substr (str start &optional length)
  (subseq str 0 (if length (min (length str) length))))

(defun get-ip-address-string (&optional ip-address)
  (let ((ip-vec (or ip-address
                    (sb-bsd-sockets:host-ent-address
                     (sb-bsd-sockets:get-host-by-name
                      (str (machine-instance) ".local"))))))
    (format nil "~d.~d.~d.~d"
            (aref ip-vec 0)
            (aref ip-vec 1)
            (aref ip-vec 2)
            (aref ip-vec 3))))

(mp:process-run-function
 "SLIME-listener"
 (lambda ()
   (with-autorelease-pool ()
     (cond
       ((string-equal (safe-substr (machine-type) 0 2) "iP")
        (let ((swank::*loopback-interface* (get-ip-address-string)))
          (swank:create-server :port 4005 :dont-close t)
          (set-text *label*
                    (format nil "slime: ~a:~a~%"
                            swank::*loopback-interface* 4005))))
       (t
        (swank:create-server :port 4005 :dont-close t)
        (set-text *label*
                  (format nil "slime: ~a:~a~%" "127.0.0.1" 4005)))))))
