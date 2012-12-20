(format t "ECL (Embeddable Common-Lisp) ~A (git:~D)~%"
	(lisp-implementation-version)
	(ext:lisp-implementation-vcs-id))

(in-package :cl-user)

(require 'SOCKETS)
(require 'ASDF)

(setq *default-directory* *default-pathname-defaults*)

(defvar *ecl-home* *default-directory*)

(ext:setenv "HOME" (namestring *ecl-home*))

(pushnew (namestring *default-pathname-defaults*) asdf:*central-registry*)

(asdf:oos 'asdf:load-op :swank)


;; The following "patches" swank to work correctly on android/iOS

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

(swank-loader:init :load-contribs nil)


(mp:process-run-function
 "SLIME-listener"
 (lambda ()
   (swank:create-server :port 4005
			:dont-close t)))


(cond ((probe-file #P"user.lisp")
       (load "user")))

