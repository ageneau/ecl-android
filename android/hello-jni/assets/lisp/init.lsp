(in-package :cl-user)
(require 'SOCKETS)

(setq *default-directory* *default-pathname-defaults*)

(defun str (&rest args)
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))

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

(setq *default-pathname-defaults* (merge-pathnames #P"slime/"))

(load "swank-loader")
(swank-loader:init :load-contribs nil)
  
(load "contrib/swank-package-fu")
(load "contrib/swank-presentations")
(load "contrib/swank-fuzzy")
(load "contrib/swank-c-p-c")
(load "contrib/swank-fancy-inspector")
(load "contrib/swank-arglists")

(setq *default-pathname-defaults* *default-directory*)
(in-package :cl-user)

(mp:process-run-function
 "SLIME-listener"
 (lambda ()
   (cond
     ((string-equal (safe-substr (machine-type) 0 2) "iP")
      (let ((swank::*loopback-interface* (get-ip-address-string)))
	(swank:create-server :port 4005 :dont-close t)
	(format nil "slime: ~a:~a~%"
		swank::*loopback-interface* 4005)))
     (t
      (swank:create-server :port 4005 :dont-close t)
      (format nil "slime: ~a:~a~%" "127.0.0.1" 4005)))))

(cond ((probe-file #P"user.lsp")
       (load "user.lsp")))

