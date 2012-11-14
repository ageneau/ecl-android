

(defun threads-info ()
  (format t "~%")
  (dolist (tt (sb-thread:list-all-threads))
    (format t "Thread ~S tid=~D~%"
	    (sb-thread::thread-name tt)
	    (sb-thread::thread-os-thread tt)))
  )