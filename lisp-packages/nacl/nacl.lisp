(in-package :nacl)

(clines
"#include \"ppapi/c/pp_macros.h\""
"#include \"ppapi/c/pp_stdint.h\""
"#include \"ppapi/c/pp_errors.h\""
"#include \"ppapi/c/pp_module.h\""
"#include \"ppapi/c/pp_var.h\""
"#include \"ppapi/c/ppb.h\""
"#include \"ppapi/c/ppb_instance.h\""
"#include \"ppapi/c/ppb_messaging.h\""
"#include \"ppapi/c/ppb_var.h\""
"#include \"ppapi/c/ppp.h\""
"#include \"ppapi/c/ppp_instance.h\""
"#include \"ppapi/c/ppp_messaging.h\""
"extern PPB_Messaging* ppb_messaging_interface;"
"extern PPB_Var* ppb_var_interface;"
"extern struct PP_Var CStrToVar(const char* str);"
"extern PP_Instance nacl_instance;"
)

(defun post-message (msg)
  (c-inline (msg) (:cstring) :void
    "ppb_messaging_interface->PostMessage(nacl_instance,CStrToVar(#0))"
    :one-liner t))


(defclass wrapped-stream (fundamental-stream)
  ((stream :initarg :stream :reader stream-of)))

(defmethod stream-element-type ((stream wrapped-stream))
  (stream-element-type (stream-of stream)))

(defmethod close ((stream wrapped-stream) &key abort)
  (close (stream-of stream) :abort abort))

(defclass wrapped-character-output-stream
    (wrapped-stream fundamental-character-output-stream)
  ((col-index :initform 0 :accessor col-index-of)))

(defmethod stream-line-column ((stream wrapped-character-output-stream))
  (col-index-of stream))

(defmethod stream-write-char ((stream wrapped-character-output-stream)
			      char)
  (with-accessors ((inner-stream stream-of) (cols col-index-of)) stream
    (post-message (format nil "lisp:~C" char))
    (write-char char inner-stream)
    (if (char= char #\Newline)
	(setf cols 0)
	(incf cols))))

(defclass prefixed-character-output-stream
    (wrapped-character-output-stream)
  ((prefix :initarg :prefix :reader prefix-of)))

(defgeneric write-prefix (prefix stream)
  (:method ((prefix string) stream) (write-string prefix stream))
  (:method ((prefix function) stream) (funcall prefix stream)))

(defmethod stream-write-char ((stream prefixed-character-output-stream)
			      char)
  (with-accessors ((inner-stream stream-of) (cols col-index-of)
		   (prefix prefix-of)) stream
    (when (zerop cols)
      (write-prefix prefix inner-stream))
    (call-next-method)))

(defun test-stream ()
  (flet ((format-timestamp (stream)
	   (apply #'format stream "[~2@*~2,' D:~1@*~2,'0D:~0@*~2,'0D] "
		  (multiple-value-list (get-decoded-time)))))
    (let ((output (make-instance 'prefixed-character-output-stream
				 :stream *standard-output*
				 :prefix #'format-timestamp)))
      (loop for string in '("abc" "def" "ghi") do
      	 (write-line string output)
      	 (sleep 1))
;;      (si::top-level)
      )))
