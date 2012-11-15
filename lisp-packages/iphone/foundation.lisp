;; This code was originally written by Ram Krishnan
;; https://github.com/kriyative/ecl-iphone-builder

(in-package :iphone)

(clines
 "#import <Foundation/Foundation.h>"
 )

#+ffi
(defmacro def-ffi-enum (name bindings &key (type :int))
  `(progn
     (defvar ,name
       (list
	,@(mapcar (lambda (binding)
		    `(cons ,(first binding)
			   (ffi:c-inline () ()
			       ,type
			     ,(second binding)
			     :one-liner t)))
		  bindings)))
     (defun ,(sym "keyword->" name) (key)
       (cdr (find key ,name :key 'car)))
     (defun ,(sym name "->keyword") (key)
       (car (find key ,name :key 'cdr)))))

(defvar *objc-nil* (c-fficall () :pointer-void "nil" :one-liner t))

(defun ns-log (msg)
  (c-fficall ((msg :cstring)) :void
    "NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init];
     NSLog([NSString stringWithCString: #0]);
     [pool release];"))

(defun make-NSString (string)
  (if string
      (c-fficall ((string :cstring))
          :pointer-void
        "[NSString stringWithCString: #0]" :one-liner t)
      *objc-nil*))

(defun alloc (class-name &key init)
  (let ((obj (c-fficall ((class-name :cstring))
                 :pointer-void
               "[NSClassFromString([NSString stringWithCString: #0]) alloc]"
               :one-liner t)))
    (if init
        (c-fficall ((obj :pointer-void)) :pointer-void "[#0 init]" :one-liner t)
        obj)))

(defun release (obj)
  (c-fficall ((obj :pointer-void)) :void "[#0 release];"))

(defvar *autorelease-pool* nil)
(defvar *autorelease-level* 0)

(defun make-autorelease-pool ()
  (c-fficall () :pointer-void "[[NSAutoreleasePool alloc] init]" :one-liner t))

(defmacro with-autorelease-pool (() &body body)
  `(let ((*autorelease-pool* (or *autorelease-pool* (make-autorelease-pool))))
     (unwind-protect
          (let ((*autorelease-level* (1+ *autorelease-level*)))
            ,@body)
       (when (zerop *autorelease-level*)
         (release *autorelease-pool*)))))

(defun make-callback-function (fun)
  (lambda (&rest args)
    (with-simple-restart (abort "Abort this callback")
      (apply fun args))))

