(in-package :iphone)

(clines
 "#import <CoreGraphics/CoreGraphics.h>"
 "#import <Foundation/Foundation.h>"
 "#import <UIKit/UIKit.h>"
 "#import <GLKit/GLKit.h>"
 "#import \"LispGLKViewController.h\""
 )


(defun set-setupgl-handler (glkviewcontroller fun)
  (when fun
    (c-fficall ((glkviewcontroller :pointer-void)
                (fun :object))
	       :void
	       "[#0 setSetupGLHandler: #1];")))

(defun set-teardowngl-handler (glkviewcontroller fun)
  (when fun
    (c-fficall ((glkviewcontroller :pointer-void)
                (fun :object))
	       :void
	       "[#0 setTearDownGLHandler: #1];")))

(defun set-update-handler (glkviewcontroller fun)
  (when fun
    (c-fficall ((glkviewcontroller :pointer-void)
                (fun :object))
	       :void
	       "[#0 setUpdateHandler: #1];")))

(defun set-redraw-handler (glkviewcontroller fun)
  (when fun
    (c-fficall ((glkviewcontroller :pointer-void)
                (fun :object))
	       :void
	       "[#0 setRedrawHandler: #1];")))

(defun make-glkviewcontroller (&key on-setupgl on-teardowngl on-update on-redraw)
  (let ((viewcontroller (make-viewcontroller-instance "LispGLKViewController")))
    (set-setupgl-handler viewcontroller on-setupgl)
    (set-teardowngl-handler viewcontroller on-teardowngl)
    (set-update-handler viewcontroller on-update)
    (set-redraw-handler viewcontroller on-redraw)
    viewcontroller))
