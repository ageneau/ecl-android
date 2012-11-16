(defpackage :eclffi
  (:use :cl :ffi :util)
  (:export
   :add-subview
   :alloc
   :color-argb
   :key-window
   :load-font
   :make-button
   :make-label
   :make-NSString
   :ns-log
   :redraw
   :release
   :set-alpha
   :set-background-color
   :set-bounds
   :set-enabled
   :set-font
   :set-frame
   :set-frame
   :set-image
   :set-number-of-lines
   :set-text
   :set-text
   :set-text-color
   :set-title
   :set-title-color
   :system-font
   :with-autorelease-pool))

(in-package :eclffi)

(clines
 "#import <CoreGraphics/CoreGraphics.h>"
 "#import <Foundation/Foundation.h>"
 "#import <UIKit/UIKit.h>"
 "#import \"Classes/UIButtonCB.h\""
 "#import \"ecl_boot.h\""
 )

#+ffi
(eval-when (:compile-toplevel :load-toplevel :execute)
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
  )

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

(defun key-window ()
  (c-fficall () :pointer-void
    "[[UIApplication sharedApplication] keyWindow]"
    :one-liner t))

(defun redraw (view)
  (c-fficall ((view :pointer-void)) :void
    "[#0 performSelectorOnMainThread: NSSelectorFromString([NSString stringWithCString: \"setNeedsDisplay\"])
	 withObject: nil
	 waitUntilDone: YES];"))

(defun add-subview (view subview)
  (c-fficall ((view :pointer-void)
              (subview :pointer-void))
      :void
    "[((id) #0) addSubview: #1];"))

(defun color-argb (a r g b)
  (c-fficall (((float r) :float)
              ((float g) :float)
              ((float b) :float)
              ((float a) :float))
      :pointer-void
    "[[UIColor alloc] initWithRed: #0 green: #1 blue: #2 alpha: #3]"
    :one-liner t))

(defun set-text (label text)
  (c-fficall ((label :pointer-void)
              (text :cstring))
      :void
    "[#0 setText: [NSString stringWithCString: #1]];"))

(defun set-frame (view frame)
  (bind (((x y width height) frame))
    (c-fficall ((view :pointer-void)
                (x :float)
                (y :float)
                (width :float)
                (height :float))
        :void
      "[((id) #0) setFrame: CGRectMake(#1,#2,#3,#4)];")))

(defun set-bounds (view frame)
  (bind (((x y width height) frame))
    (c-fficall ((view :pointer-void)
                (x :float)
                (y :float)
                (width :float)
                (height :float))
        :void
      "[((id) #0) setBounds: CGRectMake(#1,#2,#3,#4)];")))

(defun set-background-color (view color)
  (c-fficall ((view :pointer-void)
              (color :pointer-void))
      :void
    "[#0 setBackgroundColor: #1];"))

(defun set-alpha (view alpha)
  (c-fficall ((view :pointer-void)
              (alpha :float))
      :void
    "[#0 setAlpha: #1];"))

(defun init-view (view &key frame bounds background-color alpha &allow-other-keys)
  (when frame (set-frame view frame))
  (when bounds (set-bounds view bounds))
  (set-background-color view (or background-color (color-argb 1 1 1 1)))
  (when alpha (set-alpha view alpha))
  view)

(defmacro make-view-instance (view-class init-view-args)
  `(let ((view (apply 'init-view
                      (c-fficall ()
                          :pointer-void
                        ,(format nil "[[~a alloc] initWithFrame: CGRectZero]" view-class)
                        :one-liner t)
                      ,init-view-args)))
     (ext:set-finalizer view #'release)
     view))

(def-ffi-enum uicontrol-state
    ((:normal "UIControlStateNormal")
     (:highlighted "UIControlStateHighlighted")
     (:disabled "UIControlStateDisabled")
     (:selected "UIControlStateSelected")
     (:application "UIControlStateApplication")
     (:reserved "UIControlStateReserved")))

(defun set-image (view image &key (state :normal))
  (c-fficall ((view :pointer-void)
              (image :pointer-void)
              ((keyword->uicontrol-state state) :int))
      :void
    "[#0 setImage: #1 forState: #2];"))

(defun set-title (view title &key (state :normal))
  (c-fficall ((view :pointer-void)
              ((make-NSString title) :pointer-void)
              ((keyword->uicontrol-state state) :int))
      :void
    "[#0 setTitle: #1 forState: #2];"))

(defun set-title-color (view color &key (state :normal))
  (c-fficall ((view :pointer-void)
              (color :pointer-void)
              ((keyword->uicontrol-state state) :int))
      :void
    "[#0 setTitleColor: #1 forState: #2];"))

(defun set-enabled (view bool)
  (c-fficall ((view :pointer-void) ((if bool 1 0) :int))
      :void
    "[#0 setEnabled: #1];"))

(defun set-click-handler (view fun)
  (when fun
    (c-fficall ((view :pointer-void)
                ((make-callback-function fun) :object))
        :void
      "[#0 setClickHandler: #1];")))

(defun make-button (&rest init-view-args &key title title-color icon
                    on-click (enabled t) &allow-other-keys)
  (let ((view (make-view-instance "UIButtonCB" init-view-args)))
    (set-click-handler view on-click)
    (when icon (set-image view icon))
    (when title (set-title view title))
    (when title-color (set-title-color view title-color))
    (set-enabled view enabled)
    view))

(defun set-number-of-lines (view num)
  (c-fficall ((view :pointer-void)
              (num :int))
      :void
    "[#0 setNumberOfLines: #1];"))

(defun system-font (size)
  (c-fficall ((size :float))
      :pointer-void
    "[UIFont systemFontOfSize: #0]"
    :one-liner t))

(defun load-font (family size)
  (c-fficall (((make-NSString family) :pointer-void)
              (size :float))
      :pointer-void
    "[UIFont fontWithName: #0 size: #1]"
    :one-liner t))

(defun set-font (label font)
  (c-fficall ((label :pointer-void)
              (font :pointer-void))
      :void
    "[#0 setFont: #1];"))

(defun set-text-color (label color)
  (c-fficall ((label :pointer-void)
              (color :pointer-void))
      :void
    "[#0 setTextColor: #1];"))

(defun make-label (text &rest init-view-args &key font text-color
                   number-of-lines &allow-other-keys)
  (let ((view (make-view-instance "UILabel" init-view-args)))
    (set-text view text)
    (set-font view (or font (system-font 24.0)))
    (set-text-color view (or text-color (color-argb 1 0 0 0)))
    (when number-of-lines (set-number-of-lines view number-of-lines))
    view))
