(in-package :iphone)

(clines
 "#import <CoreGraphics/CoreGraphics.h>"
 "#import <Foundation/Foundation.h>"
 "#import <UIKit/UIKit.h>"
 "#import \"UIButtonCB.h\""
 )

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

(defmacro make-viewcontroller-instance (viewcontroller-class)
  `(let ((viewcontroller (c-fficall ()
				    :pointer-void
				    ,(format nil "[[~a alloc] initWithNibName:nil bundle:nil]" viewcontroller-class)
				    :one-liner t)))
     (ext:set-finalizer viewcontroller #'release)
     viewcontroller))

(defun set-root-viewcontroller (window viewcontroller)
  (c-fficall ((window :pointer-void)
              (viewcontroller :pointer-void))
	     :void
	     "[#0 setRootViewController: #1];"))


(defun show-alert (title &key message (dismiss-label "OK"))
  "Displays a simple alert to notify the user"
  (check-type title string)
  (check-type dismiss-label string)
  (c-fficall (((make-NSString title) :pointer-void)
              ((make-NSString message) :pointer-void)
              ((make-NSString dismiss-label) :pointer-void))
      :void
    "{UIAlertView *alert = [[UIAlertView alloc] 
      initWithTitle: #0
      message: #1
      delegate: nil
      cancelButtonTitle: #2
      otherButtonTitles: nil];
     [alert show];
     [alert release];}"))