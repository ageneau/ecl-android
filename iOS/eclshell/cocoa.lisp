;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; COCOA

(defpackage :cocoa
  (:use :cl :ffi :util :eclffi)
  (:export
   :show-alert))

(in-package :cocoa)

(clines
 "#import <CoreGraphics/CoreGraphics.h>"
 "#import <Foundation/Foundation.h>"
 "#import <UIKit/UIKit.h>"
 "#import \"ecl_boot.h\""
 )

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
