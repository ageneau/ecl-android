;;;
;;;
;;; Copyright (c) 2009, Jean-Claude Beaudoin
;;; All rights reserved by the author.
;;;
;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without
;;; restriction, including without limitation the rights to use, copy,
;;; modify, merge, publish, distribute, sublicense, and/or sell copies
;;; of the Software, and to permit persons to whom the Software is
;;; furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.
;;;
;;;

(defvar cl-user::*jre-home* "/usr/lib/jvm/java-1.6.0-openjdk/jre")
(defvar cl-user::*jvm-path* "/usr/lib/jvm/java-1.6.0-openjdk/jre/lib/amd64/server/libjvm.so")
(defvar cl-user::*jvm-options* '("-Djava.class.path=/home/sylvain/src/LISP/ecl-android/android/lisp-packages/cl+j/cl_j.jar"))


(require :asdf)

(asdf:initialize-source-registry
 '(:source-registry
   (:tree "/home/sylvain/src/LISP/ecl-android/android/lisp-packages/")
   :inherit-configuration))
(require :cl+j)

(in-package :cl-user)


(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find (find-package :cl+j) (package-use-list *package*))
    (use-package :cl+j)))

(java-init) ;; if not already done.


(trace jni::FindClass)
(trace cl+j::fetch-java-class-from-jvm)
(trace cl+j::fetch-java-class-in-context)
(trace cl+j::find-java-class-proxy)
(trace cl+j::find-java-class-proxy-from-real)
(trace cl+j::find-java-class)
(trace cl+j::jfield-static)
(trace cl+j::jfield-instance)
(trace cl+j::jmethod-instance)
(trace cl+j::jmethod-static)
(trace cl+j::find-java-static-method)
(trace cl+j::register-simple-type-name)
(trace cl+j::split-full-type-name)


(in-java-context "hello-swing")

(java-import "javax.swing.*")

;(defvar a-frame)
;(defvar a-label)

(defun hello-swing ()
  (with-java-context "hello-swing"
    (let (a-frame a-label a-font)
      (format t "Decorated ~A ~%:" (#_JFrame.isDefaultLookAndFeelDecorated))
      (setq a-frame (jnew "JFrame" (jstring "Hello brave Swing World")))
      (#_setDefaultCloseOperation a-frame #?WindowConstants.DISPOSE_ON_CLOSE)
      (setq a-label (jnew "JLabel" (jstring "Hello brave Swing World from Common Lisp!")))
      (setq a-font (jnew "java.awt.Font" (jstr "Dialog") #?java.awt.Font.BOLD 20))
      (#_setFont a-label a-font)
      (#_add (#_getContentPane a-frame) a-label)
      (#_pack a-frame)
      (#_setVisible a-frame jtrue)
      )
    )
  )

(hello-swing)


