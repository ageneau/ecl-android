;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;; *************************************************************************
;; Name:          cl-android.asd
;; Purpose:       SL4A lisp bridge
;; Programmer:    Sylvain Ageneau
;; Date Started:  Apr 2011
;; *************************************************************************

(in-package #:cl-user)
(defpackage #:cl-android-system (:use #:asdf #:cl))
(in-package #:cl-android-system)


(defsystem :cl-android
  :name "cl-android"
  :author "Sylvain Ageneau"
  :version "1.0"
  :maintainer "Sylvain Ageneau"
  :licence "BSD-style"
  :description "SL4A lisp bridge"
  :depends-on (:usocket :cl-json)
  :components
  ((:file "package")
   (:file "android" :depends-on ("package"))
   ))
