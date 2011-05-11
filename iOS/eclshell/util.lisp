(defpackage :util
  (:use :cl)
  (:export
   :bind
   :c-fficall
   :join
   :keysym
   :str
   :sym))

(in-package :util)

(defun str (&rest args)
  "Return a concatenated string of all the args. e.g.,

  (str \"abc\" :def 'ghi)
  => \"abcdefghi\"
"
  (with-output-to-string (*standard-output*)
    (dolist (s args) (princ s))))

(defun sym (&rest args)
  "Return a properly interned symbol"
  (intern (string-upcase (apply 'str args))))

(defun keysym (&rest args)
  "Return a properly interned keyword symbol"
  (intern (string-upcase (apply 'str args)) :keyword))

(defun join (seq sep)
  "Concatenate the strings in `seq' delimited by the separator `sep'."
  (format nil (concatenate 'string "~{~a~^" sep "~}") seq))

(defmacro c-fficall (arg-specs return-type body &rest args)
  `(ffi:c-inline ,(map 'list #'first arg-specs)
                 ,(map 'list #'second arg-specs)
                 ,return-type
                 ,body
                 ,@args))

(defmacro bind (clauses &body body)
  "This macro combines the behaviour of the forms `let*',
`destructuring-bind', and `multiple-value-bind', permitting the
following style of binding form:

  (bind (((:values m n) (values 10 20))
         ((a b _c &key (d 10)) '(1 2 3))
         (x 5))
    (+ x a b d m n))
  => 48

Note in the destructuring form (a b _c &key (d 10)), _c is a short form
for declaring it as ignorable.

This is a more limited and lightweight implementation of some ideas from
metabang-bind (http://common-lisp.net/project/metabang-bind/)."
  (labels
      ((parse-arglist (args)
         (loop
            for arg in args
            collect arg into args
            when (and (symbolp arg) (eq (aref (symbol-name arg) 0) #\_))
            collect arg into ignorables
            finally (return (values args ignorables))))
       (cons-form (form args clauses body)
         (multiple-value-bind (arglist ignorables)
             (parse-arglist args)
           `(,form ,arglist
                   ,@(cdar clauses)
                   ,@(when ignorables `((declare ,(list* 'ignore ignorables))))
                   (bind ,(cdr clauses) ,@body)))))
    (cond
      ((null clauses) `(progn ,@body))
      ((listp (caar clauses))
       (cond
         ((eq (caaar clauses) :values)
          (cons-form 'multiple-value-bind (cdaar clauses) clauses body))
         ((eq (caaar clauses) :slots)
          `(with-slots ,(cdaar clauses) ,@(cdar clauses)
             (bind ,(cdr clauses) ,@body)))
         (t
          (cons-form 'destructuring-bind (caar clauses) clauses body))))
      (t
       `(let (,(car clauses))
          (bind ,(cdr clauses) ,@body))))))
