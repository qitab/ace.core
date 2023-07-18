;;; Copyright 2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

;;; A universal WITH macro.
;;;

(defpackage #:ace.core.with
  (:use #:ace.core
        #:ace.core.macro
        #:cl)
  (:export
   #:with
   #:defcleanup))

(in-package #:ace.core.with)

(defgeneric cleanup (init-function-symbol &rest variables)
  (:documentation
   "For a given INIT-FUNCTION-SYBMOL form and VARIABLES' symbols
generate the corresponding cleanup form."))

(defun %cleanup-form (init-form var-symbols)
  (expect (typep init-form '(cons symbol)))
  (if (consp var-symbols)
      (apply #'cleanup (car init-form) var-symbols)
      (cleanup (car init-form) var-symbols)))

(defmacro defcleanup (init-function-symbol (&rest variables) &body body)
  "For a given INIT-FUNCTION-SYMBOL and VARIABLES binding allocated resources,
generate a cleanup form function. The DEFCLEANUP macro accepts
a list of symbols for the VARIABLES or a '&REST var-symbols' for an undefined
number of VARIABLES. E.g.:

 (defcleanup init (&rest vars)
   `(release ,@vars))

is a cleanup definition for an INIT function that returns a indefinite number
of values. Only the first of the VARS is used to determine if RELEASE should
be executed. The remaining VARS act as parameters.
"
  (with-gensyms (sym vars)
    `(eval-always
      (defmethod cleanup ((,sym (eql ',init-function-symbol)) &rest ,vars)
        (destructuring-bind ,variables ,vars
          `(when ,(car ,vars)
             ,,@body))))))

(defmacro with ((&rest bindings) &body body)
  "With evaluates BINDINGS successively binding each variable
to the corresponding init-form value. If the value is NIL,
the evaluation of successive bindings and the BODY aborts.

With provides an UNWIND-PROTECT environment where the cleanup
is populated with forms generated using the WITH:CLEANUP generic function.
The cleanup forms are evaluated in reverse order to the BINDINGS
and are evaluated only if the corresponding (first) value was not NIL.

The bindings can have a multi-value-form. E.g.:
  (with (((v0 v1) (reserve-resource :foo)))
    #| the body is evaluated if V0 is not NIL. |#)
"
  `(let ,(lconc ((v) bindings) (if (consp v) v `(,v)))
     (unwind-protect
          (when (and ,@(lmap ((v b) bindings)
                         `(setf ,(if (consp v) `(values ,@v) v) ,b)))
            (locally ,@body))
       :cleanup
       ,@(lmap ((vars init-form) (reverse bindings))
               (%cleanup-form init-form vars)))))
