;;; Copyright 2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

;;; Utilities for dealing with DYNAMIC-EXTENT declarations.
;;;

(defpackage #:ace.core.dx
  (:use :cl :ace.core #:ace.core.macro)
  (:shadow
   cl:flet cl:labels cl:let cl:let*)
  (:export
   #:flet
   #:flet*
   #:labels
   #:let
   #:let*))

(in-package #:ace.core.dx)

(defun %flet-form (let bindings body)
  (cl:let* ((inlines
             (loop :for b. :on bindings
                   :nconc (and (eq 'inline (car b.))
                               (list (caadr b.)))))
            (bindings (remove 'inline bindings))
            (dx (lmap ((name) bindings) `(function ,name))))
    `(,let ,bindings
       (declare (dynamic-extent ,@dx)
                ,@(and inlines `((inline ,@inlines))))
       ,@body)))

(defmacro flet ((&rest bindings) &body body)
  "Declare function BINDINGS as dynamic-extent within BODY scope."
  (%flet-form 'cl:flet bindings body))

(defmacro labels ((&rest bindings) &body body)
  "Declare function BINDINGS as dynamic-extent within BODY scope."
  (%flet-form 'cl:labels bindings body))

(defmacro flet* ((&rest bindings) &body body)
  "Declare function BINDINGS as dynamic-extent within BODY scope.
Subsequent functions are defined in a lexical scope of previous function
definitions."
  (cl:let ((rest (cdr bindings)))
    (when (eq 'inline (first bindings))
      (pop rest))
    (when rest
      (setf body `((flet* ,rest ,@body))))
    `(flet ,(ldiff bindings rest) ,@body)))

(defmacro let ((&rest bindings) &body body)
  "Declare variable BINDINGS as dynamic-extent within BODY scope."
  (cl:let ((dx (lconc (b bindings) (and (consp b) (list (car b))))))
    `(cl:let ,bindings
       ,@(when dx `((declare (dynamic-extent ,@dx))))
       ,@body)))

(defmacro let* ((&rest bindings) &body body)
  "Declare variable BINDINGS as dynamic-extent within BODY scope."
  (cl:let ((dx (lconc (b bindings) (and (consp b) (list (car b))))))
    `(cl:let* ,bindings
       ,@(when dx `((declare (dynamic-extent ,@dx))))
       ,@body)))

