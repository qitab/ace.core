;;; Copyright 2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

;;; The package provides symbols for core libraries.
;;;

(defpackage #:ace.core
  (:use #:cl)
  (:export
   ;; Symbols for ACE.CORE.PACKAGE.
   #:defpackage*
   ;; Symbols implemented in ACE.CORE.CHECK, ACE.TEST, ACE.LOG.
   #:check #:dcheck #:expect
   ;; Symbols implemented in ACE.CORE.DEFUN.
   #:defun* #:self #:foldable #:known
   ;; Symbols implemented in ACE.CORE.SWITCH.
   #:switch #:eswitch #:switch*
   ;; Symbols implemented in ACE.CORE.MACRO.
   #:gensym* #:eval-always
   ;; Conditional binding.
   #:clet
   ;; Useful, functions.
   #:strcat #:symcat))

(in-package #:ace.core)
