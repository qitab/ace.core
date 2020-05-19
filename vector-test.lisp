;;; Copyright 2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

;;;;
;;;; Tests for the ace.core.vector package.
;;;;


(ace.core.package:defpackage* #:ace.core.vector-test
  (:use #:common-lisp
        #:ace.test)
  (:use-alias #:ace.core.vector))

(in-package #:ace.core.vector-test)

(deftest test-make-empty ()
  (expect (zerop (length (vector:make-empty 'fixnum))))
  (expect (= 1 (array-rank (vector:make-empty 'fixnum))))
  (expect (not (array-has-fill-pointer-p (vector:make-empty 'fixnum))))

  #+(or sbcl ccl)
  (expect (eq (vector:make-empty 'fixnum)
              (vector:make-empty 'fixnum)))

  #+(or sbcl ccl)
  (expect (eq (vector:make-empty '(or integer symbol character string))
              (vector:make-empty 't))))

(declaim (inline %test-prefetch))
(defun %test-prefetch (x)
  (declare (fixnum x))
  (let ((a (make-array 128 :initial-element 0)))
    ;; Not a real-use case. As DougK explains:
    ;;  "Prefetch followed immediately by a read to the same
    ;;   location is strictly worse than just a read alone."
    (vector:prefetch a (logand x 15))
    (the fixnum (svref a (logand x 15)))))

(deftest test-prefetch ()
  (expect (= 0 (%test-prefetch 10))))
