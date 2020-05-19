;;; Copyright 2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

;;; Test for fast-ops
;;;
;;; This only contains test for some helper functions.
;;; Most of this is tested in fx-test.lisp.
;;;

(defpackage #:ace.core.fast-ops-test
  (:use #:cl #:ace.test #:ace.core.fast-ops)
  (:import-from #:ace.core.fast-ops
                ace.core.fast-ops::%dostack))

(in-package #:ace.core.fast-ops-test)

(defun my+ (&rest args)
  (declare (dynamic-extent args))
  (let ((result 0))
    (declare (fixnum result))
    (%dostack (arg args result)
      (declare (fixnum arg))
      (incf result arg))))

(deftest test-do-stack-list ()
  (expect (= 0 (my+)))
  (expect (= 1 (my+ 1)))
  (expect (= 3 (my+ 1 2)))
  (expect (= 6 (my+ 1 2 3)))
  (expect (= 10 (my+ 1 2 3 4))))
