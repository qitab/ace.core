;;; Test for fast-ops
;;;
;;; This only contains test for some helper functions.
;;; Most of this is tested in fx-test.lisp.
;;;

(defpackage #:google.core.fast-ops-test
  (:use #:cl #:google.test #:google.core.fast-ops)
  (:import-from #:google.core.fast-ops
                google.core.fast-ops::%dostack))

(in-package #:google.core.fast-ops-test)

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
