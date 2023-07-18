;;; Copyright 2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

;;; Tests for assertion checks.
;;;
;;; cllint: disable=invalid-assert

(defpackage #:ace.core.check-test
  (:use #:ace.core.check
        #:ace.core.defun
        #:ace.test
        #:common-lisp)
  (:import-from #:ace.test.runner #:*failed-conditions*))

(in-package #:ace.core.check-test)

(deftype foo () '(integer -13 23))

;; Set type info for FOO including the formatter.
(ace.core.macro:eval-always
 (setf (get 'foo 'ace.core.type:info)
       (ace.core.type:make-info :name 'foo :format "#x~X")))

(defun* tt (x &key y)
  (declare (self (foo &key fixnum) list))
  (when (plusp y) (list x y)))

(deftest test-check ()
  (setf *failed-conditions* nil)
  (let* ((x 18)
         (z 0)
         (error (signals error
                  (check (tt x :y z))))
         (error2 (signals error
                   (check (tt x :y z) "CHECK ~A!" :failed)))
         (error3 (signals error
                   (check (not (/= x z)))))
         (error4 (signals error
                   (check (and x (not x) z))))
         (actual (princ-to-string error))
         (actual2 (princ-to-string error2))
         (actual3 (princ-to-string error3))
         (actual4 (princ-to-string error4)))
    (expect error)
    (expect
     (string=
      "[check-test:36] Failed check (TT X :Y Z) with X = #x12, Z = 0."
      actual))

    (expect error2)
    (expect (string= "[check-test:38] CHECK FAILED!" actual2))

    (expect error3)
    (expect
     (string=
      "[check-test:40] Failed check (NOT #1=(/= X Z)) with X = 18, Z = 0, #1# = T."
      actual3))

    (expect error4)
    (expect
     (string=
      "[check-test:42] Failed check (AND X #1=(NOT X) Z) with X = 18, #1# = NIL."
      actual4))))

(deftest test-expect ()
  (setf *failed-conditions* nil)
  (let* ((x 18)
         (z 0))
    (assert (null *failed-conditions*))
    (expect (tt x :y z))
    (assert (= 1 (length *failed-conditions*)))
    (expect (tt x :y z) "CHECK ~A!" :failed)
    (assert (= 2 (length *failed-conditions*)))
    ;; Clear.
    (setf *failed-conditions* nil)))

(deftest test-not-expect ()
  (let ((x 2)
        (y 3)
        (nay nil))
    (expect (null nil))
    (expect (null nay))
    (expect (not (null x)))
    (expect (not (= x y x y x y x y  x y x y x y x y  x y x y x y x y )))
    (expect (null (= x 5)))
    (expect (null (= 5 y)))
    (expect (null (= 5 6)))))

(deftest test-and-expect ()
  (let ((x 2)
        (y 3)
        (nay nil))
    (expect (and))
    (expect (= 42 (expect (and 42))))
    (expect (= 3 (expect (and x (not nay) y))))
    (expect (eq t (expect (and x y (null nay)))))
    (expect (= 2 (expect (and x))))
    (expect (and x y x y x y x y  x y x y x y x y  x y x y x y x y ))
    (expect (= 5 (expect (and x 5))))
    (expect (= 3 (expect (and 5 y))))
    (expect (= 6 (expect (and 5 6))))

    (expect-error (check (and nil)))
    (expect-error (check (and nay)))
    (expect-error (check (and x nay y)))
    (expect-error (check (and x y nay)))
    (expect-error (check (and nay x)))
    (expect-error (check (and x y x y x y x y  x y x y x y x y nay)))
    (expect-error (check (and x nil 5)))
    (expect-error (check (and 5 nay y)))
    (expect-error (check (and 5 nay 6)))
    t))
