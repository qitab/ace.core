;;; Copyright 2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

;;;;
;;;; Test for collectors
;;;;

(cl:defpackage #:ace.core.collect-test
  (:use #:common-lisp
        #:ace.core.collect
        #:ace.test))

(cl:in-package #:ace.core.collect-test)

(deftest with-collectors-test ()
  (let ((y-init (list 1 2 3)))
    (with-collectors (x
                      (y add-y :init y-init)
                      (z z :init nil)
                      (u new-u :unique '=))
      (declare (ignore #'z))
      (x 1)
      (x 2)
      (x 3)
      (add-y 4)
      (new-u 1)
      (new-u 2)
      (new-u 1)
      (expect (equal '(1 2 3) x))
      (expect (equal '(1 2 3 4) y))
      (expect (null z))
      (expect (equal '(1 2) u))
      (values x y z))
    ;; WARNING: y-init is intentionally modified.
    (expect (equal '(1 2 3 4) y-init))))

(deftest with-collected-values-test ()
  (let* ((nodes (list 1 2 3))
         (result
          (with-collected-values ((add-node :init nodes :unique '= :type fixnum))
            (add-node 4)
            (add-node 5)
            (add-node 4))))
    (expect (equal nodes result))
    (expect (equal '(1 2 3 4 5) result))))


(deftest with-collected-values-nconc-cons-test ()
  (let* ((nodes (list 1 2 3))
         (result
          (with-collected-values ((add-node :init nodes :type fixnum :mode :cons))
            (add-node (list 4))
            (add-node (list 5 6 7 8))
            (add-node (list 4)))))
    (expect (equal nodes result))
    (expect (equal '(1 2 3 4 5 4) result)))
  (let* ((nodes (list 1 2 3))
         (result
          (with-collected-values ((add-node :init nodes :unique '= :type fixnum :mode :cons))
            (add-node (list 4))
            (add-node (list 5 6 7 8))
            (add-node (list 4)))))
    (expect (equal nodes result))
    (expect (equal '(1 2 3 4 5) result))))

(deftest with-collected-values-nconc-list-test ()
  (let* ((nodes (list 1 2 3))
         (result
          (with-collected-values ((add-node :init nodes :type fixnum :mode :nconc))
            (add-node (list 4))
            (add-node (list 5 6 7 8))
            (add-node (list 4 3))
            (add-node nil)
            (add-node (list 9 4 3)))))
    (expect (equal nodes result))
    (expect (equal '(1 2 3 4 5 6 7 8 4 3 9 4 3) result)))
  (let* ((nodes (list 1 2 3))
         (result
          (with-collected-values ((add-node :init nodes :unique '= :type fixnum :mode :nconc))
            (add-node (list 4))
            (add-node (list 5 6 7 8))
            (add-node (list 4 3))
            (add-node nil)
            (add-node (list 9 4 3)))))
    (expect (equal nodes result))
    (expect (equal '(1 2 3 4 5 6 7 8 9) result))))

(deftest with-collected-values-append-list-test ()
  (let* ((nodes (list 1 2 3))
         (result
          (with-collected-values ((add-node :init nodes :type fixnum :mode :append))
            (add-node (list 4))
            (add-node (list 5 6 7 8))
            (add-node (list 4 3))
            (add-node nil)
            (add-node (list 9 4 3)))))
    (expect (equal nodes result))
    (expect (equal '(1 2 3 4 5 6 7 8 4 3 9 4 3) result)))
  (let* ((nodes (list 1 2 3))
         (result
          (with-collected-values ((add-node :init nodes :unique '= :type fixnum :mode :append))
            (add-node (list 4))
            (add-node (list 5 6 7 8))
            (add-node (list 4 3))
            (add-node nil)
            (add-node (list 9 4 3)))))
    (expect (equal nodes result))
    (expect (equal '(1 2 3 4 5 6 7 8 9) result))))
