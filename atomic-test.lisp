;;; Copyright 2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

;;; Tests the atomic utilities.
;;;

(ace.core:defpackage* #:ace.core.atomic-test
  (:use #:ace.test
        #:common-lisp)
  (:use-alias #:ace.core.atomic))

(in-package :ace.core.atomic-test)

(defstruct foo
  (list nil :type list)
  (word 0 :type atomic:word))

(deftest mexpand ()
  (let ((f (make-foo))
        (a (make-array 16 :initial-element 0)))
    (declare (ignorable f a))
    (expect (macroexpand '(atomic:incf (foo-word f))))
    (expect (macroexpand '(atomic:decf (foo-word f))))
    (expect (macroexpand '(atomic:push t (foo-list f))))
    (expect (macroexpand '(atomic:pop (foo-list f))))
    (expect (macroexpand '(atomic:cas (foo-word f) old new)))
    (expect (macroexpand '(atomic:cas2 (svref a i) o1 o2 n1 n2)))

    (expect (macroexpand '(atomic:update (foo-word f) #'1+)))
    (expect (macroexpand '(atomic:update* (foo-word f) #'1+)))

    (expect (macroexpand '(atomic:peek (foo-word f))))

    (expect (macroexpand '(atomic:poke (foo-word f) 0)))))

(deftest arithmetics ()
  (let ((f (make-foo)))
    (expect (= 0 (atomic:incf (foo-word f) 5)))
    (expect (= 5 (foo-word f)))
    (expect (= 5 (atomic:decf (foo-word f) 3)))
    (expect (= 2 (foo-word f)))

    (expect (atomic:push :foo (foo-list f)))
    (expect (eq :foo (first (foo-list f))))
    (expect (eq :foo (atomic:pop (foo-list f))))
    (expect (null (foo-list f)))

    (expect (= 2 (atomic:cas (foo-word f) 3 5)))
    (expect (= 2 (foo-word f)))

    (expect (= 2 (atomic:cas (foo-word f) 2 5)))
    (expect (= 5 (foo-word f)))

    (expect (= 6 (atomic:update (foo-word f) #'1+)))
    (expect (= 6 (foo-word f)))

    (expect (= 6 (atomic:update* (foo-word f) #'1+)))
    (expect (= 7 (foo-word f)))

    (expect (= 7 (atomic:peek (foo-word f))))
    (expect (= 7 (atomic:poke (foo-word f) 2)))
    (expect (= 2 (atomic:peek (foo-word f))))))

(deftest cas2 ()
  (let ((a (make-array 16 :initial-element 0))
        o1 o2)
    (setf (values o1 o2)
          (atomic:cas2 (svref a 2) 0 0 1 2))
    (expect (= 0 o1 o2))
    (expect (= 1 (svref a 2)))
    (expect (= 2 (svref a 3)))
    (setf (values o1 o2)
          (atomic:cas2 (svref a 2) 0 0 1 2))
    (expect (= 1 o1))
    (expect (= 2 o2))
    (expect (= 1 (svref a 2)))
    (expect (= 2 (svref a 3)))))

(deftest orf-test ()
  (let ((c (cons nil nil)))
    (expect (null (atomic:orf (car c))))
    (expect (null (car c)))
    (expect (null (atomic:orf (car c) nil)))
    (expect (null (car c)))
    (expect (null (atomic:orf (car c) nil nil)))
    (expect (null (car c)))

    (let ((a 0) b d)
      (expect (eql 1 (atomic:orf (car c)
                                 (setf a nil)
                                 (setf b 1)
                                 (setf d 2)
                                 3)))
      (expect (eql 1 (car c)))
      (expect (null a))
      (expect (eql 1 b))
      (expect (null d)))

    (expect (eql 1 (atomic:orf (car c))))
    (expect (eql 1 (car c)))
    (expect (eql 1 (atomic:orf (car c) nil)))
    (expect (eql 1 (car c)))
    (expect (eql 1 (atomic:orf (car c) nil nil)))
    (expect (eql 1 (car c)))

    (let ((foo nil))
      (expect (eql 1 (atomic:orf (car c) nil (setf foo 2) 3)))
      (expect (eql 1 (car c)))
      (expect (null foo)))))
