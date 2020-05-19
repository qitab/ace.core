;;; Copyright 2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

;;;;
;;;; Tests for the ace.core.list package.
;;;;

(ace.core.package:defpackage* #:ace.core.list-test
  (:use #:common-lisp
        #:ace.test)
  (:use-alias #:ace.core.list))

(in-package #:ace.core.list-test)

(deftest test-list-length= ()
  (expect (list:length= '() '()))
  (expect (list:length= '(a) '(1)))
  (expect (list:length= '(a b) '(1 2)))
  (expect (list:length= '(a b c) '(1 2 3)))

  (expect (not (list:length= '(a) '())))
  (expect (not (list:length= '(a b) '(1))))
  (expect (not (list:length= '(a b c) '(1 2))))
  (expect (not (list:length= '(a b c d) '(1 2 3))))

  (expect (not (list:length= '() '(a))))
  (expect (not (list:length= '(1) '(a b))))
  (expect (not (list:length= '(1 2) '(a b c))))
  (expect (not (list:length= '(1 2 3) '(a b c d)))))


(deftest list-elements-eq-test ()
  (declare (notinline list:elements-eq))
  (expect (list:elements-eq () ()))
  (expect (not (list:elements-eq '() '(1))))
  (expect (not (list:elements-eq '(1) '())))
  (expect (list:elements-eq (list 1) (list 1)))
  (expect (not (list:elements-eq (list 1) (list 2))))
  (expect (not (list:elements-eq (list 1) (list 1 2))))
  (expect (list:elements-eq (list 1 2) (list 1 2)))
  (expect (not (list:elements-eq (list 1 2) (list 1))))
  (expect (not (list:elements-eq (list 1 2) (list 1 3))))
  (expect (list:elements-eq (list 1 2 3) (list 1 2 3))))

(deftest match-test ()
  (declare (notinline list:match))
  (expect (list:match () () :test #'=))
  (expect (not (list:match '() '(1) :test #'=)))
  (expect (not (list:match '(1) '() :test #'=)))
  (expect (list:match (list 1) (list 1) :test #'=))
  (expect (list:match (list 1) (list 1) :test #'= :key #'1+))
  (expect (list:match (list 1) (list 1) :key #'oddp))
  (expect (list:match (list 1) (list 3) :key #'oddp))
  (expect (list:match (list 1 2) (list 3 4) :key #'oddp))
  (expect (not (list:match (list 1 2 3) (list 3 4 6) :key #'oddp)))
  (expect (not (list:match (list 1) (list 2) :test #'=)))
  (expect (not (list:match (list 1) (list 1 2) :test #'=)))
  (expect (list:match (list 1 2) (list 1 2) :test #'=))
  (expect (not (list:match (list 1 2) (list 1) :test #'=)))
  (expect (not (list:match (list 1 2) (list 1 3) :test #'=)))
  (expect (list:match (list 1 2 3) (list 1 2 3) :test #'=))

  (multiple-value-bind (match l1 l2)
      (list:match (list 1 2 3) (list 3 4 6) :key #'oddp)
    (expect (not match))
    (expect (equal l1 '(3)))
    (expect (equal l2 '(6)))))

(deftest partition2-test ()
  (flet ((test (expected-true expected-false predicate list)
           (multiple-value-bind (actual-true actual-false)
               (list:PARTITION list predicate)
             (expect (equal expected-true actual-true))
             (expect (equal expected-false actual-false)))
           (multiple-value-bind (actual-true actual-false)
               (list:NPARTITION (copy-list list) predicate)
             (expect (equal expected-true actual-true))
             (expect (equal expected-false actual-false)))

           (locally (declare (notinline list:partition list:npartition))
             (multiple-value-bind (actual-true actual-false)
                 (list:PARTITION list predicate)
               (expect (equal expected-true actual-true))
               (expect (equal expected-false actual-false)))
             (multiple-value-bind (actual-true actual-false)
                 (list:NPARTITION (copy-list list) predicate)
               (expect (equal expected-true actual-true))
               (expect (equal expected-false actual-false))))))
    (test '(1 3 5) '(2 4) #'oddp '(1 2 3 4 5))
    (test '(1 3 3 5 5 7 5 3) '(2 2 4 4 6 4 2) #'oddp '(1 2 3 2 3 4 4 5 5 6 4 2 7 5 3))
    (test '(1 11 33 3 3 5 5 7 5 3) '(2 2 4 4 6 4 2) #'oddp '(1 11 33 2 3 2 3 4 4 5 5 6 4 2 7 5 3))
    (test '(3 3 5 55 57) '(2 22 24 2 4) #'oddp '(2 22 24 3 2 3 4 5 55 57))
    (test '(1) '(2) #'oddp '(1 2))
    (test '(2) '(1) #'evenp '(1 2))
    (test '(1 1) '(2 2) #'oddp '(1 2 2 1))
    (test '(1 3 5) '() #'oddp '(1 3 5))
    (test '() '(2 4) #'oddp '(2 4))
    (test '(2 4) '() #'evenp '(2 4))
    (test '() '(2) #'oddp '(2))
    (test '(4) '() #'evenp '(4))
    (test '() '() #'oddp '())))

(deftest partition-test ()
  (expect (equal '() (list:partition '())))
  (expect (equal '() (list:npartition '())))
  (expect (equal '(1 2 3) (list:partition '(1 2 3))))
  (expect (equal '(1 2 3) (list:npartition '(1 2 3))))
  (locally (declare (notinline list:partition list:npartition))
    (expect (equal '() (list:partition '())))
    (expect (equal '() (list:npartition '())))
    (expect (equal '(1 2 3) (list:partition '(1 2 3))))
    (expect (equal '(1 2 3) (list:npartition '(1 2 3)))))

  (flet ((test (expected list)
           (multiple-value-bind (strings odd even)
               (list:PARTITION list #'stringp #'oddp)
             (expect (equal (first expected) strings))
             (expect (equal (second expected) odd))
             (expect (equal (third expected) even)))
           (multiple-value-bind (strings odd even)
               (list:NPARTITION (copy-list list) #'stringp #'oddp)
             (expect (equal (first expected) strings))
             (expect (equal (second expected) odd))
             (expect (equal (third expected) even)))

           (locally (declare (notinline list:partition list:npartition))
             (multiple-value-bind (strings odd even)
                 (list:PARTITION list #'stringp #'oddp)
               (expect (equal (first expected) strings))
               (expect (equal (second expected) odd))
               (expect (equal (third expected) even)))
             (multiple-value-bind (strings odd even)
                 (list:NPARTITION (copy-list list) #'stringp #'oddp)
               (expect (equal (first expected) strings))
               (expect (equal (second expected) odd))
               (expect (equal (third expected) even))))))

    (test '(("A" "B" "C") (1 3 5) (2 4))
          '("A" 1 2 "B" 3 4 5 "C"))
    (test '(("A" "B" "C") (1 3 3 5 5 7 5 3) (2 2 4 4 6 4 2))
          '(1 2 3 "A" "B" 2 3 4 4 5 5 6 4 2 7 5 "C" 3))
    (test '(("A" "A" "A" "C" "C" "C") (1 11 33 3 3 5 5 7 5 3) (2 2 4 4 6 4 2))
          '("A" "A" "A" 1 11 33 2 3 2 3 4 4 "C" 5 "C" "C" 5 6 4 2 7 5 3))
    (test '(("A" "A" "A" "C" "C" "C") (3 3 5 55 57) (2 22 24 2 4))
          '(2 "A" "A" 22 "A" 24 3 2 3 4 5 55 57 "C" "C" "C"))
    (test '(("A") (1) (2))
          '("A" 1 2))
    (test '(("A" "B") (1 1) (2 2))
          '("A" 1 2 2 1 "B"))
    (test '(("A" "B") (1 1) (2 2))
          '(1 "A" 2 2 "B" 1))
    (test '(("A" "B") (1 1) (2 2))
          '(1 2 "A" "B" 2 1))
    (test '(() (1 3 5) ())
          '(1 3 5))
    (test '(() () (2 4))
          '(2 4))
    (test '(("A" "B" "C") () ())
          '("A" "B" "C"))
    (test '(("A" "B") () ())
          '("A" "B"))
    (test '(("A") () ())
          '("A"))
    (test '(() () (2))
          '(2))
    (test '(() () ())
          '())))

(deftest car-or-atom-test ()
  (expect (eq 'foo (list:car-or-atom 'foo)))
  (expect (eq nil (list:car-or-atom nil)))
  (expect (= 1 (list:car-or-atom '(1 2 3)))))

(deftest appendf-test ()
  (let ((a (list 1 2 3)))
    (expect (equal '(1 2 3 4 5 6) (list:appendf a '(4 5 6))))
    (expect (equal '(1 2 3 4 5 6) a)))
  (let ((a (list 1 2 3)))
    (expect (equal '(2 3 4 5 6) (list:appendf (cdr a) '(4 5 6))))
    (expect (equal '(1 2 3 4 5 6) a)))
  (let ((c 0)
        (a (list 1 2 3)))
    (flet ((id (b) (incf c) b))
      (expect (equal '(2 3 4 5 6) (list:appendf (cdr (id a)) '(4 5 6))))
      (expect (equal '(1 2 3 4 5 6) a))
      (expect (= c 1)))))

(deftest push*-test ()
  (let ((a (list 1 2 3)))
    (expect (equal '(4 5 6 1 2 3) (list:push* '(4 5 6) a)))
    (expect (equal '(4 5 6 1 2 3) a)))
  (let ((a (list 1 2 3)))
    (expect (equal '(4 5 6 2 3) (list:push* '(4 5 6) (cdr a))))
    (expect (equal '(1 4 5 6 2 3) a)))
  (let ((c 0)
        (a (list 1 2 3)))
    (flet ((id (b) (incf c) b))
      (expect (equal '(4 5 6 2 3) (list:push* '(4 5 6) (cdr (id a)))))
      (expect (equal '(1 4 5 6 2 3) a))
      (expect (= c 1)))))

(deftest mappend-test ()
  (expect (equal '(0 1 2 3 4 5 6)
                 (list:mappend (lambda (x) (list (1+ x))) '(-1 0 1 2 3 4 5))))
  (expect (equal '(0 2 4 6 8 10)
                 (list:mappend (lambda (x y) (list (+ x y))) '(0 1 2 3 4 5) '(0 1 2 3 4 5)))))

(deftest intersectp-test ()
  (expect (eql 4 (list:intersectp '(1 2 3 4) '(5 6 7 8 9 4) :test #'eq)))
  (expect (not (list:intersectp '(1 2 3 4) '(5 6 7 8 9))))

  (expect (eql 4 (list:intersectp '(1 2 3 4) '(5 6 7 8 9 4) :key #'1+)))
  (expect (not (list:intersectp '(1 2 3 4) '(5 6 7 8 9) :key #'1+)))

  (expect (list:intersectp '("1" "2" "3" "4") '("5" "6" "7" "8" "9" "4") :test #'string=))
  (expect (not (list:intersectp '("1" "2" "3" "4") '("5" "6" "7" "8" "9") :test #'string=))))


(defun eqq (x y) (eq x y))
(defun meq (x y) (not (eq x y)))
(defun oggp (x) (oddp x))
(defun efenp (x) (evenp x))
(defun itendity (x) x)
(defun plus1 (x) (1+ x))
(declaim (ftype (function () (values t &optional)) nothing))
(defun nothing () nil)

(deftest find-test-1 ()
  (expect (eql 4 (list:find 4 '(1 2 3 4 5))))
  (expect (eql NIL (list:find 0 '(1 2 3 4 5))))

  (expect (eql 4 (list:find 4 '(1 2 3 4 5) :test #'=)))
  (expect (eql NIL (list:find 0 '(1 2 3 4 5) :test #'=)))

  (expect (eql 3 (list:find 4 '(1 2 3 4 5) :test #'= :key #'1+ :test #'/=)))
  (expect (eql NIL (list:find 0 '(0 1 2 3 4 5) :test #'= :key #'1+)))

  (expect (eql 3 (list:find 4 '(1 2 3 4 5) :test #'= :key #'1+ :test-not (nothing))))
  (expect (eql NIL (list:find 0 '(0 1 2 3 4 5) :test #'= :key #'1+ :test-not (nothing))))

  (expect (eql 0 (list:find 4 '(0 1 2 3 4 5) :test #'/= :key #'1+ :test-not (nothing))))
  (expect (eql 0 (list:find 0 '(0 1 2 3 4 5) :test #'/= :key #'1+ :test-not (nothing))))

  (expect (eql 0 (list:find 4 '(0 1 2 3 4 5) :test #'/= :test-not (nothing))))
  (expect (eql 1 (list:find 0 '(0 1 2 3 4 5) :test #'/= :test-not (nothing))))

  (expect (eql 0 (list:find 4 '(0 1 2 3 4 5) :test-not #'= :test (nothing))))
  (expect (eql 1 (list:find 0 '(0 1 2 3 4 5) :test-not #'= :test (nothing))))

  (expect (eql 0 (list:find 4 '(0 1 2 3 4 5) :key (nothing) :test #'/= :test-not (nothing))))
  (expect (eql 1 (list:find 0 '(0 1 2 3 4 5) :key (nothing) :test #'/= :test-not (nothing))))

  (expect (eql 0 (list:find 4 '(0 1 2 3 4 5) :key (nothing) :test-not #'= :test (nothing))))
  (expect (eql 1 (list:find 0 '(0 1 2 3 4 5) :key (nothing) :test-not #'= :test (nothing))))

  ;; Does not adhere to CL:FIND contract. START must not be NIL.
  ;; (expect (eql 0 (list:find 4 '(0 1 2 3 4 5) :start (nothing) :test #'/= :test-not (nothing))))
  ;; (expect (eql 1 (list:find 0 '(0 1 2 3 4 5) :start (nothing) :test #'/= :test-not (nothing))))
  ;; (expect (eql 0 (list:find 4 '(0 1 2 3 4 5) :start (nothing) :test-not #'= :test (nothing))))
  ;; (expect (eql 1 (list:find 0 '(0 1 2 3 4 5) :start (nothing) :test-not #'= :test (nothing))))

  (expect (eql 0 (list:find 4 '(0 1 2 3 4 5) :end (nothing) :test #'/= :test-not (nothing))))
  (expect (eql 1 (list:find 0 '(0 1 2 3 4 5) :end (nothing) :test #'/= :test-not (nothing))))

  (expect (eql 0 (list:find 4 '(0 1 2 3 4 5) :end (nothing) :test-not #'= :test (nothing))))
  (expect (eql 1 (list:find 0 '(0 1 2 3 4 5) :end (nothing) :test-not #'= :test (nothing))))

  (expect (eql 0 (list:find 4 '(0 1 2 3 4 5) :end nil :test #'/= :test-not (nothing))))
  (expect (eql 1 (list:find 0 '(0 1 2 3 4 5) :end nil :test #'/= :test-not (nothing))))

  (expect (eql 0 (list:find 4 '(0 1 2 3 4 5) :end nil :test-not #'= :test (nothing))))
  (expect (eql 1 (list:find 0 '(0 1 2 3 4 5) :end nil :test-not #'= :test (nothing)))))

(deftest find-test-2 ()
  (flet ((plus1 (x) (1- x))) ; not that plus1
    (expect (eql 3 (list:find 4 '(1 2 3 4 5) :test #'= :key 'plus1 :key #'1- :test '/=)))
    (expect (eql NIL (list:find 0 '(0 1 2 3 4 5) :test #'= :key 'plus1)))

    ;; using the above plus1.
    (expect (eql 5 (list:find 4 '(1 2 3 4 5) :test #'= :key #'plus1)))
    (expect (eql 1 (list:find 0 '(0 1 2 3 4 5) :test #'= :key #'plus1))))

  (flet ((itendity (x) (declare (ignore x)) 4)) ; not that itendity
    (expect (eql 4 (list:find 4 '(1 2 3 4 5) :test #'= :key 'itendity)))
    (expect (eql NIL (list:find 0 '(1 2 3 4 5) :test #'= :key 'itendity)))

    ;; using the above itendity.
    (expect (eql 1 (list:find 4 '(1 2 3 4 5) :test #'= :key #'itendity)))
    (expect (eql NIL (list:find 0 '(1 2 3 4 5) :test #'= :key #'itendity))))

  (flet ((eqq (x y) (not (eq x y)))) ; not that EQQ
    (expect (eql 4 (list:find 4 '(1 2 3 4 5) :test 'eqq :test 'meq)))
    (expect (eql NIL (list:find 0 '(1 2 3 4 5) :test 'eqq)))

    ;; using the above eqq
    (expect (eql 1 (list:find 4 '(1 2 3 4 5) :test #'eqq)))
    (expect (eql 1 (list:find 0 '(1 2 3 4 5) :test #'eqq))))

  (expect (eql 4 (list:find 4 '(1 2 3 4 5) :test-not #'meq)))
  (expect (eql NIL (list:find 0 '(1 2 3 4 5) :test-not #'meq)))

  (expect (eql 4 (list:find 4 '(1 2 3 4 5) :test (nothing) :test-not #'meq)))
  (expect (eql NIL (list:find 0 '(1 2 3 4 5) :test (nothing) :test-not #'meq)))

  (expect-error (list:find 4 '(1 2 3 4 5) :test (itendity #'eqq) :test-not (itendity #'meq)))
  (expect-error (list:find 0 '(1 2 3 4 5) :test (itendity #'eqq) :test-not (itendity #'meq)))

  (flet ((meq (x y) (eq x y))) ; not that MEQ
    (expect (eql 4 (list:find 4 '(1 2 3 4 5) :test-not 'meq)))
    (expect (eql NIL (list:find 0 '(1 2 3 4 5) :test-not 'meq)))

    ;; using the above meq.
    (expect (eql 1 (list:find 4 '(1 2 3 4 5) :test-not #'meq)))
    (expect (eql 1 (list:find 0 '(1 2 3 4 5) :test-not #'meq)))))

(deftest find-if-test ()
  (expect (eql 1 (list:find-if #'oddp '(0 1 2 3 4 5))))
  (expect (eql NIL (list:find-if #'oddp '(0 2 4))))

  (flet ((oggp (x) (not (oddp x)))) ; not that OGGP
    (expect (eql 1 (list:find-if 'oggp '(0 1 2 3 4 5))))
    (expect (eql NIL (list:find-if 'oggp '(0 2 4))))

    ;; using the above oggp.
    (expect (eql 0 (list:find-if #'oggp '(0 1 2 3 4 5))))
    (expect (eql 0 (list:find-if #'oggp '(0 2 4)))))

  (expect (eql 3 (list:find-if #'oddp '(0 1 2 3 4 5) :start 2)))
  (expect (eql NIL (list:find-if #'oddp '(0 2 4) :start 2)))

  (expect (eql 3 (list:find-if #'oddp '(0 2 3 4 5) :end 3)))
  (expect (eql NIL (list:find-if #'oddp '(0 2 4 5) :end 3)))

  (expect (eql 5 (list:find-if #'oddp '(0 1 2 3 4 5) :from-end t)))
  (expect (eql NIL (list:find-if #'oddp '(0 2 4) :from-end t)))

  (expect (eql 1 (list:find-if-not #'evenp '(0 1 2 3 4 5))))
  (expect (eql NIL (list:find-if-not #'evenp '(0 2 4))))

  (flet ((efenp (x) (not (efenp x)))) ; not that efenp
    (expect (eql 1 (list:find-if-not 'efenp '(0 1 2 3 4 5))))
    (expect (eql NIL (list:find-if-not 'efenp '(0 2 4))))

    ;; using the above efenp.
    (expect (eql 0 (list:find-if-not #'efenp '(0 1 2 3 4 5))))
    (expect (eql 0 (list:find-if-not #'efenp '(0 2 4))))))

(deftest position-*-test ()
  (expect (eql 4 (list:position 4 '(0 1 2 3 4 5) :test #'=)))
  (expect (eql NIL (list:position 0 '(1 2 3 4 5) :test #'=)))

  (expect (eql 7 (list:position 4 '(0 1 2 3 4 5 6 4 8) :from-end t)))
  (expect (eql NIL (list:position 0 '(1 2 3 4 5 6 7) :from-end t)))

  (expect (eql 1 (list:position-if #'oddp '(0 1 2 3 4 5))))
  (expect (eql NIL (list:position-if #'oddp '(0 2 4))))

  (expect (eql 3 (list:position-if #'oddp '(0 1 2 3 4 5) :start 2)))
  (expect (eql NIL (list:position-if #'oddp '(0 2 4) :start 2)))

  (expect (eql 2 (list:position-if #'oddp '(0 2 3 4 5) :end 3)))
  (expect (eql NIL (list:position-if #'oddp '(0 2 4 5) :end 3)))

  (expect (eql 5 (list:position-if #'oddp '(0 1 2 3 4 5) :from-end t)))
  (expect (eql NIL (list:position-if #'oddp '(0 2 4) :from-end nil)))

  (expect (eql 1 (list:position-if-not #'evenp '(0 1 2 3 4 5))))
  (expect (eql NIL (list:position-if-not #'evenp '(0 2 4)))))

;;;
;;; remove
;;;

(deftest remove-test-1 ()
  (expect (equal (list )  (list:remove 1 (list ) :test #'=)))
  (expect (equal (list 1) (list:remove 0 (list 1) :test #'=)))
  (expect (equal (list )  (list:remove 1 (list 1) :test #'=)))
  (expect (equal (list 1) (list:remove 1 (list 1) :start 1 :test #'=)))
  (expect (equal (list 1) (list:remove 1 (list 1) :end 0 :test #'=)))

  (let ((l (list 1 2 3 4 5))
        (l-4 (list 1 2 3 5)))
    (expect (equal l-4 (list:remove 4 l)))
    (expect (eq l (list:remove 0 l)))
    (expect (equal l-4 (list:remove 4 l :test #'=)))
    (expect (eq l (list:remove 0 l :test #'=)))

    (expect
     (eq l (list:remove 4 l :start 3 :test #'= :key #'1+ :test-not (nothing))))

    (expect (equal (list 1 2 3 4 5) l))
    (expect (equal (list 1 2 3 5) l-4)))

  (expect
   (equal (list 1 2 4 5)
          (list:remove 4 (list 1 2 3 4 5) :test #'= :key #'1+ :test #'/=)))
  (expect
   (equal (list 0 1 2 3 4 5)
          (list:remove 0 (list 0 1 2 3 4 5) :test #'= :key #'1+)))

  (expect
   (equal (list 1 2 4 5)
          (list:remove 4 (list 1 2 3 4 5) :test #'= :key #'1+ :test-not (nothing))))
  (expect
   (equal (list 1 2 3 4 5)
          (list:remove
           4 (list 1 2 3 4 5) :start 3 :test #'= :key #'1+ :test-not (nothing))))

  (expect
   (equal (list 0 1 2 3 4 5)
          (list:remove 0 (list 0 1 2 3 4 5) :test #'= :key #'1+ :test-not (nothing))))
  (let ((l (list 0 1 2 3 4 5)))
    (expect (eq l (list:remove 0 l :test #'= :key #'1+)))
    (expect (eq l (list:remove 0 l :test #'= :key #'1+ :test-not (nothing))))
    (expect (equal (list 0 1 2 3 4 5) l)))

  (expect
   (equal (list 3)
          (list:remove 4 (list 0 1 2 3 4 5) :test #'/= :key #'1+ :test-not (nothing))))
  (expect
   (equal nil
          (list:remove 0 (list 0 1 2 3 4 5) :test #'/= :key #'1+ :test-not (nothing))))

  (expect
   (equal (list 4)
          (list:remove 4 (list 0 1 2 3 4 5) :test #'/= :test-not (nothing))))
  (expect
   (equal (list 0)
          (list:remove 0 (list 0 1 2 3 4 5) :test #'/= :test-not (nothing))))

  (expect
   (equal (list 4)
          (list:remove 4 (list 0 1 2 3 4 5) :test-not #'= :test (nothing))))
  (expect
   (equal (list 0)
          (list:remove 0 (list 0 1 2 3 4 5) :test-not #'= :test (nothing))))
  (expect
   (equal (list 4)
          (list:remove 4 (list 0 1 2 3 4 5) :key (nothing) :test #'/= :test-not (nothing))))
  (expect
   (equal (list 0)
          (list:remove 0 (list 0 1 2 3 4 5) :key (nothing) :test #'/= :test-not (nothing))))
  (expect
   (equal (list 4)
          (list:remove 4 (list 0 1 2 3 4 5) :key (nothing) :test-not #'= :test (nothing))))
  (expect
   (equal (list 0)
          (list:remove 0 (list 0 1 2 3 4 5) :key (nothing) :test-not #'= :test (nothing))))

  (expect
   (equal (list 4)
          (list:remove 4 (list 0 1 2 3 4 5) :end (nothing) :test #'/= :test-not (nothing))))
  (expect
   (equal (list 0)
          (list:remove 0 (list 0 1 2 3 4 5) :end (nothing) :test #'/= :test-not (nothing))))

  (expect
   (equal (list 4)
          (list:remove 4 (list 0 1 2 3 4 5) :end (nothing) :test-not #'= :test (nothing))))
  (expect
   (equal (list 0)
          (list:remove 0 (list 0 1 2 3 4 5) :end (nothing) :test-not #'= :test (nothing))))

  (expect
   (equal (list 4)
          (list:remove 4 (list 0 1 2 3 4 5) :end nil :test #'/= :test-not (nothing))))
  (expect
   (equal (list 0)
          (list:remove 0 (list 0 1 2 3 4 5) :end nil :test #'/= :test-not (nothing))))

  (expect
   (equal (list 4)
          (list:remove 4 (list 0 1 2 3 4 5) :end nil :test-not #'= :test (nothing))))
  (expect
   (equal (list 0)
          (list:remove 0 (list 0 1 2 3 4 5) :end nil :test-not #'= :test (nothing)))))

(deftest remove-test-1-count ()
  (expect (equal (list )  (list:remove 1 (list ) :test #'= :count 1)))
  (expect (equal (list 1) (list:remove 0 (list 1) :test #'= :count 0)))
  (expect (equal (list )  (list:remove 1 (list 1) :test #'= :count 1)))
  (expect (equal (list 1) (list:remove 1 (list 1) :test #'= :count 0)))
  (expect (equal (list 1) (list:remove 1 (list 1) :start 1 :test #'= :count 10)))
  (expect (equal (list 1) (list:remove 1 (list 1) :end 0 :test #'= :count 1000)))

  (let ((l (list 1 2 3 4 5))
        (l-4 (list 1 2 3 5)))
    (expect (equal l-4 (list:remove 4 l :count 1)))
    (expect (eq l (list:remove 0 l)))
    (expect (eq l (list:remove 4 l :count 0)))
    (expect (equal l-4 (list:remove 4 l :test #'= :count 1)))
    (expect (eq l (list:remove 0 l :test #'= :count 0)))
    (expect (eq l (list:remove 0 l :test #'= :count 1)))

    (expect
     (eq l (list:remove
            4 l :start 3 :test #'= :key #'1+ :count (nothing))))
    (expect
     (eq l (list:remove
            4 l :start 3 :test #'= :key #'1+ :test-not (nothing) :count 0)))
    (expect
     (eq l (list:remove
            4 l :start 3 :test #'= :key #'1+ :test-not (nothing) :count 1)))

    (expect (eq l (list:remove 4 l :start 3 :test #'/= :count 0)))

    (expect (equal (list 4) (list:remove 4 l :test #'/= :count nil)))
    (expect (equal (list 4) (list:remove 4 l :test #'/= :count (nothing))))
    (expect (equal (list 4 5) (list:remove 4 l :test #'/= :count 3)))
    (expect (equal (list 1 4) (list:remove 4 l :start 1 :test #'/= :count 3)))
    (expect (equal (list 1 4 5) (list:remove 4 l :start 1 :test #'/= :count 2)))
    (expect
     (equal (list 1 2 4)
            (list:remove 4 l :start 1 :test #'/= :count 2 :from-end t)))
    (expect
     (equal (list 1 4 5)
            (list:remove 4 l :start 1 :end 4 :test #'/= :count 2 :from-end t)))
    (expect
     (equal (list 1 2 3 4 8 9)
            (list:remove
             4 (list 1 2 3 4 5 6 7 8 9)
             :start 1 :end 7 :test #'/= :count 3 :from-end t)))
    (expect
     (equal (list 4)
            (list:remove
             4 (list 4 5 6 7) :start 1 :end 4 :test #'/= :count 3 :from-end t)))
    (expect
     (equal nil (list:remove 4 nil :test #'/= :count 3 :from-end t)))

    (expect (equal (list 1 2 3 4 5) l))
    (expect (equal (list 1 2 3 5) l-4))))

(deftest remove-test-1-count-notinline ()
  ;; Test for the "unoptimized" version of list:remove.
  ;; This for the case when #'list:remove is called instead of inlined.
  (declare (notinline list:remove))
  (expect (equal (list )  (list:remove 1 (list ) :test #'= :count 1)))
  (expect (equal (list 1) (list:remove 0 (list 1) :test #'= :count 0)))
  (expect (equal (list )  (list:remove 1 (list 1) :test #'= :count 1)))
  (expect (equal (list 1) (list:remove 1 (list 1) :test #'= :count 0)))
  (expect (equal (list 1) (list:remove 1 (list 1) :start 1 :test #'= :count 10)))
  (expect (equal (list 1) (list:remove 1 (list 1) :end 0 :test #'= :count 1000)))

  (let ((l (list 1 2 3 4 5))
        (l-4 (list 1 2 3 5)))
    (expect (equal l-4 (list:remove 4 l :count 1)))
    (expect (eq l (list:remove 0 l)))
    (expect (eq l (list:remove 4 l :count 0)))
    (expect (equal l-4 (list:remove 4 l :test #'= :count 1)))
    (expect (eq l (list:remove 0 l :test #'= :count 0)))
    (expect (eq l (list:remove 0 l :test #'= :count 1)))

    (expect
     (eq l (list:remove
            4 l :start 3 :test #'= :key #'1+ :count (nothing))))
    (expect
     (eq l (list:remove
            4 l :start 3 :test #'= :key #'1+ :test-not (nothing) :count 0)))
    (expect
     (eq l (list:remove
            4 l :start 3 :test #'= :key #'1+ :test-not (nothing) :count 1)))

    (expect (eq l (list:remove 4 l :start 3 :test #'/= :count 0)))

    (expect (equal (list 4) (list:remove 4 l :test #'/= :count nil)))
    (expect (equal (list 4) (list:remove 4 l :test #'/= :count (nothing))))
    (expect (equal (list 4 5) (list:remove 4 l :test #'/= :count 3)))
    (expect (equal (list 1 4) (list:remove 4 l :start 1 :test #'/= :count 3)))
    (expect (equal (list 1 4 5) (list:remove 4 l :start 1 :test #'/= :count 2)))
    (expect
     (equal (list 1 2 4)
            (list:remove 4 l :start 1 :test #'/= :count 2 :from-end t)))
    (expect
     (equal (list 1 4 5)
            (list:remove 4 l :start 1 :end 4 :test #'/= :count 2 :from-end t)))
    (expect
     (equal (list 1 2 3 4 8 9)
            (list:remove
             4 (list 1 2 3 4 5 6 7 8 9)
             :start 1 :end 7 :test #'/= :count 3 :from-end t)))
    (expect
     (equal (list 4)
            (list:remove
             4 (list 4 5 6 7) :start 1 :end 4 :test #'/= :count 3 :from-end t)))
    (expect
     (equal nil (list:remove 4 nil :test #'/= :count 3 :from-end t)))

    (expect (equal (list 1 2 3 4 5) l))
    (expect (equal (list 1 2 3 5) l-4))))

(deftest remove-test-1-long ()
  (let ((l (list 1 2 3 4 5 6 7 8 9))
        (l-4 (list 1 2 3 5 6 7 8 9))
        (l09 (list 0 1 2 3 4 5 6 7 8 9)))
    (expect (equal l-4 (list:remove 4 l)))
    (expect (eq l (list:remove 0 l)))
    (expect (equal l-4 (list:remove 4 l :test #'=)))
    (expect (eq l (list:remove 0 l :test #'=)))

    (expect
     (eq l (list:remove 4 l :start 3 :test #'= :key #'1+ :test-not (nothing))))

    (expect
     (equal (list 1 2 4 5 6 7 8 9)
            (list:remove 4 (list 1 2 3 4 5 6 7 8 9) :test #'= :key #'1+ :test #'/=)))
    (expect
     (eq l09 (list:remove 0 l09 :test #'= :key #'1+)))

    (expect
     (equal (list 1 2 4 5 6 7 8 9)
            (list:remove 4 (list 1 2 3 4 5 6 7 8 9) :test #'= :key #'1+ :test-not (nothing))))
    (expect
     (equal (list 1 2 3 4 5 6 7 8 9)
            (list:remove
             4 (list 1 2 3 4 5 6 7 8 9) :start 3 :test #'= :key #'1+ :test-not (nothing))))

    (expect
     (eq l09 (list:remove 0 l09 :test #'= :key #'1+ :test-not (nothing))))

    (expect
     (equal (list 3)
            (list:remove 4 l09 :test #'/= :key #'1+ :test-not (nothing))))
    (expect
     (equal nil
            (list:remove 0 l09 :test #'/= :key #'1+ :test-not (nothing))))

    (expect
     (equal (list 4)
            (list:remove 4 l09 :test #'/= :test-not (nothing))))
    (expect
     (equal (list 0)
            (list:remove 0 l09 :test #'/= :test-not (nothing))))

    (expect
     (equal (list 4)
            (list:remove 4 l09 :test-not #'= :test (nothing))))
    (expect
     (equal (list 0)
            (list:remove 0 l09 :test-not #'= :test (nothing))))
    (expect
     (equal (list 4)
            (list:remove 4 l09 :key (nothing) :test #'/= :test-not (nothing))))
    (expect
     (equal (list 0)
            (list:remove 0 l09 :key (nothing) :test #'/= :test-not (nothing))))
    (expect
     (equal (list 4)
            (list:remove 4 l09 :key (nothing) :test-not #'= :test (nothing))))
    (expect
     (equal (list 0)
            (list:remove 0 l09 :key (nothing) :test-not #'= :test (nothing))))
    (expect
     (equal (list 4)
            (list:remove 4 l09 :end (nothing) :test #'/= :test-not (nothing))))
    (expect
     (equal (list 0)
            (list:remove 0 l09 :end (nothing) :test #'/= :test-not (nothing))))

    (expect
     (equal (list 4)
            (list:remove 4 l09 :end (nothing) :test-not #'= :test (nothing))))
    (expect
     (equal (list 0)
            (list:remove 0 l09 :end (nothing) :test-not #'= :test (nothing))))

    (expect
     (equal (list 4)
            (list:remove 4 l09 :end nil :test #'/= :test-not (nothing))))
    (expect
     (equal (list 0)
            (list:remove 0 l09 :end nil :test #'/= :test-not (nothing))))

    (expect
     (equal (list 4)
            (list:remove 4 l09 :end nil :test-not #'= :test (nothing))))
    (expect
     (equal (list 0)
            (list:remove 0 l09 :end nil :test-not #'= :test (nothing))))

    (expect (equal l (list 1 2 3 4 5 6 7 8 9)))
    (expect (equal l-4 (list 1 2 3 5 6 7 8 9)))
    (expect (equal l09 (list 0 1 2 3 4 5 6 7 8 9)))))

(deftest remove-test-1-notinline ()
  ;; Test for the "unoptimized" version of list:remove.
  ;; This for the case when #'list:remove is called instead of inlined.
  (declare (notinline list:remove))
  (expect (equal (list 1) (list:remove 0 (list 1) :test #'=)))
  (expect (equal (list )  (list:remove 1 (list 1) :test #'=)))
  (expect (equal (list 1) (list:remove 1 (list 1) :start 1 :test #'=)))
  (expect (equal (list 1) (list:remove 1 (list 1) :end 0 :test #'=)))

  (let ((l (list 1 2 3 4 5))
        (l-4 (list 1 2 3 5)))
    (expect (equal l-4 (list:remove 4 l)))
    (expect (eq l (list:remove 0 l)))
    (expect (equal l-4 (list:remove 4 l :test #'=)))
    (expect (eq l (list:remove 0 l :test #'=)))

    (expect
     (eq l (list:remove 4 l :start 3 :test #'= :key #'1+ :test-not (nothing))))

    (expect (equal l (list 1 2 3 4 5)))
    (expect (equal l-4 (list 1 2 3 5))))

  (expect
   (equal (list 1 2 4 5)
          (list:remove 4 (list 1 2 3 4 5) :test #'= :key #'1+ :test #'/=)))
  (expect
   (equal (list 0 1 2 3 4 5)
          (list:remove 0 (list 0 1 2 3 4 5) :test #'= :key #'1+)))

  (expect
   (equal (list 1 2 4 5)
          (list:remove 4 (list 1 2 3 4 5) :test #'= :key #'1+ :test-not (nothing))))
  (expect
   (equal (list 1 2 3 4 5)
          (list:remove
           4 (list 1 2 3 4 5) :start 3 :test #'= :key #'1+ :test-not (nothing))))

  (expect
   (equal (list 0 1 2 3 4 5)
          (list:remove 0 (list 0 1 2 3 4 5) :test #'= :key #'1+ :test-not (nothing))))
  (let ((l (list 0 1 2 3 4 5)))
    (expect (eq l (list:remove 0 l :test #'= :key #'1+)))
    (expect (eq l (list:remove 0 l :test #'= :key #'1+ :test-not (nothing))))
    (expect (equal l (list 0 1 2 3 4 5))))

  (expect
   (equal (list 3)
          (list:remove 4 (list 0 1 2 3 4 5) :test #'/= :key #'1+ :test-not (nothing))))
  (expect
   (equal nil
          (list:remove 0 (list 0 1 2 3 4 5) :test #'/= :key #'1+ :test-not (nothing))))

  (expect
   (equal (list 4)
          (list:remove 4 (list 0 1 2 3 4 5) :test #'/= :test-not (nothing))))
  (expect
   (equal (list 0)
          (list:remove 0 (list 0 1 2 3 4 5) :test #'/= :test-not (nothing))))

  (expect
   (equal (list 4)
          (list:remove 4 (list 0 1 2 3 4 5) :test-not #'= :test (nothing))))
  (expect
   (equal (list 0)
          (list:remove 0 (list 0 1 2 3 4 5) :test-not #'= :test (nothing))))
  (expect
   (equal (list 4)
          (list:remove 4 (list 0 1 2 3 4 5) :key (nothing) :test #'/= :test-not (nothing))))
  (expect
   (equal (list 0)
          (list:remove 0 (list 0 1 2 3 4 5) :key (nothing) :test #'/= :test-not (nothing))))
  (expect
   (equal (list 4)
          (list:remove 4 (list 0 1 2 3 4 5) :key (nothing) :test-not #'= :test (nothing))))
  (expect
   (equal (list 0)
          (list:remove 0 (list 0 1 2 3 4 5) :key (nothing) :test-not #'= :test (nothing))))

  (expect
   (equal (list 4)
          (list:remove 4 (list 0 1 2 3 4 5) :end (nothing) :test #'/= :test-not (nothing))))
  (expect
   (equal (list 0)
          (list:remove 0 (list 0 1 2 3 4 5) :end (nothing) :test #'/= :test-not (nothing))))

  (expect
   (equal (list 4)
          (list:remove 4 (list 0 1 2 3 4 5) :end (nothing) :test-not #'= :test (nothing))))
  (expect
   (equal (list 0)
          (list:remove 0 (list 0 1 2 3 4 5) :end (nothing) :test-not #'= :test (nothing))))

  (expect
   (equal (list 4)
          (list:remove 4 (list 0 1 2 3 4 5) :end nil :test #'/= :test-not (nothing))))
  (expect
   (equal (list 0)
          (list:remove 0 (list 0 1 2 3 4 5) :end nil :test #'/= :test-not (nothing))))

  (expect
   (equal (list 4)
          (list:remove 4 (list 0 1 2 3 4 5) :end nil :test-not #'= :test (nothing))))
  (expect
   (equal (list 0)
          (list:remove 0 (list 0 1 2 3 4 5) :end nil :test-not #'= :test (nothing)))))

(deftest remove-test-2 ()
  (let* ((l (list 1 2 3 4 5))
         (l0 (list* 0 l)))
    (flet ((plus1 (x) (1- x))) ; not that plus1
      (expect
       (equal (list 1 2 4 5)
              (list:remove 4 l :test #'= :key 'plus1 :key #'1- :test '/=)))
      (expect (eql l0 (list:remove 0 l0 :test #'= :key 'plus1)))

      ;; using the above plus1.
      (expect (equal (list 1 2 3 4) (list:remove 4 l :test #'= :key #'plus1)))
      (expect (equal (list 0 2 3 4 5) (list:remove 0 l0 :test #'= :key #'plus1))))

    (flet ((itendity (x) (declare (ignore x)) 4)) ; not that identity
      (expect (equal (list 1 2 3 5) (list:remove 4 l :test #'= :key 'itendity)))
      (expect (eq l (list:remove 0 l :test #'= :key 'itendity)))

      ;; using the above itendity.
      (expect (null (list:remove 4 l :test #'= :key #'itendity)))
      (expect (eq l (list:remove 0 l :test #'= :key #'itendity))))

    (flet ((eqq (x y) (not (eq x y)))) ; not that EQQ
      (expect (equal (list 1 2 3 5) (list:remove 4 l :test 'eqq :test 'meq)))
      (expect (eq l (list:remove 0 l :test 'eqq)))

      ;; using the above eqq
      (expect (equal (list 4) (list:remove 4 l :test #'eqq)))
      (expect (null (list:remove 0 l :test #'eqq))))

    (expect (equal (list 1 2 3 5) (list:remove 4 l :test-not #'meq)))
    (expect (eq l (list:remove 0 l :test-not #'meq)))

    (expect (equal (list 1 2 3 5)
                   (list:remove 4 l :test (nothing) :test-not #'meq)))
    (expect (eql l (list:remove 0 l :test (nothing) :test-not #'meq)))

    (expect-error
      (list:remove 4 l :test (itendity #'eqq) :test-not (itendity #'meq)))
    (expect-error
      (list:remove 0 l :test (itendity #'eqq) :test-not (itendity #'meq)))

    (flet ((meq (x y) (eq x y))) ; not that MEQ
      (expect (equal (list 1 2 3 5) (list:remove 4 l :test-not 'meq)))
      (expect (eq l (list:remove 0 l :test-not 'meq)))

      ;; using the above meq.
      (expect (equal (list 4) (list:remove 4 l :test-not #'meq)))
      (expect (null (list:remove 0 l :test-not #'meq))))

    (expect (equal l (list 1 2 3 4 5)))
    (expect (equal l0 (list* 0 l)))))

(deftest remove-if-test ()
  (expect (equal (list 0 2 4) (list:remove-if #'oddp (list 0 1 2 3 4 5))))
  (expect (equal (list 0 2 4) (list:remove-if #'oddp (list 0 2 4))))

  (flet ((oggp (x) (not (oddp x)))) ; not that OGGP
    (expect (equal (list 0 2 4) (list:remove-if 'oggp (list 0 1 2 3 4 5))))
    (expect (equal (list 0 2 4) (list:remove-if 'oggp (list 0 2 4))))

    ;; using the above oggp.
    (expect (equal (list 1 3 5) (list:remove-if #'oggp (list 0 1 2 3 4 5))))
    (expect (null (list:remove-if #'oggp (list 0 2 4)))))

  (expect (equal (list 0 1 2 4) (list:remove-if #'oddp (list 0 1 2 3 4 5) :start 2)))
  (expect (equal (list 0 2 4) (list:remove-if #'oddp (list 0 2 4) :start 2)))

  (expect (equal (list 0 2 4 5) (list:remove-if #'oddp (list 0 2 3 4 5) :end 3)))
  (expect (equal (list 0 2 4 5) (list:remove-if #'oddp (list 0 2 4 5) :end 3)))

  (expect (equal (list 0 2 4 5) (list:remove-if #'oddp (list 0 1 2 3 4 5) :count 2)))
  (let ((l (list 0 2 4)))
    (expect (eql l (list:remove-if #'oddp l :count 1)))
    (expect (equal (list 0 2 4) l)))

  (expect
   (equal (list 0 1 2 4)
          (list:remove-if #'oddp (list 0 1 2 3 4 5) :from-end t :count 2)))
  (expect
   (equal (list 0 2 4) (list:remove-if #'oddp (list 0 2 4) :from-end t :count 1)))

  (expect (equal (list 0 2 4) (list:remove-if-not #'evenp (list 0 1 2 3 4 5))))
  (expect (equal (list 0 2 4) (list:remove-if-not #'evenp (list 0 2 4))))

  (flet ((efenp (x) (not (efenp x)))) ; not that efenp
    (expect (equal (list 0 2 4) (list:remove-if-not 'efenp (list 0 1 2 3 4 5))))
    (expect (equal (list 0 2 4) (list:remove-if-not 'efenp (list 0 2 4))))

    ;; using the above efenp.
    (expect (equal (list 1 3 5) (list:remove-if-not #'efenp (list 0 1 2 3 4 5))))
    (expect (equal (list ) (list:remove-if-not #'efenp (list 0 2 4))))))

(deftest remove-if-test-notinline ()
  ;; Test for the "unoptimized" version of list:remove-if(-not).
  ;; This for the case when #'list:remove-if is called instead of inlined.
  (declare (notinline list:remove-if list:remove-if-not))
  (expect (equal (list 0 2 4) (list:remove-if #'oddp (list 0 1 2 3 4 5))))
  (expect (equal (list 0 2 4) (list:remove-if #'oddp (list 0 2 4))))

  (flet ((oggp (x) (not (oddp x)))) ; not that OGGP
    (expect (equal (list 0 2 4) (list:remove-if 'oggp (list 0 1 2 3 4 5))))
    (expect (equal (list 0 2 4) (list:remove-if 'oggp (list 0 2 4))))

    ;; using the above oggp.
    (expect (equal (list 1 3 5) (list:remove-if #'oggp (list 0 1 2 3 4 5))))
    (expect (null (list:remove-if #'oggp (list 0 2 4)))))

  (expect (equal (list 0 1 2 4) (list:remove-if #'oddp (list 0 1 2 3 4 5) :start 2)))
  (expect (equal (list 0 2 4) (list:remove-if #'oddp (list 0 2 4) :start 2)))

  (expect (equal (list 0 2 4 5) (list:remove-if #'oddp (list 0 2 3 4 5) :end 3)))
  (expect (equal (list 0 2 4 5) (list:remove-if #'oddp (list 0 2 4 5) :end 3)))

  (expect (equal (list 0 2 4 5) (list:remove-if #'oddp (list 0 1 2 3 4 5) :count 2)))

  (let ((l (list 0 2 4)))
    (expect (eql l (list:remove-if #'oddp l :count 1)))
    (expect (equal (list 0 2 4) l)))

  (expect
   (equal (list 0 1 2 4)
          (list:remove-if #'oddp (list 0 1 2 3 4 5) :from-end t :count 2)))
  (expect
   (equal (list 0 2 4) (list:remove-if #'oddp (list 0 2 4) :from-end t :count 1)))

  (expect (equal (list 0 2 4) (list:remove-if-not #'evenp (list 0 1 2 3 4 5))))
  (expect (equal (list 0 2 4) (list:remove-if-not #'evenp (list 0 2 4))))

  (flet ((efenp (x) (not (efenp x)))) ; not that efenp
    (expect (equal (list 0 2 4) (list:remove-if-not 'efenp (list 0 1 2 3 4 5))))
    (expect (equal (list 0 2 4) (list:remove-if-not 'efenp (list 0 2 4))))

    ;; using the above efenp.
    (expect (equal (list 1 3 5) (list:remove-if-not #'efenp (list 0 1 2 3 4 5))))
    (expect (equal (list ) (list:remove-if-not #'efenp (list 0 2 4))))))

;;;
;;; delete
;;;

(deftest delete-test-1 ()
  (expect (equal (list )  (list:delete 1 (list ) :test #'=)))
  (expect (equal (list 1) (list:delete 0 (list 1) :test #'=)))
  (expect (equal (list )  (list:delete 1 (list 1) :test #'=)))
  (expect (equal (list 1) (list:delete 1 (list 1) :start 1 :test #'=)))
  (expect (equal (list 1) (list:delete 1 (list 1) :end 0 :test #'=)))

  (let* ((l (list 1 2 3 4 5))
         (l* l)
         (l-4 (list 1 2 3 5)))
    (expect (equal l-4 (list:delete 4 (list 1 2 3 4 5))))
    (expect (eq l (list:delete 0 l)))
    (expect (equal l-4 (list:delete 4 (list 1 2 3 4 5) :test #'=)))
    (expect (eq l (list:delete 0 l :test #'=)))

    (expect
     (eq l (list:delete 4 l :start 3 :test #'= :key #'1+ :test-not (nothing))))
    (expect (equal l (list 1 2 3 4 5)))
    (expect (eq l l*)))

  (expect
   (equal (list 1 2 4 5)
          (list:delete 4 (list 1 2 3 4 5) :test #'= :key #'1+ :test #'/=)))
  (expect
   (equal (list 0 1 2 3 4 5)
          (list:delete 0 (list 0 1 2 3 4 5) :test #'= :key #'1+)))

  (expect
   (equal (list 1 2 4 5)
          (list:delete 4 (list 1 2 3 4 5) :test #'= :key #'1+ :test-not (nothing))))
  (expect
   (equal (list 1 2 3 4 5)
          (list:delete
           4 (list 1 2 3 4 5) :start 3 :test #'= :key #'1+ :test-not (nothing))))

  (expect
   (equal (list 0 1 2 3 4 5)
          (list:delete 0 (list 0 1 2 3 4 5) :test #'= :key #'1+ :test-not (nothing))))
  (let ((l (list 0 1 2 3 4 5)))
    (expect (eq l (list:delete 0 l :test #'= :key #'1+)))
    (expect (eq l (list:delete 0 l :test #'= :key #'1+ :test-not (nothing))))
    (expect (equal l (list 0 1 2 3 4 5))))

  (expect
   (equal (list 3)
          (list:delete 4 (list 0 1 2 3 4 5) :test #'/= :key #'1+ :test-not (nothing))))
  (expect
   (equal nil
          (list:delete 0 (list 0 1 2 3 4 5) :test #'/= :key #'1+ :test-not (nothing))))

  (expect
   (equal (list 4)
          (list:delete 4 (list 0 1 2 3 4 5) :test #'/= :test-not (nothing))))
  (expect
   (equal (list 0)
          (list:delete 0 (list 0 1 2 3 4 5) :test #'/= :test-not (nothing))))

  (expect
   (equal (list 4)
          (list:delete 4 (list 0 1 2 3 4 5) :test-not #'= :test (nothing))))
  (expect
   (equal (list 0)
          (list:delete 0 (list 0 1 2 3 4 5) :test-not #'= :test (nothing))))
  (expect
   (equal (list 4)
          (list:delete 4 (list 0 1 2 3 4 5) :key (nothing) :test #'/= :test-not (nothing))))
  (expect
   (equal (list 0)
          (list:delete 0 (list 0 1 2 3 4 5) :key (nothing) :test #'/= :test-not (nothing))))
  (expect
   (equal (list 4)
          (list:delete 4 (list 0 1 2 3 4 5) :key (nothing) :test-not #'= :test (nothing))))
  (expect
   (equal (list 0)
          (list:delete 0 (list 0 1 2 3 4 5) :key (nothing) :test-not #'= :test (nothing))))

  (expect
   (equal (list 4)
          (list:delete 4 (list 0 1 2 3 4 5) :end (nothing) :test #'/= :test-not (nothing))))
  (expect
   (equal (list 0)
          (list:delete 0 (list 0 1 2 3 4 5) :end (nothing) :test #'/= :test-not (nothing))))

  (expect
   (equal (list 4)
          (list:delete 4 (list 0 1 2 3 4 5) :end (nothing) :test-not #'= :test (nothing))))
  (expect
   (equal (list 0)
          (list:delete 0 (list 0 1 2 3 4 5) :end (nothing) :test-not #'= :test (nothing))))

  (expect
   (equal (list 4)
          (list:delete 4 (list 0 1 2 3 4 5) :end nil :test #'/= :test-not (nothing))))
  (expect
   (equal (list 0)
          (list:delete 0 (list 0 1 2 3 4 5) :end nil :test #'/= :test-not (nothing))))

  (expect
   (equal (list 4)
          (list:delete 4 (list 0 1 2 3 4 5) :end nil :test-not #'= :test (nothing))))
  (expect
   (equal (list 0)
          (list:delete 0 (list 0 1 2 3 4 5) :end nil :test-not #'= :test (nothing)))))

(deftest delete-test-1-count ()
  (expect (equal (list )  (list:delete 1 (list ) :test #'= :count 1)))
  (expect (equal (list 1) (list:delete 0 (list 1) :test #'= :count 0)))
  (expect (equal (list )  (list:delete 1 (list 1) :test #'= :count 1)))
  (expect (equal (list 1) (list:delete 1 (list 1) :test #'= :count 0)))
  (expect (equal (list 1) (list:delete 1 (list 1) :start 1 :test #'= :count 10)))
  (expect (equal (list 1) (list:delete 1 (list 1) :end 0 :test #'= :count 1000)))

  (let* ((l (list 1 2 3 4 5))
         (l* l)
         (l-4 (list 1 2 3 5)))
    (expect (equal l-4 (list:delete 4 (list 1 2 3 4 5) :count 1)))
    (expect (eq l (list:delete 0 l)))
    (expect (eq l (list:delete 4 l :count 0)))
    (expect (equal l-4 (list:delete 4 (list 1 2 3 4 5) :test #'= :count 1)))
    (expect (eq l (list:delete 0 l :test #'= :count 0)))
    (expect (eq l (list:delete 0 l :test #'= :count 1)))

    (expect
     (eq l (list:delete
            4 l :start 3 :test #'= :key #'1+ :count (nothing))))
    (expect
     (eq l (list:delete
            4 l :start 3 :test #'= :key #'1+ :test-not (nothing) :count 0)))
    (expect
     (eq l (list:delete
            4 l :start 3 :test #'= :key #'1+ :test-not (nothing) :count 1)))

    (expect (eq l (list:delete 4 l :start 3 :test #'/= :count 0)))

    (expect (equal (list 4) (list:delete 4 (list 1 2 3 4 5) :test #'/= :count nil)))
    (expect (equal (list 4) (list:delete 4 (list 1 2 3 4 5) :test #'/= :count (nothing))))
    (expect (equal (list 4 5) (list:delete 4 (list 1 2 3 4 5) :test #'/= :count 3)))
    (expect (equal (list 1 4) (list:delete 4 (list 1 2 3 4 5) :start 1 :test #'/= :count 3)))
    (expect (equal (list 1 4 5) (list:delete 4 (list 1 2 3 4 5) :start 1 :test #'/= :count 2)))
    (expect
     (equal (list 1 2 4)
            (list:delete 4 (list 1 2 3 4 5) :start 1 :test #'/= :count 2 :from-end t)))
    (expect
     (equal (list 1 4 5)
            (list:delete 4 (list 1 2 3 4 5) :start 1 :end 4 :test #'/= :count 2 :from-end t)))
    (expect
     (equal (list 1 2 3 4 8 9)
            (list:delete
             4 (list 1 2 3 4 5 6 7 8 9)
             :start 1 :end 7 :test #'/= :count 3 :from-end t)))
    (expect
     (equal (list 4)
            (list:delete
             4 (list 4 5 6 7) :start 1 :end 4 :test #'/= :count 3 :from-end t)))
    (expect
     (equal nil (list:delete 4 nil :test #'/= :count 3 :from-end t)))

    (expect (equal l (list 1 2 3 4 5)))
    (expect (eq l l*))))

(deftest delete-test-1-count-notinline ()
  ;; Test for the "unoptimized" version of list:delete.
  ;; This for the case when #'list:delete is called instead of inlined.
  (declare (notinline list:delete))
  (expect (equal (list )  (list:delete 1 (list ) :test #'= :count 1)))
  (expect (equal (list 1) (list:delete 0 (list 1) :test #'= :count 0)))
  (expect (equal (list )  (list:delete 1 (list 1) :test #'= :count 1)))
  (expect (equal (list 1) (list:delete 1 (list 1) :test #'= :count 0)))
  (expect (equal (list 1) (list:delete 1 (list 1) :start 1 :test #'= :count 10)))
  (expect (equal (list 1) (list:delete 1 (list 1) :end 0 :test #'= :count 1000)))

  (let* ((l (list 1 2 3 4 5))
         (l* l)
         (l-4 (list 1 2 3 5))
         (l-4* l-4))
    (expect (equal l-4 (list:delete 4 (list 1 2 3 4 5) :count 1)))
    (expect (eq l (list:delete 0 l)))
    (expect (eq l (list:delete 4 l :count 0)))
    (expect (equal l-4 (list:delete 4 (list 1 2 3 4 5) :test #'= :count 1)))
    (expect (eq l (list:delete 0 l :test #'= :count 0)))
    (expect (eq l (list:delete 0 l :test #'= :count 1)))

    (expect
     (eq l (list:delete
            4 l :start 3 :test #'= :key #'1+ :count (nothing))))
    (expect
     (eq l (list:delete
            4 l :start 3 :test #'= :key #'1+ :test-not (nothing) :count 0)))
    (expect
     (eq l (list:delete
            4 l :start 3 :test #'= :key #'1+ :test-not (nothing) :count 1)))

    (expect (eq l (list:delete 4 l :start 3 :test #'/= :count 0)))

    (expect (equal (list 4) (list:delete 4 (list 1 2 3 4 5) :test #'/= :count nil)))
    (expect (equal (list 4) (list:delete 4 (list 1 2 3 4 5) :test #'/= :count (nothing))))
    (expect (equal (list 4 5) (list:delete 4 (list 1 2 3 4 5) :test #'/= :count 3)))
    (expect (equal (list 1 4) (list:delete 4 (list 1 2 3 4 5) :start 1 :test #'/= :count 3)))
    (expect (equal (list 1 4 5) (list:delete 4 (list 1 2 3 4 5) :start 1 :test #'/= :count 2)))
    (expect
     (equal (list 1 2 4)
            (list:delete 4 (list 1 2 3 4 5) :start 1 :test #'/= :count 2 :from-end t)))
    (expect
     (equal (list 1 4 5)
            (list:delete 4 (list 1 2 3 4 5) :start 1 :end 4 :test #'/= :count 2 :from-end t)))
    (expect
     (equal (list 1 2 3 4 8 9)
            (list:delete
             4 (list 1 2 3 4 5 6 7 8 9)
             :start 1 :end 7 :test #'/= :count 3 :from-end t)))
    (expect
     (equal (list 4)
            (list:delete
             4 (list 4 5 6 7) :start 1 :end 4 :test #'/= :count 3 :from-end t)))
    (expect
     (equal nil (list:delete 4 nil :test #'/= :count 3 :from-end t)))

    (expect (equal (list 1 2 3 4 5) l))
    (expect (eq l* l))
    (expect (equal (list 1 2 3 5) l-4))
    (expect (eq l-4* l-4))))

(deftest delete-test-1-long ()
  (let* ((l (list 1 2 3 4 5 6 7 8 9))
         (l-4 (list 1 2 3 5 6 7 8 9))
         (l09 (list 0 1 2 3 4 5 6 7 8 9))
         (l* l) (l-4* l-4) (l09* l09))
    (expect (equal l-4 (list:delete 4 (list 1 2 3 4 5 6 7 8 9))))
    (expect (eq l (list:delete 0 l)))
    (expect (equal l-4 (list:delete 4 (list 1 2 3 4 5 6 7 8 9) :test #'=)))
    (expect (eq l (list:delete 0 l :test #'=)))

    (expect
     (eq l (list:delete 4 l :start 3 :test #'= :key #'1+ :test-not (nothing))))

    (expect
     (equal (list 1 2 4 5 6 7 8 9)
            (list:delete 4 (list 1 2 3 4 5 6 7 8 9) :test #'= :key #'1+ :test #'/=)))
    (expect
     (eq l09 (list:delete 0 l09 :test #'= :key #'1+)))

    (expect
     (equal (list 1 2 4 5 6 7 8 9)
            (list:delete 4 (list 1 2 3 4 5 6 7 8 9) :test #'= :key #'1+ :test-not (nothing))))
    (expect
     (equal (list 1 2 3 4 5 6 7 8 9)
            (list:delete
             4 (list 1 2 3 4 5 6 7 8 9) :start 3 :test #'= :key #'1+ :test-not (nothing))))

    (expect
     (eq l09 (list:delete 0 l09 :test #'= :key #'1+ :test-not (nothing))))

    (expect
     (equal (list 3)
            (list:delete 4 (copy-list l09) :test #'/= :key #'1+ :test-not (nothing))))
    (expect
     (equal nil
            (list:delete 0 (copy-list l09) :test #'/= :key #'1+ :test-not (nothing))))

    (expect
     (equal (list 4)
            (list:delete 4 (copy-list l09) :test #'/= :test-not (nothing))))
    (expect
     (equal (list 0)
            (list:delete 0 (copy-list l09) :test #'/= :test-not (nothing))))

    (expect
     (equal (list 4)
            (list:delete 4 (copy-list l09) :test-not #'= :test (nothing))))
    (expect
     (equal (list 0)
            (list:delete 0 (copy-list l09) :test-not #'= :test (nothing))))
    (expect
     (equal (list 4)
            (list:delete 4 (copy-list l09) :key (nothing) :test #'/= :test-not (nothing))))
    (expect
     (equal (list 0)
            (list:delete 0 (copy-list l09) :key (nothing) :test #'/= :test-not (nothing))))
    (expect
     (equal (list 4)
            (list:delete 4 (copy-list l09) :key (nothing) :test-not #'= :test (nothing))))
    (expect
     (equal (list 0)
            (list:delete 0 (copy-list l09) :key (nothing) :test-not #'= :test (nothing))))
    (expect
     (equal (list 4)
            (list:delete 4 (copy-list l09) :end (nothing) :test #'/= :test-not (nothing))))
    (expect
     (equal (list 0)
            (list:delete 0 (copy-list l09) :end (nothing) :test #'/= :test-not (nothing))))

    (expect
     (equal (list 4)
            (list:delete 4 (copy-list l09) :end (nothing) :test-not #'= :test (nothing))))
    (expect
     (equal (list 0)
            (list:delete 0 (copy-list l09) :end (nothing) :test-not #'= :test (nothing))))

    (expect
     (equal (list 4)
            (list:delete 4 (copy-list l09) :end nil :test #'/= :test-not (nothing))))
    (expect
     (equal (list 0)
            (list:delete 0 (copy-list l09) :end nil :test #'/= :test-not (nothing))))

    (expect
     (equal (list 4)
            (list:delete 4 (copy-list l09) :end nil :test-not #'= :test (nothing))))
    (expect
     (equal (list 0)
            (list:delete 0 (copy-list l09) :end nil :test-not #'= :test (nothing))))

    (expect (equal l (list 1 2 3 4 5 6 7 8 9)))
    (expect (equal l-4 (list 1 2 3 5 6 7 8 9)))
    (expect (equal l09 (list 0 1 2 3 4 5 6 7 8 9)))

    (expect (eq l l*))
    (expect (eq l-4 l-4*))
    (expect (eq l09 l09*))))

(deftest delete-test-1-notinline ()
  ;; Test for the "unoptimized" version of list:delete.
  ;; This for the case when #'list:delete is called instead of inlined.
  (declare (notinline list:delete))
  (expect (equal (list 1) (list:delete 0 (list 1) :test #'=)))
  (expect (equal (list )  (list:delete 1 (list 1) :test #'=)))
  (expect (equal (list 1) (list:delete 1 (list 1) :start 1 :test #'=)))
  (expect (equal (list 1) (list:delete 1 (list 1) :end 0 :test #'=)))

  (let* ((l (list 1 2 3 4 5))
         (l-4 (list 1 2 3 5))
         (l* l) (l-4* l-4))
    (expect (equal l-4 (list:delete 4 (list 1 2 3 4 5))))
    (expect (eq l (list:delete 0 l)))
    (expect (equal l-4 (list:delete 4 (list 1 2 3 4 5) :test #'=)))
    (expect (eq l (list:delete 0 l :test #'=)))

    (expect
     (eq l (list:delete 4 l :start 3 :test #'= :key #'1+ :test-not (nothing))))

    (expect (equal l (list 1 2 3 4 5)))
    (expect (equal l-4 (list 1 2 3 5)))
    (expect (eq l* l))
    (expect (eq l-4* l-4)))

  (expect
   (equal (list 1 2 4 5)
          (list:delete 4 (list 1 2 3 4 5) :test #'= :key #'1+ :test #'/=)))
  (expect
   (equal (list 0 1 2 3 4 5)
          (list:delete 0 (list 0 1 2 3 4 5) :test #'= :key #'1+)))

  (expect
   (equal (list 1 2 4 5)
          (list:delete 4 (list 1 2 3 4 5) :test #'= :key #'1+ :test-not (nothing))))
  (expect
   (equal (list 1 2 3 4 5)
          (list:delete
           4 (list 1 2 3 4 5) :start 3 :test #'= :key #'1+ :test-not (nothing))))

  (expect
   (equal (list 0 1 2 3 4 5)
          (list:delete 0 (list 0 1 2 3 4 5) :test #'= :key #'1+ :test-not (nothing))))
  (let ((l (list 0 1 2 3 4 5)))
    (expect (eq l (list:delete 0 l :test #'= :key #'1+)))
    (expect (eq l (list:delete 0 l :test #'= :key #'1+ :test-not (nothing))))
    (expect (equal l (list 0 1 2 3 4 5))))

  (expect
   (equal (list 3)
          (list:delete 4 (list 0 1 2 3 4 5) :test #'/= :key #'1+ :test-not (nothing))))
  (expect
   (equal nil
          (list:delete 0 (list 0 1 2 3 4 5) :test #'/= :key #'1+ :test-not (nothing))))

  (expect
   (equal (list 4)
          (list:delete 4 (list 0 1 2 3 4 5) :test #'/= :test-not (nothing))))
  (expect
   (equal (list 0)
          (list:delete 0 (list 0 1 2 3 4 5) :test #'/= :test-not (nothing))))

  (expect
   (equal (list 4)
          (list:delete 4 (list 0 1 2 3 4 5) :test-not #'= :test (nothing))))
  (expect
   (equal (list 0)
          (list:delete 0 (list 0 1 2 3 4 5) :test-not #'= :test (nothing))))
  (expect
   (equal (list 4)
          (list:delete 4 (list 0 1 2 3 4 5) :key (nothing) :test #'/= :test-not (nothing))))
  (expect
   (equal (list 0)
          (list:delete 0 (list 0 1 2 3 4 5) :key (nothing) :test #'/= :test-not (nothing))))
  (expect
   (equal (list 4)
          (list:delete 4 (list 0 1 2 3 4 5) :key (nothing) :test-not #'= :test (nothing))))
  (expect
   (equal (list 0)
          (list:delete 0 (list 0 1 2 3 4 5) :key (nothing) :test-not #'= :test (nothing))))

  (expect
   (equal (list 4)
          (list:delete 4 (list 0 1 2 3 4 5) :end (nothing) :test #'/= :test-not (nothing))))
  (expect
   (equal (list 0)
          (list:delete 0 (list 0 1 2 3 4 5) :end (nothing) :test #'/= :test-not (nothing))))

  (expect
   (equal (list 4)
          (list:delete 4 (list 0 1 2 3 4 5) :end (nothing) :test-not #'= :test (nothing))))
  (expect
   (equal (list 0)
          (list:delete 0 (list 0 1 2 3 4 5) :end (nothing) :test-not #'= :test (nothing))))

  (expect
   (equal (list 4)
          (list:delete 4 (list 0 1 2 3 4 5) :end nil :test #'/= :test-not (nothing))))
  (expect
   (equal (list 0)
          (list:delete 0 (list 0 1 2 3 4 5) :end nil :test #'/= :test-not (nothing))))

  (expect
   (equal (list 4)
          (list:delete 4 (list 0 1 2 3 4 5) :end nil :test-not #'= :test (nothing))))
  (expect
   (equal (list 0)
          (list:delete 0 (list 0 1 2 3 4 5) :end nil :test-not #'= :test (nothing)))))

(deftest delete-test-2 ()
  (let* ((l (list 1 2 3 4 5))
         (l0 (list* 0 l))
         (l* l)
         (l0* l0))
    (flet ((plus1 (x) (1- x))) ; not that plus1
      (expect
       (equal (list 1 2 4 5)
              (list:delete 4 (list 1 2 3 4 5) :test #'= :key 'plus1 :key #'1- :test '/=)))
      (expect (eql l0 (list:delete 0 l0 :test #'= :key 'plus1)))

      ;; using the above plus1.
      (expect (equal (list 1 2 3 4) (list:delete 4 (list 1 2 3 4 5) :test #'= :key #'plus1)))
      (expect (equal (list 0 2 3 4 5) (list:delete 0 (list 0 1 2 3 4 5) :test #'= :key #'plus1))))

    (flet ((itendity (x) (declare (ignore x)) 4)) ; not that identity
      (expect (equal (list 1 2 3 5) (list:delete 4 (list 1 2 3 4 5) :test #'= :key 'itendity)))
      (expect (eq l (list:delete 0 l :test #'= :key 'itendity)))

      ;; using the above itendity.
      (expect (null (list:delete 4 l :test #'= :key #'itendity)))
      (expect (eq l (list:delete 0 l :test #'= :key #'itendity))))

    (flet ((eqq (x y) (not (eq x y)))) ; not that EQQ
      (expect (equal (list 1 2 3 5) (list:delete 4 (list 1 2 3 4 5) :test 'eqq :test 'meq)))
      (expect (eq l (list:delete 0 l :test 'eqq)))

      ;; using the above eqq
      (expect (equal (list 4) (list:delete 4 (list 1 2 3 4 5) :test #'eqq)))
      (expect (null (list:delete 0 l :test #'eqq))))

    (expect (equal (list 1 2 3 5) (list:delete 4 (list 1 2 3 4 5) :test-not #'meq)))
    (expect (eq l (list:delete 0 l :test-not #'meq)))

    (expect (equal (list 1 2 3 5)
                   (list:delete 4 (list 1 2 3 4 5) :test (nothing) :test-not #'meq)))
    (expect (eql l (list:delete 0 l :test (nothing) :test-not #'meq)))

    (expect-error
      (list:delete 4 l :test (itendity #'eqq) :test-not (itendity #'meq)))
    (expect-error
      (list:delete 0 l :test (itendity #'eqq) :test-not (itendity #'meq)))

    (flet ((meq (x y) (eq x y))) ; not that MEQ
      (expect (equal (list 1 2 3 5) (list:delete 4 (list 1 2 3 4 5) :test-not 'meq)))
      (expect (eq l (list:delete 0 l :test-not 'meq)))

      ;; using the above meq.
      (expect (equal (list 4) (list:delete 4 (list 1 2 3 4 5) :test-not #'meq)))
      (expect (null (list:delete 0 l :test-not #'meq))))

    (expect (equal l (list 1 2 3 4 5)))
    (expect (equal l0 (list* 0 l)))
    (expect (eq l l*))
    (expect (eq l0 l0*))))

(deftest delete-if-test ()
  (expect (equal (list 0 2 4) (list:delete-if #'oddp (list 0 1 2 3 4 5))))
  (expect (equal (list 0 2 4) (list:delete-if #'oddp (list 0 2 4))))

  (flet ((oggp (x) (not (oddp x)))) ; not that OGGP
    (expect (equal (list 0 2 4) (list:delete-if 'oggp (list 0 1 2 3 4 5))))
    (expect (equal (list 0 2 4) (list:delete-if 'oggp (list 0 2 4))))

    ;; using the above oggp.
    (expect (equal (list 1 3 5) (list:delete-if #'oggp (list 0 1 2 3 4 5))))
    (expect (null (list:delete-if #'oggp (list 0 2 4)))))

  (expect (equal (list 0 1 2 4) (list:delete-if #'oddp (list 0 1 2 3 4 5) :start 2)))
  (expect (equal (list 0 2 4) (list:delete-if #'oddp (list 0 2 4) :start 2)))

  (expect (equal (list 0 2 4 5) (list:delete-if #'oddp (list 0 2 3 4 5) :end 3)))
  (expect (equal (list 0 2 4 5) (list:delete-if #'oddp (list 0 2 4 5) :end 3)))

  (expect (equal (list 0 2 4 5) (list:delete-if #'oddp (list 0 1 2 3 4 5) :count 2)))
  (let ((l (list 0 2 4)))
    (expect (eql l (list:delete-if #'oddp l :count 1)))
    (expect (equal (list 0 2 4) l)))

  (expect
   (equal (list 0 1 2 4)
          (list:delete-if #'oddp (list 0 1 2 3 4 5) :from-end t :count 2)))
  (expect
   (equal (list 0 2 4) (list:delete-if #'oddp (list 0 2 4) :from-end t :count 1)))

  (expect (equal (list 0 2 4) (list:delete-if-not #'evenp (list 0 1 2 3 4 5))))
  (expect (equal (list 0 2 4) (list:delete-if-not #'evenp (list 0 2 4))))

  (flet ((efenp (x) (not (efenp x)))) ; not that efenp
    (expect (equal (list 0 2 4) (list:delete-if-not 'efenp (list 0 1 2 3 4 5))))
    (expect (equal (list 0 2 4) (list:delete-if-not 'efenp (list 0 2 4))))

    ;; using the above efenp.
    (expect (equal (list 1 3 5) (list:delete-if-not #'efenp (list 0 1 2 3 4 5))))
    (expect (equal (list ) (list:delete-if-not #'efenp (list 0 2 4))))))

(deftest delete-if-test-notinline ()
  ;; Test for the "unoptimized" version of list:delete-if(-not).
  ;; This for the case when #'list:delete-if is called instead of inlined.
  (declare (notinline list:delete-if list:delete-if-not))
  (expect (equal (list 0 2 4) (list:delete-if #'oddp (list 0 1 2 3 4 5))))
  (expect (equal (list 0 2 4) (list:delete-if #'oddp (list 0 2 4))))

  (flet ((oggp (x) (not (oddp x)))) ; not that OGGP
    (expect (equal (list 0 2 4) (list:delete-if 'oggp (list 0 1 2 3 4 5))))
    (expect (equal (list 0 2 4) (list:delete-if 'oggp (list 0 2 4))))

    ;; using the above oggp.
    (expect (equal (list 1 3 5) (list:delete-if #'oggp (list 0 1 2 3 4 5))))
    (expect (null (list:delete-if #'oggp (list 0 2 4)))))

  (expect (equal (list 0 1 2 4) (list:delete-if #'oddp (list 0 1 2 3 4 5) :start 2)))
  (expect (equal (list 0 2 4) (list:delete-if #'oddp (list 0 2 4) :start 2)))

  (expect (equal (list 0 2 4 5) (list:delete-if #'oddp (list 0 2 3 4 5) :end 3)))
  (expect (equal (list 0 2 4 5) (list:delete-if #'oddp (list 0 2 4 5) :end 3)))

  (expect (equal (list 0 2 4 5) (list:delete-if #'oddp (list 0 1 2 3 4 5) :count 2)))
  (let ((l (list 0 2 4)))
    (expect (eql l (list:delete-if #'oddp l :count 1)))
    (expect (equal (list 0 2 4) l)))

  (expect
   (equal (list 0 1 2 4)
          (list:delete-if #'oddp (list 0 1 2 3 4 5) :from-end t :count 2)))
  (expect
   (equal (list 0 2 4) (list:delete-if #'oddp (list 0 2 4) :from-end t :count 1)))

  (expect (equal (list 0 2 4) (list:delete-if-not #'evenp (list 0 1 2 3 4 5))))
  (expect (equal (list 0 2 4) (list:delete-if-not #'evenp (list 0 2 4))))

  (flet ((efenp (x) (not (efenp x)))) ; not that efenp
    (expect (equal (list 0 2 4) (list:delete-if-not 'efenp (list 0 1 2 3 4 5))))
    (expect (equal (list 0 2 4) (list:delete-if-not 'efenp (list 0 2 4))))

    ;; using the above efenp.
    (expect (equal (list 1 3 5) (list:delete-if-not #'efenp (list 0 1 2 3 4 5))))
    (expect (equal (list ) (list:delete-if-not #'efenp (list 0 2 4))))))


;;; DELETE-ADJACENT

(deftest delete-adjacent ()
  (declare (notinline list:delete-adjacent))
  (expect (equal (list:delete-adjacent nil) nil))
  (let* ((input (list 1))
         (result (list:delete-adjacent input)))
     (expect (eq input result))
     (expect (equal '(1) result)))
   (let* ((input (list 1 2 2 1))
          (result (list:delete-adjacent input)))
     (expect (eq input result))
     (expect (equal '(1 2 1) result)))
  (expect (equal '(1 2 1) (list:delete-adjacent (list 1 2 2 1))))
  (expect (equal '(1 2 3) (list:delete-adjacent (list 1 1 1 2 2 3))))

  (expect (equal '(1 2 2 1)
                 (list:delete-adjacent (list 1 2 2 1) :end 2)))
  (expect (equal '(1 2 2 1)
                 (list:delete-adjacent (list 1 2 2 1) :start 2)))
  (expect (equal '(1 1 2 3)
                 (list:delete-adjacent (list 1 1 1 2 2 3) :start 1 :end 5)))

  (expect (equal '(1 2 2 1)
                 (list:delete-adjacent (list 1 2 2 1) :end 0)))
  (expect (equal '(1 2 2 1)
                 (list:delete-adjacent (list 1 2 2 1) :start 10)))
  (expect (equal '(1 2 3)
                 (list:delete-adjacent (list 1 1 1 2 2 3) :start 0 :end 20)))

  (expect (equal '("foo" "bar" "foo")
                 (list:delete-adjacent
                  (list "foo" "bar" "bar" "foo")
                  :test #'string-equal)))
  (expect (equal '("foo" "bar" "foo")
                 (list:delete-adjacent
                  (list "foo" "bar" "bar" "foo")
                  :test 'string-equal))))

(defvar start 0)
(defvar end nil)

(defun %delete-adjacent-less-more (start end)
  (declare (notinline list:delete-adjacent))
  (declare (notinline list::%%delete-adjacent))
  ;; Subtest.
  (expect (equal '(1 1 1)
                 (list:delete-adjacent (list 1 1 1 2 2 3) :test #'<
                                       :start start :end end)))
  (expect (equal '(1 1 1 2 2 3)
                 (list:delete-adjacent (list 1 1 1 2 2 3) :test #'>
                                       :start start :end end)))

  (expect (equal '(1 1 1 1)
                 (list:delete-adjacent (list 1 1 1 2 1 2 3) :test #'<
                                       :start start :end end)))
  (expect (equal '(1 1 1 2 2 3)
                 (list:delete-adjacent (list 1 1 1 2 1 2 3) :test #'>
                                       :start start :end end)))

  (expect (equal '(1 1 1 1)
                 (list:delete-adjacent (list 1 1 2 3 2 1 1) :test #'<
                                       :start start :end end)))
  (expect (equal '(1 1 2 3)
                 (list:delete-adjacent (list 1 1 2 3 2 1 1) :test #'>
                                       :start start :end end)))

  (expect (equal '(2 2 2 1 1)
                 (list:delete-adjacent (list 2 2 3 2 1 1) :test #'<
                                       :start start :end end)))
  (expect (equal '(2 2 3)
                 (list:delete-adjacent (list 2 2 3 2 1 1) :test #'>
                                       :start start :end end))))

(deftest delete-adjacent-less-more ()
  (%delete-adjacent-less-more 0 nil)
  (%delete-adjacent-less-more 1 nil)
  (%delete-adjacent-less-more 0 20)
  (%delete-adjacent-less-more 1 20))

(deftest delete-adjacent-key ()
  (declare (notinline list:delete-adjacent))
  (declare (notinline list::%%delete-adjacent))
  (expect (equal '("foo")
                 (list:delete-adjacent
                  (list "foo" "bar" "bar" "foo")
                  :key #'length)))
  (expect (equal '("foo" "bar1" "bar")
                 (list:delete-adjacent
                  (list "foo" "bar1" "bar" "foo")
                  :key #'length)))

  (expect (equal '("foo" "bar2" "foo")
                 (list:delete-adjacent
                  (list "foo" "bar1" "bar2" "foo")
                  :key #'length :from-end t)))
  (expect (equal '("foo" "bar1" "bar2" "baz-2" "foo")
                 (list:delete-adjacent
                  (list "foo" "bar1" "bar2" "baz-1" "baz-2" "foo")
                  :key #'length :from-end t :start 2)))
  (expect (equal '("foo" "bar2" "baz-1" "baz-2" "foo")
                 (list:delete-adjacent
                  (list "foo" "bar1" "bar2" "baz-1" "baz-2" "foo")
                  :key #'length :from-end t :end 4)))

  (expect (equal '("bar0" "bar5" "bar6")
                 (list:delete-adjacent
                  (list "bar0" "bar1" "bar2" "bar3" "bar4" "bar5" "bar6")
                  :key #'length :start 0 :end 5)))
  (expect (equal '("bar4" "bar5" "bar6")
                 (list:delete-adjacent
                  (list "bar0" "bar1" "bar2" "bar3" "bar4" "bar5" "bar6")
                  :key #'length :from-end t :end 5)))

  (expect (equal '("bar0" "bar1")
                 (list:delete-adjacent
                  (list "bar0" "bar1" "bar2" "bar3" "bar4" "bar5" "bar6")
                  :key #'length :start 1)))
  (expect (equal '("bar0" "bar6")
                 (list:delete-adjacent
                  (list "bar0" "bar1" "bar2" "bar3" "bar4" "bar5" "bar6")
                  :key #'length :from-end t :start 1)))

  (expect (equal '("bar0" "bar1" "bar5" "bar6")
                 (list:delete-adjacent
                  (list "bar0" "bar1" "bar2" "bar3" "bar4" "bar5" "bar6")
                  :key #'length :start 1 :end 5)))
  (expect (equal '("bar0" "bar4" "bar5" "bar6")
                 (list:delete-adjacent
                  (list "bar0" "bar1" "bar2" "bar3" "bar4" "bar5" "bar6")
                  :key #'length :from-end t :start 1 :end 5)))

  (expect (equal '("foo" "bar1" "foo")
                 (list:delete-adjacent
                  (list "foo" "bar1" "bar2" "bar3" "foo")
                  :key #'length :end 4)))
  (expect (equal '("foo" "bar3" "foo")
                 (list:delete-adjacent
                  (list "foo" "bar1" "bar2" "bar3" "foo")
                  :key #'length :from-end t :end 4))))

(deftest delete-adjacent-key-test-applications ()
  (declare (notinline list:delete-adjacent))
  (declare (notinline list::%%delete-adjacent))
  (let ((tested nil))
    (flet ((key (str)
             (push str tested)
             (length str)))
      (expect (equal '("foo" "bar3" "foo")
                 (list:delete-adjacent
                  (list "foo" "bar1" "bar2" "bar3" "foo")
                  :key #'key :from-end t :end 4))))
    (expect (equal '("foo" "bar1" "bar2" "bar3") tested)))

  (let ((tested nil))
    (flet ((key (str)
             (push str tested)
             (length str)))
      (expect (equal '("foo" "bar1" "bar3" "foo")
                 (list:delete-adjacent
                  (list "foo" "bar1" "bar2" "bar3" "foo")
                  :key #'key :from-end t :start 2))))
    (expect (equal '("bar2" "bar3" "foo") tested)))

  (let ((tested nil))
    (flet ((key (str)
             (push str tested)
             (length str)))
      (expect (equal '("foo" "bar1" "foo")
                 (list:delete-adjacent
                  (list "foo" "bar1" "bar2" "bar3" "foo") :key #'key))))
    (expect (equal '("foo" "bar3" "bar2" "bar1" "foo") tested)))

  (let ((tested nil))
    (flet ((key (str)
             (push str tested)
             (length str)))
      (expect (equal '("foo" "bar1" "baz" "bar3" "foo")
                 (list:delete-adjacent
                  (list "foo" "bar1" "bar2" "baz" "quz" "bar3" "bar4" "foo")
                  :key #'key))))
    (expect (equal '("foo" "bar1" "bar2" "baz" "quz" "bar3" "bar4" "foo")
                   (reverse tested))))

  (let ((tested nil))
    (flet ((key (str)
             (push str tested)
             (length str)))
      (expect (equal '("foo" "bar1" "bar2" "baz" "bar3" "foo")
                 (list:delete-adjacent
                  (list "foo" "bar1" "bar2" "baz" "quz" "bar3" "bar4" "foo")
                  :key #'key :start 2))))
    (expect (equal '("bar2" "baz" "quz" "bar3" "bar4" "foo")
                   (reverse tested))))

  (let ((tested nil))
    (flet ((key (str)
             (push str tested)
             (length str)))
      (expect (equal '("foo" "bar1" "baz" "bar3" "bar4" "foo")
                 (list:delete-adjacent
                  (list "foo" "bar1" "bar2" "baz" "quz" "bar3" "bar4" "foo")
                  :key #'key :end 6))))
    (expect (equal '("foo" "bar1" "bar2" "baz" "quz" "bar3")
                   (reverse tested)))))

;;; COPY-IF

(deftest copy-if-test ()
  (expect (equal (list 0 2 4) (list:copy-if-not #'oddp (list 0 1 2 3 4 5))))
  (expect (equal (list 0 2 4) (list:copy-if-not #'oddp (list 0 2 4))))

  (flet ((oggp (x) (not (oddp x)))) ; masks the outer OGGP
    (expect (equal (list 0 2 4) (list:copy-if-not 'oggp (list 0 1 2 3 4 5))))
    (expect (equal (list 0 2 4) (list:copy-if-not 'oggp (list 0 2 4))))

    ;; using the local oggp.
    (expect (equal (list 1 3 5) (list:copy-if-not #'oggp (list 0 1 2 3 4 5))))
    (expect (null (list:copy-if-not #'oggp (list 0 2 4)))))

  (expect (equal (list 2 4) (list:copy-if-not #'oddp (list 0 1 2 3 4 5) :start 2)))
  (expect (equal (list 4) (list:copy-if-not #'oddp (list 0 2 4) :start 2)))

  (expect (equal (list 0 2) (list:copy-if-not #'oddp (list 0 2 3 4 5) :end 3)))
  (expect (equal (list 0 2 4) (list:copy-if-not #'oddp (list 0 2 4 5) :end 3)))

  (expect (null (list:copy-if #'oddp (list 0 2 4 5) :count 0)))
  (expect (null (list:copy-if #'oddp (list 0 2 4 5) :start 2 :end 2)))
  (expect (null (list:copy-if #'oddp (list 0 2 4 5) :count 0 :start 2 :end 3)))
  (expect (null (list:copy-if #'oddp (list 0 2 4 5) :count 1 :start 2 :end 3)))

  (expect (null (list:copy-if #'oddp (list 0 1 3 5) :count 0)))
  (expect (null (list:copy-if #'oddp (list 0 1 3 5) :start 0 :end 0)))
  (expect (null (list:copy-if #'oddp (list 0 1 3 5) :start 1 :end 1)))
  (expect (null (list:copy-if #'oddp (list 0 1 3 5) :start 2 :end 2)))
  (expect (null (list:copy-if #'oddp (list 0 1 3 5) :start 3 :end 3)))
  (expect (null (list:copy-if #'oddp (list 0 1 3 5) :start 4 :end 4)))
  (expect (null (list:copy-if #'oddp (list 0 1 3 5) :start 5 :end 5)))
  (expect (null (list:copy-if #'oddp (list 0 1 3 5) :count 0 :start 0 :end 1)))
  (expect (null (list:copy-if #'oddp (list 0 1 3 5) :count 0 :start 1 :end 2)))
  (expect (null (list:copy-if #'oddp (list 0 1 3 5) :count 0 :start 2 :end 3)))
  (expect (null (list:copy-if #'oddp (list 0 1 3 5) :count 0 :start 3 :end 4)))
  (expect (null (list:copy-if #'oddp (list 0 1 3 5) :count 0 :start 3 :end 5)))

  (expect (equal (list 1)
                 (list:copy-if #'oddp (list 0 1 3 5) :count 1 :start 0 :end 2)))
  (expect (equal (list 1)
                 (list:copy-if #'oddp (list 0 1 3 5) :count 1 :start 1 :end 2)))
  (expect (equal (list 3)
                 (list:copy-if #'oddp (list 0 1 3 5) :count 1 :start 2 :end 3)))
  (expect (equal (list 5)
                 (list:copy-if #'oddp (list 0 1 3 5) :count 1 :start 3 :end 5)))

  (expect (equal (list 0 2) (list:copy-if-not #'oddp (list 0 1 2 3 4 5) :count 2)))
  (let ((l (list 0 2 4)))
    (expect (not (eql l (list:copy-if-not #'oddp l))))
    (expect (equal l (list:copy-if-not #'oddp l)))
    (expect (equal (list 0 2 4) l)))

  (expect
   (equal (list 2 4)
          (list:copy-if-not #'oddp (list 0 1 2 3 4 5) :from-end t :count 2)))
  (expect
   (equal (list 4) (list:copy-if-not #'oddp (list 0 2 4) :from-end t :count 1)))

  (expect (equal (list 0 2 4) (list:copy-if #'evenp (list 0 1 2 3 4 5))))
  (expect (equal (list 0 2 4) (list:copy-if #'evenp (list 0 2 4))))

  (flet ((efenp (x) (not (efenp x)))) ; masks the outer efenp
    (expect (equal (list 0 2 4) (list:copy-if 'efenp (list 0 1 2 3 4 5))))
    (expect (equal (list 0 2 4) (list:copy-if 'efenp (list 0 2 4))))

    ;; using the local efenp.
    (expect (equal (list 1 3 5) (list:copy-if #'efenp (list 0 1 2 3 4 5))))
    (expect (equal (list ) (list:copy-if #'efenp (list 0 2 4))))))

(deftest copy-if-test-notinline ()
  ;; Test for the "unoptimized" version of list:copy-if-not(-not).
  ;; This for the case when #'list:copy-if-not is called instead of inlined.
  (declare (notinline list:copy-if-not list:copy-if))
  (expect (equal (list 0 2 4) (list:copy-if-not #'oddp (list 0 1 2 3 4 5))))
  (expect (equal (list 0 2 4) (list:copy-if-not #'oddp (list 0 2 4))))

  (flet ((oggp (x) (not (oddp x)))) ; masks the outer OGGP
    (expect (equal (list 0 2 4) (list:copy-if-not 'oggp (list 0 1 2 3 4 5))))
    (expect (equal (list 0 2 4) (list:copy-if-not 'oggp (list 0 2 4))))

    ;; using the local oggp.
    (expect (equal (list 1 3 5) (list:copy-if-not #'oggp (list 0 1 2 3 4 5))))
    (expect (null (list:copy-if-not #'oggp (list 0 2 4)))))

  (expect (equal (list 2 4) (list:copy-if-not #'oddp (list 0 1 2 3 4 5) :start 2)))
  (expect (equal (list 4) (list:copy-if-not #'oddp (list 0 2 4) :start 2)))

  (expect (equal (list 0 2) (list:copy-if-not #'oddp (list 0 2 3 4 5) :end 3)))
  (expect (equal (list 0 2 4) (list:copy-if-not #'oddp (list 0 2 4 5) :end 3)))

  (expect (null (list:copy-if #'oddp (list 0 2 4 5) :count 0)))
  (expect (null (list:copy-if #'oddp (list 0 2 4 5) :start 2 :end 2)))
  (expect (null (list:copy-if #'oddp (list 0 2 4 5) :count 0 :start 2 :end 3)))
  (expect (null (list:copy-if #'oddp (list 0 2 4 5) :count 1 :start 2 :end 3)))

  (expect (null (list:copy-if #'oddp (list 0 1 3 5) :count 0)))
  (expect (null (list:copy-if #'oddp (list 0 1 3 5) :start 0 :end 0)))
  (expect (null (list:copy-if #'oddp (list 0 1 3 5) :start 1 :end 1)))
  (expect (null (list:copy-if #'oddp (list 0 1 3 5) :start 2 :end 2)))
  (expect (null (list:copy-if #'oddp (list 0 1 3 5) :start 3 :end 3)))
  (expect (null (list:copy-if #'oddp (list 0 1 3 5) :start 4 :end 4)))
  (expect (null (list:copy-if #'oddp (list 0 1 3 5) :start 5 :end 5)))
  (expect (null (list:copy-if #'oddp (list 0 1 3 5) :count 0 :start 0 :end 1)))
  (expect (null (list:copy-if #'oddp (list 0 1 3 5) :count 0 :start 1 :end 2)))
  (expect (null (list:copy-if #'oddp (list 0 1 3 5) :count 0 :start 2 :end 3)))
  (expect (null (list:copy-if #'oddp (list 0 1 3 5) :count 0 :start 3 :end 4)))
  (expect (null (list:copy-if #'oddp (list 0 1 3 5) :count 0 :start 3 :end 5)))

  (expect (equal (list 1)
                 (list:copy-if #'oddp (list 0 1 3 5) :count 1 :start 0 :end 2)))
  (expect (equal (list 1)
                 (list:copy-if #'oddp (list 0 1 3 5) :count 1 :start 1 :end 2)))
  (expect (equal (list 3)
                 (list:copy-if #'oddp (list 0 1 3 5) :count 1 :start 2 :end 3)))
  (expect (equal (list 5)
                 (list:copy-if #'oddp (list 0 1 3 5) :count 1 :start 3 :end 5)))

  (expect (equal (list 0 2) (list:copy-if-not #'oddp (list 0 1 2 3 4 5) :count 2)))
  (let ((l (list 0 2 4)))
    (expect (not (eql l (list:copy-if-not #'oddp l))))
    (expect (equal l (list:copy-if-not #'oddp l)))
    (expect (equal (list 0 2 4) l)))

  (expect
   (equal (list 2 4)
          (list:copy-if-not #'oddp (list 0 1 2 3 4 5) :from-end t :count 2)))
  (expect
   (equal (list 4) (list:copy-if-not #'oddp (list 0 2 4) :from-end t :count 1)))

  (expect (equal (list 0 2 4) (list:copy-if #'evenp (list 0 1 2 3 4 5))))
  (expect (equal (list 0 2 4) (list:copy-if #'evenp (list 0 2 4))))

  (flet ((efenp (x) (not (efenp x)))) ; masks the outer efenp
    (expect (equal (list 0 2 4) (list:copy-if 'efenp (list 0 1 2 3 4 5))))
    (expect (equal (list 0 2 4) (list:copy-if 'efenp (list 0 2 4))))

    ;; using the local efenp.
    (expect (equal (list 1 3 5) (list:copy-if #'efenp (list 0 1 2 3 4 5))))
    (expect (equal (list ) (list:copy-if #'efenp (list 0 2 4))))))
