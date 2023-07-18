;;; Copyright 2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

;;; Test for ace.core.enum.
;;;

(defpackage #:ace.core.enum-test
  (:use #:ace.core.enum #:ace.test #:cl))

(in-package #:ace.core.enum-test)

(define (enum1 :prefix :my-enum1)
  :lazy :developers :are :cool)

(define (enum2 :prefix :my-enum2)
  3 (:e3)
  (:e4 :name "some-option")
  1 :e1
  (2 :e2))

(define (test-enum-prefix :prefix :default-prefix)
    :one :two (:three)
    (:four :prefix "STRING-PREFIX")
    (:five :prefix :prefix5)
    (7 :seven :prefix prefix7)
    9 (:nine :prefix :prefix9)
    11 (:eleven :name "CamelCaseEleven"))

(deftest enum-prefix-test ()
  (expect (= +default-prefix-one+ 1))
  (expect (= +default-prefix-two+ 2))
  (expect (= +default-prefix-three+ 3))
  (expect (= +string-prefix-four+ 4))
  (expect (= +prefix5-five+ 5))
  (expect (= +prefix7-seven+ 7))
  (expect (= +prefix9-nine+ 9))
  (expect (= +default-prefix-eleven+ 11)))

(define (alias-enum :prefix :alias :allow-aliases t)
    1 :one 2 :two 3 :three
    1 :uno 2 :dos 3 :tres)

(deftest enum-aliases-test ()
  (expect (= +alias-one+ 1))
  (expect (= +alias-two+ 2))
  (expect (= +alias-three+ 3))
  (expect (= +alias-uno+ 1))
  (expect (= +alias-dos+ 2))
  (expect (= +alias-tres+ 3)))

(deftest test-emum-no-alias ()
  (expect-macro-warning (define no-alias 1 :one 1 :uno)))

(deftest to-numeral-test ()
  (expect (= 0 (enum2-to-numeral nil)))
  (expect (= 1 (enum2-to-numeral :e1)))
  (expect (= 2 (enum2-to-numeral :e2)))
  (expect (= 3 (enum2-to-numeral :e3)))
  (expect (= 4 (enum2-to-numeral :e4)))
  (expect (= 0 (enum2-to-numeral :e5))))
(deftest to-numeral-test-not-inline ()
  (declare (notinline enum2-to-numeral))
  (expect (= 0 (enum2-to-numeral nil)))
  (expect (= 1 (enum2-to-numeral :e1)))
  (expect (= 2 (enum2-to-numeral :e2)))
  (expect (= 3 (enum2-to-numeral :e3)))
  (expect (= 4 (enum2-to-numeral :e4)))
  (expect (= 0 (enum2-to-numeral :e5))))

(deftest to-keyword-test ()
  (expect (eq nil (enum2-to-keyword 0)))
  (expect (eq :e1 (enum2-to-keyword 1)))
  (expect (eq :e2 (enum2-to-keyword 2)))
  (expect (eq :e3 (enum2-to-keyword 3)))
  (expect (eq :e4 (enum2-to-keyword 4)))
  (expect (eq nil (enum2-to-keyword 5))))
(deftest to-keyword-test-notinline ()
  (declare (notinline enum2-to-keyword))
  (expect (eq nil (enum2-to-keyword 0)))
  (expect (eq :e1 (enum2-to-keyword 1)))
  (expect (eq :e2 (enum2-to-keyword 2)))
  (expect (eq :e3 (enum2-to-keyword 3)))
  (expect (eq :e4 (enum2-to-keyword 4)))
  (expect (eq nil (enum2-to-keyword 5))))

(define enum-with-nil
  (1 :one)
  (2 :two)
  (3 nil)
  (4 :four))

(deftest enume-with-nil-test ()
  (expect (= 3 (enum-with-nil-to-numeral :bar)))
  (expect (eq nil (enum-with-nil-to-keyword 3)))
  (expect (not (typep nil 'enum1)))
  (expect (typep nil 'enum-with-nil)))

(deftest enume-with-nil-test-notinline ()
  (declare (notinline enum-with-nil-to-numeral
                      enum-with-nil-to-keyword))
  (expect (= 3 (enum-with-nil-to-numeral :bar)))
  (expect (eq nil (enum-with-nil-to-keyword 3))))
