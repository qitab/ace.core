;;;;
;;;; Tests for the fast ops fixnum package.
;;;; Note that most tests require both the optimized and unoptimized version.
;;;;

(google.core.package:defpackage* #:google.core.fx-test
  (:use #:common-lisp
        #:google.test)
  (:use-alias #:google.core.fx))

(cl:in-package #:google.core.fx-test)

;;;
;;; FX operations
;;;

(deftest test-fx-unary ()
  (expect (= 11 (fx:1+ 10)))
  (expect (=  9 (fx:1- 10))))

(deftest test-fx-binary1 ()
  (expect (equal '( 3  1) (multiple-value-list (fx:truncate  10  3))))
  (expect (equal '(-3  1) (multiple-value-list (fx:truncate  10 -3))))
  (expect (equal '(-3 -1) (multiple-value-list (fx:truncate -10  3))))
  (expect (equal '( 3 -1) (multiple-value-list (fx:truncate -10 -3))))

  (expect (equal '( 3  1) (multiple-value-list (fx:floor  10  3))))
  (expect (equal '(-4 -2) (multiple-value-list (fx:floor  10 -3))))
  (expect (equal '(-4  2) (multiple-value-list (fx:floor -10  3))))
  (expect (equal '( 3 -1) (multiple-value-list (fx:floor -10 -3))))

  (expect (equal '( 4 -2) (multiple-value-list (fx:ceiling  10  3))))
  (expect (equal '(-3  1) (multiple-value-list (fx:ceiling  10 -3))))
  (expect (equal '(-3 -1) (multiple-value-list (fx:ceiling -10  3))))
  (expect (equal '( 4  2) (multiple-value-list (fx:ceiling -10 -3)))))

(deftest test-fx-binary1b ()
  (declare (notinline fx:truncate fx:floor fx:ceiling))
  (expect (equal '( 3  1) (multiple-value-list (fx:truncate  10  3))))
  (expect (equal '(-3  1) (multiple-value-list (fx:truncate  10 -3))))
  (expect (equal '(-3 -1) (multiple-value-list (fx:truncate -10  3))))
  (expect (equal '( 3 -1) (multiple-value-list (fx:truncate -10 -3))))

  (expect (equal '( 3  1) (multiple-value-list (fx:floor  10  3))))
  (expect (equal '(-4 -2) (multiple-value-list (fx:floor  10 -3))))
  (expect (equal '(-4  2) (multiple-value-list (fx:floor -10  3))))
  (expect (equal '( 3 -1) (multiple-value-list (fx:floor -10 -3))))

  (expect (equal '( 4 -2) (multiple-value-list (fx:ceiling  10  3))))
  (expect (equal '(-3  1) (multiple-value-list (fx:ceiling  10 -3))))
  (expect (equal '(-3 -1) (multiple-value-list (fx:ceiling -10  3))))
  (expect (equal '( 4  2) (multiple-value-list (fx:ceiling -10 -3)))))

(deftest test-fx-binary2 ()
  (expect (=  0 (fx:mod 4 4)))
  (expect (=  3 (fx:mod 7 4)))
  (expect (=  3 (fx:mod -1 4)))
  (expect (= -1 (fx:mod -1 -4)))
  (expect (= -3 (fx:mod  1 -4)))

  (expect (=  0 (fx:rem 4 4)))
  (expect (=  3 (fx:rem 7 4)))
  (expect (= -1 (fx:rem -1 4)))
  (expect (= -1 (fx:rem -1 -4)))
  (expect (=  1 (fx:rem  1 -4))))


(deftest test-fx-binary2b ()
  (declare (notinline fx:mod fx:rem))
  (expect (=  0 (fx:mod 4 4)))
  (expect (=  3 (fx:mod 7 4)))
  (expect (=  3 (fx:mod -1 4)))
  (expect (= -1 (fx:mod -1 -4)))
  (expect (= -3 (fx:mod  1 -4)))

  (expect (=  0 (fx:rem 4 4)))
  (expect (=  3 (fx:rem 7 4)))
  (expect (= -1 (fx:rem -1 4)))
  (expect (= -1 (fx:rem -1 -4)))
  (expect (=  1 (fx:rem  1 -4))))

(deftest test-fx-binary3 ()
  (expect (= 2 (fx:ash 1 1)))
  (expect (= 2 (fx:logandc1 #xFD 7)))
  (expect (= 2 (fx:logandc2    7 #xFD))))

(deftest test-fx-binary3b ()
  (declare (notinline fx:ash fx:logandc1 fx:logandc2))
  (expect (= 2 (fx:ash 1 1)))
  (expect (= 2 (fx:logandc1 #xFD 7)))
  (expect (= 2 (fx:logandc2    7 #xFD))))

(deftest test-fx-n-ary1 ()
  (expect (= 0 (fx:+)))
  (expect (= 1 (fx:+ 1)))
  (expect (= 3 (fx:+ 1 2)))
  (expect (= 2 (fx:+ 1 -2 3)))

  (expect (= 1   (fx:*)))
  (expect (= 2   (fx:* 2)))
  (expect (= -6  (fx:* 2 -3)))
  (expect (= -30 (fx:* 2 -3 5))))


(deftest test-fx-n-ary1b ()
  (declare (notinline fx:+ fx:*))
  (expect (= 0 (fx:+)))
  (expect (= 1 (fx:+ 1)))
  (expect (= 3 (fx:+ 1 2)))
  (expect (= 2 (fx:+ 1 -2 3)))

  (expect (= 1   (fx:*)))
  (expect (= 2   (fx:* 2)))
  (expect (= -6  (fx:* 2 -3)))
  (expect (= -30 (fx:* 2 -3 5))))

(deftest test-fx-n-ary2 ()
  (expect (= 0 (fx:logior)))
  (expect (= 1 (fx:logior 1)))
  (expect (= 3 (fx:logior 1 2)))

  (expect (= 0 (fx:logxor)))
  (expect (= 1 (fx:logxor 1)))
  (expect (= 3 (fx:logxor 1 2)))
  (expect (= 0 (fx:logxor 1 2 3)))

  (expect (= -1 (fx:logand)))
  (expect (=  1 (fx:logand 1)))
  (expect (=  0 (fx:logand 1 2)))
  (expect (=  1 (fx:logand 1 3 5))))

(deftest test-fx-n-ary2b ()
  (declare (notinline fx:logior fx:logxor fx:logand))
  (expect (= 0 (fx:logior)))
  (expect (= 1 (fx:logior 1)))
  (expect (= 3 (fx:logior 1 2)))

  (expect (= 0 (fx:logxor)))
  (expect (= 1 (fx:logxor 1)))
  (expect (= 3 (fx:logxor 1 2)))
  (expect (= 0 (fx:logxor 1 2 3)))

  (expect (= -1 (fx:logand)))
  (expect (=  1 (fx:logand 1)))
  (expect (=  0 (fx:logand 1 2)))
  (expect (=  1 (fx:logand 1 3 5))))

(deftest test-fx-n1-ary1 ()
  (expect (= -1 (fx:- 1)))
  (expect (= -1 (fx:- 1 2)))
  (expect (= -4 (fx:- 1 2 3)))

  (expect (= 1 (fx:/ 1)))
  (expect (= 2 (fx:/ 2 1)))
  (expect (= 1 (fx:/ 6 3 2))))


(deftest test-fx-n1-ary1b ()
  (declare (notinline fx:- fx:/))
  (expect (= -1 (fx:- 1)))
  (expect (= -1 (fx:- 1 2)))
  (expect (= -4 (fx:- 1 2 3)))

  (expect (= 1 (fx:/ 1)))
  (expect (= 2 (fx:/ 2 1)))
  (expect (= 1 (fx:/ 6 3 2))))

;;;
;;; FX modifying macros
;;;

(deftest test-fx-mod1 ()
  (let ((place 0))
    (fx:decf place)
    (expect (= -1 place))
    (fx:incf place)
    (expect (zerop place))

    (fx:decf place 2)
    (expect (= -2 place))
    (fx:incf place 2)
    (expect (zerop place))

    (fx:maxf place)
    (expect (zerop place))
    (fx:minf place)
    (expect (zerop place))

    (fx:maxf place 2)
    (expect (= 2 place))
    (fx:minf place 0)
    (expect (zerop place))

    (fx:maxf place 2 1 4 place)
    (expect (= 4 place))
    (fx:minf place 0 2 place 5)
    (expect (zerop place))))

(deftest test-fx-mod2 ()
  (let ((place 1))
    (fx:ashf place 1)
    (expect (= 2 place))
    (fx:logandc2f place 2)
    (expect (zerop place))
    (fx:logandc1f place 1)
    (expect (= 1 place))

    (fx:logiorf place 2)
    (expect (= 3 place))
    (fx:logxorf place 2)
    (expect (= 1 place))
    (fx:logandf place 2)
    (expect (zerop place))))

;;;
;;; FX predicates
;;;

(deftest test-fx-unaryp ()
  (expect (fx:minusp -1))
  (expect (not (fx:minusp 0)))
  (expect (not (fx:minusp 1)))

  (expect (fx:plusp 10))
  (expect (not (fx:plusp 0)))
  (expect (not (fx:plusp -10)))

  (expect (fx:zerop 0))
  (expect (not (fx:zerop 1)))
  (expect (not (fx:zerop -1)))

  (expect (fx:oddp 1))
  (expect (not (fx:oddp 0)))

  (expect (fx:evenp 2))
  (expect (not (fx:evenp 1))))

(deftest test-fx-unaryp-b ()
  (declare (notinline fx:minusp fx:plusp fx:zerop fx:oddp fx:evenp))
  (expect (fx:minusp -1))
  (expect (not (fx:minusp 0)))
  (expect (not (fx:minusp 1)))

  (expect (fx:plusp 10))
  (expect (not (fx:plusp 0)))
  (expect (not (fx:plusp -10)))

  (expect (fx:zerop 0))
  (expect (not (fx:zerop 1)))
  (expect (not (fx:zerop -1)))

  (expect (fx:oddp 1))
  (expect (not (fx:oddp 0)))

  (expect (fx:evenp 2))
  (expect (not (fx:evenp 1))))

(deftest test-fx-binaryp ()
  (expect (fx:logbitp 1 3))
  (expect (not (fx:logbitp 2 3))))

(deftest test-fx-binaryp-b ()
  (declare (notinline fx:logbitp))
  (expect (fx:logbitp 1 3))
  (expect (not (fx:logbitp 2 3))))

(deftest test-fx-n-aryp ()
  (expect (fx:< 1))
  (expect (fx:< 1 2 3 4 5))
  (expect (not (fx:< 2 1)))
  (expect (not (fx:< 1 2 3 4 5 5 6 7)))

  (expect (fx:<= 1))
  (expect (fx:<= 1 2 3 4 5))
  (expect (fx:<= 1 2 3 3 4 5 6))
  (expect (not (fx:<= 2 1)))
  (expect (not (fx:<= 1 2 3 4 5 4 6 7)))

  (expect (fx:> 1))
  (expect (fx:> 5 4 3 2 1))
  (expect (not (fx:> 1 2)))
  (expect (not (fx:> 5 4 4 3 2 1)))

  (expect (fx:>= 1))
  (expect (fx:>= 5 4 3 2 1))
  (expect (fx:>= 6 5 4 3 3 2 1))
  (expect (not (fx:>= 1 2)))
  (expect (not (fx:>= 5 3 4 3 2 1)))

  (expect (fx:= 1))
  (expect (fx:= 1 1))
  (expect (not (fx:= 1 2 1)))

  (expect (fx:/= 1))
  (expect (fx:/= 1 2))
  (expect (not (fx:/= 1 1 1))))

(deftest test-fx-n-aryp-b ()
  (declare (notinline fx:< fx:<= fx:> fx:>= fx:= fx:/=))
  (expect (fx:< 1))
  (expect (fx:< 1 2 3 4 5))
  (expect (not (fx:< 2 1)))
  (expect (not (fx:< 1 2 3 4 5 5 6 7)))

  (expect (fx:<= 1))
  (expect (fx:<= 1 2 3 4 5))
  (expect (fx:<= 1 2 3 3 4 5 6))
  (expect (not (fx:<= 2 1)))
  (expect (not (fx:<= 1 2 3 4 5 4 6 7)))

  (expect (fx:> 1))
  (expect (fx:> 5 4 3 2 1))
  (expect (not (fx:> 1 2)))
  (expect (not (fx:> 5 4 4 3 2 1)))

  (expect (fx:>= 1))
  (expect (fx:>= 5 4 3 2 1))
  (expect (fx:>= 6 5 4 3 3 2 1))
  (expect (not (fx:>= 1 2)))
  (expect (not (fx:>= 5 3 4 3 2 1)))

  (expect (fx:= 1))
  (expect (fx:= 1 1))
  (expect (not (fx:= 1 2 1)))

  (expect (fx:/= 1))
  (expect (fx:/= 1 2))
  (expect (not (fx:/= 1 1 1))))
