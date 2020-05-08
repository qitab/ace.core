;;;;
;;;; Tests for the google.list package.
;;;;

(google.core.package:defpackage* #:google.core.sequence-test
  (:use #:common-lisp
        #:google.test
        #:google.core.sequence)
  (:use-alias #:google.core.sequence))

(in-package #:google.core.sequence-test)

(deftest removef-test ()
  (let* ((c 0)
         (a (list 1 2 3 4))
         (%a a))
    (expect (equal '(2 3 4) (removef 1 a)))
    (expect (equal '(2 3 4) a))
    (expect (equal %a '(1 2 3 4)))

    (expect (equal '(4) (removef 3 (cdr a))))
    (expect (equal '(2 4) a))
    (expect (equal %a '(1 2 3 4)))
    (flet ((id (b) (incf c) b))
      (expect (equal '() (removef 4 (cdr (id a)))))
      (expect (equal '(2) a))
      (expect (equal %a '(1 2 3 4)))
      (expect (= c 1)))))

(deftest deletef-test ()
  (let* ((c 0)
         (a (list 1 2 3 4))
         (%a a))
    (expect (equal '(2 3 4) (deletef 1 a)))
    (expect (equal '(2 3 4) a))
    (expect (equal '(1 2 3 4) %a))

    (expect (equal '(4) (deletef 3 (cdr a))))
    (expect (equal '(2 4) a))
    (expect (equal '(1 2 4) %a))
    (flet ((id (b) (incf c) b))
      (expect (equal '() (deletef 4 (cdr (id a)))))
      (expect (equal '(2) a))
      (expect (equal '(1 2) %a))
      (expect (= c 1)))))

