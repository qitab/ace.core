;;; Tests the dynamic-extent utilities (DX package).
;;;

(ace.core:defpackage* #:ace.core.dx-test
  (:use #:common-lisp
        #:ace.test)
  (:use-alias #:ace.core.dx))

(in-package :ace.core.dx-test)

(deftest flet-test ()
  (dx:flet ((f (x) x))
    (expect (eql 4 (f 4))))

  (dx:flet ((f (x) x)
            (g () :a))
    (expect (eql :a (f (g)))))

  (dx:flet ((f (x) x)
            inline
            (g () :a))
    (expect (eql :a (f (g)))))

  (dx:flet (inline (f (x) x)
            (g () :a))
    (expect (eql :a (f (g)))))

  (dx:flet* ((f (x) x))
    (expect (eql 4 (f 4))))

  (dx:flet* ((f (x) x)
             (g () (f :a)))
    (expect (eql :a (f (g)))))

  (dx:flet* ((f (x) x)
             inline
             (g () (f :a)))
    (expect (eql :a (f (g)))))

  (dx:flet* (inline
             (f (x) x)
             (g () (f :a)))
    (expect (eql :a (f (g)))))

  (dx:labels ((f (x) x))
    (expect (eql 4 (f 4))))

  (dx:labels ((f (x) x)
              (g () (f :a)))
    (expect (eql :a (f (g)))))

  (dx:labels ((f (x) x)
              inline
              (g () (f :a)))
    (expect (eql :a (f (g)))))

  (dx:labels (inline
              (f (x) x)
              (g () (f :a)))
    (expect (eql :a (f (g))))))


(deftest let-test ()
  (expect (equal :a (dx:let () :a)))

  (dx:let ((a 1))
    (expect (eql 1 a)))

  (dx:let ((a 1)
           (b 2))
    (expect (eql 1 a))
    (expect (eql 2 b)))

  (expect (equal :a (dx:let* () :a)))

  (dx:let* ((a 1))
    (expect (eql 1 a)))

  (dx:let* ((a 1)
            (b (1+ a)))
    (expect (eql 1 a))
    (expect (eql 2 b))))


