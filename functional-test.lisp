;;; Test package for ace.core.functional.
;;;

(defpackage #:ace.core.functional-test
  (:use #:common-lisp
        #:ace.core.functional
        #:ace.core
        #:ace.test)
  (:import-from #:ace.core.functional
                ace.core.functional::ftype-declaration
                ace.core.functional::function-lambda-list
                ace.core.functional::simple-lambda-list))

(in-package :ace.core.functional-test)

(defun foo (list)
  (funcall (compose #'oddp #'car) list))

(defun not-foo (list)
  (funcall (compose #'not #'oddp #'car) list))

(deftest compose-test ()
  (expect (foo '(1 2)))
  (expect (not (foo '(2 1))))

  (expect (not-foo '(2 1)))
  (expect (not (not-foo '(1 2)))))

(defun compose-list (just-arg)
  (compose #'list just-arg))

(declaim (ftype (function (fixnum fixnum) fixnum) div) (inline div))
(defun div (x y)
  (declare (fixnum x y) (optimize (speed 1) (safety 3)))
  (/ x y))
;; This one uses the lambda list of the global function resolved.
(defun div3 (x y z)
  (declare (fixnum x y z) (optimize (speed 1) (safety 3)))
  (/ x y z))

(defun* div* (x &optional (y 2))
  (declare (self inline (fixnum &optional fixnum) fixnum))
  (/ x y))

(deftest compose-test2 ()
  (expect (equal '(4) (funcall (compose-list #'1+) 3)))
  (locally (declare (notinline compose) (optimize (safety 3)))
    (expect-error (funcall (compose #'list #'div) 1 1 1))))

(deftest compose-test3 ()
  (flet ((div (&rest args) (apply #'+ args)))
    (expect (equal '(3) (funcall (compose #'list #'div) 1 1 1)))
    (expect (equal '(3) (funcall (compose #'list 'div) 9 3)))
    (expect (equal '(1) (funcall (compose #'list 'div3) 6 3 2)))
    (locally (declare (notinline compose) (optimize (speed 1) (safety 3)))
      ;; DIV taken from the global environment.
      (expect-error (funcall (compose #'list 'div) 1 1 1)))))

(deftest compose-test4 ()
  (declare (optimize (speed 1) (safety 3)))
  (flet ((div (x) (1+ x)))
    (declare (ftype (function (fixnum) (values fixnum &optional)) div))
    (expect (equal '(2) (funcall (compose #'list #'div) 1)))
    (locally (declare (notinline compose))
      ;; DIV taken from the local environment.
      (expect-error (funcall (compose #'list #'div) 1 1 1)))
    ;; DIV taken from the global environment.
    (expect (equal '(3) (funcall (compose #'list 'div) 9 3)))))

(deftest compose-test5 ()
  (declare (optimize (speed 1) (safety 3)))
  (flet ((div (x) (1+ x)))
    (expect (equal '(2) (funcall (compose #'list #'div) 1)))
    (locally (declare (notinline compose))
      ;; DIV taken from the local environment.
      (expect-error (funcall (compose #'list #'div) 1 1 1)))
    ;; DIV taken from the global environment.
    (expect (equal '(3) (funcall (compose #'list 'div) 9 3)))))

(declaim (ftype (function (fixnum &optional fixnum) fixnum) divopt)
         (inline divopt))
(defun divopt (x &optional (y 2))
  (declare (fixnum x y) (optimize (speed 1) (safety 3)))
  (/ x y))

(defun* divopt* (x &optional (y 2 yp))
  (declare (self (fixnum &optional fixnum) (or null fixnum)))
  (and yp (/ x y)))

(declaim (ftype (function (fixnum &optional fixnum fixnum) fixnum) divopt3))
(defun divopt3 (x &optional (y 1) (z 2))
  (declare (fixnum x y z) (optimize (speed 1) (safety 3)))
  (/ x y z))

;; This function has FTYPE to derive the lambda-list.
(declaim (ftype (function (fixnum fixnum &optional fixnum) fixnum) divopt3+))
(defun divopt3+ (x y &optional (z 2))
  (declare (fixnum x y z) (optimize (speed 1) (safety 3)))
  (/ x y z))

;; This function has no FTYPE to derive the lambda-list.
(defun divopt3* (x y &optional (z 2))
  (declare (fixnum x y z) (optimize (speed 1) (safety 3)))
  (/ x y z))

(deftest bind-test ()
  ;; fixed
  (expect (= 5 (funcall (bind 'div 10) 2)))
  (expect (= 5 (funcall (bind #'div 10) 2)))
  (expect (= 5 (funcall (bind #'div3 100 10) 2)))
  (expect (= 10 (funcall (funcall (bind #'constantly 10)))))
  (expect (= 10 (funcall (funcall (bind #'constantly) 10))))

  ;; optional
  (expect (= 5 (funcall (bind #'divopt 10) 2)))
  (expect (= 5 (funcall (bind #'divopt 10))))
  (expect (= 1 (funcall (bind #'divopt 10 10))))
  (expect (= 5 (funcall (bind #'divopt) 10)))
  (expect (= 5 (funcall (bind #'divopt) 10 2)))

  (expect (= 5 (funcall (bind #'divopt* 10) 2)))
  (expect (null (funcall (bind #'divopt* 10))))

  (expect (= 5 (funcall (bind #'divopt3 10))))
  (expect (= 5 (funcall (bind #'divopt3 20) 2)))
  ;; Test that with and without FTYPE this works correctly.
  (expect (= 5 (funcall (bind #'divopt3+ 10) 1)))
  (expect (= 5 (funcall (bind #'divopt3* 10) 1)))

  ;; rest
  (expect (= 5 (funcall (bind #'/ 10) 2)))
  (expect (= 3 (funcall (bind #'div 9) 3)))

  ;; error
  (expect-error (funcall (bind #'divopt3+ 10))))

(deftest ftype-test ()
  (expect (equal '(FUNCTION (FIXNUM FIXNUM) (VALUES FIXNUM &REST T))
                 (ftype-declaration 'div)))

  (let ((lexenv
         (handler-bind ((warning #'muffle-warning))
           (sb-cltl2:augment-environment
            nil :function '(foo)
                :declare `((ftype (function (fixnum string) t) foo))))))
    (expect (equal '(function (fixnum string) *)
                   (ftype-declaration 'foo lexenv)))))

(deftest function-lambda-list-test ()
  (expect (equal '(X &OPTIONAL (Y 2)) (function-lambda-list 'div*))))

(deftest simple-lambda-list-test ()
  (multiple-value-bind (ll args types)
      (simple-lambda-list '#'div)
    (expect (= 2 (length ll)))
    (expect (= 2 (length args)))
    (expect (= 2 (length types))))
  (multiple-value-bind (ll args types)
      (simple-lambda-list '#'div*)
    (expect (= 3 (length ll)))
    (expect (eq '&optional (second ll)))
    (expect (= 2 (length (third ll))))
    (expect (= 2 (length args)))
    (expect (= 2 (length types))))

  (multiple-value-bind (ll args types)
      (simple-lambda-list '#'divopt*)
    (expect (= 2 (length ll)))
    (expect (eq '&rest (first ll)))
    (expect (atom args))
    (expect (null types))))
