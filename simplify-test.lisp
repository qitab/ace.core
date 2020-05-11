;;; Test for the optimize module.
;;;

(defpackage #:ace.core.simplify-test
  (:use #:common-lisp
        #:ace.core.simplify
        #:ace.test)
  (:import-from
   #:ace.core.simplify
   ace.core.simplify::folded-form))

(in-package :ace.core.simplify-test)

(deftest folded-from-test ()
  (expect (equal '3 (folded-form '(+ 1 2))))
  (expect (equal '(+ a 2) (folded-form '(+ a 2))))
  (expect (equal 'function (folded-form 'function)))
  (expect (equal ''function (folded-form ''function)))
  (expect (equal ''function (folded-form '(identity 'function)))))

(deftest simplify-function-test ()
  (expect (eql '6 (simplify '(+ 1 2 3)))))

(defvar *special* 1)

(deftest simplify-let-test ()
  (expect (eql 1 (simplify '(let ((a 1)) a))))
  (expect (eql 1 (simplify '(let* ((a 1) (b a)) b))))
  ;; C could be a symbol-macrolet capturing a.
  (expect (equal '(let ((a 1)) c)
                 (simplify '(let ((a 1)) c))))
  (expect (eql -1 (simplify '(let ((a 1)) (+ a 2) (- a 2)))))
  (expect (eql 3 (simplify '(let ((a 1)) (declare (fixnum a)) (+ a 2)))))
  (expect (eql nil (simplify '(let ((a 1)) (declare (ignore a))))))

  ;; TODO(czak): Determine that no capture variables are left in LET subform.
  (expect (equal '(let ((a *special*)) (the fixnum a))
                 (simplify '(let ((a *special*)) (the fixnum a)))))
  (expect (equal '(let ((a *special*))
                   (declare (string a))
                   (the fixnum a))
                 (simplify '(let ((a *special*))
                                  (declare (string a))
                                  (the fixnum a)))))

  ;; TODO(czak): The type information has been erased here.
  (expect (equal '*special*
                 (simplify '(let ((a *special*))
                                  (declare (fixnum a))
                                  (the fixnum a)))))

  (expect (equal '(lambda (x) x)
                 (simplify '(let ((a (lambda (x) x))) a))))

  (expect (equal '(lambda (x) x)
                 (simplify '(let () (lambda (x) x)))))

  (expect (eql 1 (simplify '(let* ((a 1) (b a)) b))))
  (expect (eql 2 (simplify '(let* ((a 1) (b a) (c (+ a b))) c))))
  (expect (eql 3 (simplify '(let* ((a 1) (b (1+ a)) (c (+ a b))) c))))
  (expect (eql 10 (simplify '(let ((a 1)) c) nil '((c . 10)))))

  ;; Cannot flush (a b) binding without knowing b to be a variable.
  (expect (eql '(let ((a b)) c)
               (simplify '(let ((a b)) c) nil '((c . 10)))))
  ;; *special* is a known special variable here.
  (expect (eql 10 (simplify '(let ((a *special*)) c) nil '((c . 10))))))

(deftest simplify-if-form-test ()
  (expect (eq :bar (simplify '(if (< (+ 1 2) 0) :foo :bar))))
  (expect (eq :foo (simplify '(if (> (+ 1 2) 0) :foo :bar))))
  (expect (equal '(if (> (+ 1 b) 0) :foo :bar)
                 (simplify '(if (> (+ 1 b) 0) :foo :bar)))))

(deftest simplify-or-test()
  (expect (eq 'nil (simplify '(or))))
  (expect (eq '(f) (simplify '(or (f)))))
  (expect (eq 1 (simplify '(or 1 2))))
  (expect (equal 1 (simplify '(or 1 b))))
  (expect (equal '(or a 2) (simplify '(or a 2))))
  (expect (equal '(or (f) 2) (simplify '(or (f) 2))))
  (expect (equal 2 (simplify '(or a b) nil '((a . nil) (b . 2)))))
  (expect (equal 2 (simplify '(or a b c) nil '((a . nil) (b . 2)))))
  (expect (equal '(or x 2) (simplify '(or x a b c) nil '((a . nil) (b . 2)))))
  ;; Cannot inline a as "(cc)" since it is a binding.
  (expect (equal '(or x a 2) (simplify '(or x a b c) nil '((a . (cc)) (b . 2)))))
  (expect (equal '(or x #'+) (simplify '(or x a b c) nil '((a . #'+) (b . 2)))))
  (expect (eq '#'+ (simplify '(or #'+ #'-))))
  (expect (eq '#'- (simplify '(or nil #'-))))
  (expect (equal '(or (f) (lambda (x) x)) (simplify '(or (f) (lambda (x) x) #'-))))
  (expect (equal '(or a (lambda (x) x)) (simplify '(or a (lambda (x) x) #'-)))))

(deftest simplify-and-test ()
  (expect (eq 't (simplify '(and))))
  (expect (eq 't (simplify '(and t))))
  (expect (eq '(f) (simplify '(and (f)))))
  (expect (eq '#'- (simplify '(and #'+ #'-))))
  ;; This is what the macro expander does to AND.
  (expect (equal '(if (f) #'- nil) (simplify '(and (f) #'-))))
  (expect (equal '(if (if (f) (lambda (x) x) nil) #'- nil)
                 (simplify '(and (f) (lambda (x) x) #'-))))
  (expect (equal '(if (if (f) nil nil) #'- nil)
                 (simplify '(and t (f) nil #'-))))
  (expect (equal '(if (if (f) nil nil) #'- nil)
                 (simplify '(and 1 t 2 #'+ (f) nil #'-))))
  (expect (eq nil (simplify '(and nil #'-))))
  (expect (equal '#'- (simplify '(and (lambda (x) x) #'-))))
  (expect (equal '(lambda (x) x) (simplify '(and #'- (lambda (x) x)))))
  (expect (equal '(if (f) (lambda (x) x) nil) (simplify '(and #'- (f) (lambda (x) x))))))

(deftest simplify-complement ()
  (expect (eq 'lambda (car (simplify '(coerce (complement #'+) 'function))))))

(deftest simplify-constantly ()
  (expect (eq 'lambda (car (simplify '(coerce (constantly t) 'function))))))

(declaim (function *fun*))
(defvar *fun* #'+)

(deftest simplify-coerce-to-function-test ()
  (expect (equal '#'+ (simplify '(coerce #'+ 'function))))
  (expect (equal '#'+ (simplify '(coerce '+ 'function))))
  (expect (equal '#'+ (simplify '(coerce (or #'+ #'-) 'function))))
  (expect (equal '(coerce 'foo 'function)
                 (simplify '(coerce 'foo 'function))))
  (expect (equal '(lambda (x) x)
                 (simplify '(coerce (lambda (x) x) 'function))))
  (expect (equal '(lambda (x) x)
                 (simplify '(coerce (or (lambda (x) x) #'identity) 'function))))
  (expect (equal '*fun* (simplify '(coerce *fun* 'function)))))

(deftest simplify-the-test ()
  (expect (eql '1 (simplify '(the t 1))))
  (expect (eq 'a (simplify '(the t a))))
  (expect (eql '1 (simplify '(the fixnum 1))))
  (expect (equal '(the fixnum (f)) (simplify '(the fixnum (f)))))
  (expect (eql '1 (simplify '(let ((a 1)) (declare (fixnum a)) (the fixnum a)))))
  (expect (eql '"A" (simplify '(the string "A"))))
  (expect (equal '(the string 1) (simplify '(the string 1))))
  (expect (equal '(the string 1) (simplify '(the string (the string 1)))))

  (expect (equal '(the simple-string 1) (simplify '(the string (the simple-string 1)))))
  (expect (equal '(the simple-string 1) (simplify '(the simple-string (the string 1)))))
  (expect (equal '(the (and string double-float) 1)
                 (simplify '(the string (the double-float 1)))))

  (expect (equal '#'+ (simplify '(the function #'+))))
  ;; Don't coerce CL symbols.
  (expect (equal '(the function '+) (simplify '(the function '+))))
  (expect (equal '(the function (f)) (simplify '(the function (f)))))
  (expect (equal '*fun* (simplify '(the function *fun*))))
  (expect (equal '(the function *special*) (simplify '(the function *special*)))))
