;;; Copyright 2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

;;;;
;;;; Tests the type utilities.
;;;;

(defpackage #:ace.core.type-test
  (:use #:common-lisp
        #:ace.core
        #:ace.core.type
        #:ace.test))

(in-package :ace.core.type-test)

(deftype fx () 'fixnum)
(deftype u3 () '(unsigned-byte 3))

(deftype-list-of fx)
(deftype-list-of u3)

(eval-always
 (setf (get 'u3 'info)
       (make-info :name 'u3
                  :constructor-form 0
                  :compare '=))
 (setf (get 'fx 'info)
       (make-info :name 'test-fixnum
                  :constructor-form 0
                  :compare '=)))

(deftest test-list-of ()
  (expect (typep '(1 2 3 4) '(list-of fx)))
  (expect (not (typep '(1 a 3) 'list-of-fx)))

  (expect (typep '(1 2 3) '(list-of u3)))
  (expect (not (typep '(1 -2 3) 'list-of-u3)))
  (expect (not (typep '(1 2 13) 'list-of-u3)))

  (expect (typep '(1 2 3) '(list-of (unsinged 8)))))

(declaim (type fixnum *special*))
(defvar *special* 0)

(declaim (type (mod 8) *special8*))
(defvar *special8* 1)

(deftest test-specialp ()
  (let (not-special)
    (expect (null not-special))
    (expect (specialp '*special*))
    (expect (not (specialp 'not-special)))))

(deftest test-declaimed ()
  (expect (eq (declaimed '*special*) 'fixnum)))

(deftest test-unknown-type ()
  (expect (not (unknownp 'fixnum)))
  (expect (unknownp 'fixfum)))

;; TODO(czak): Add tests for expand, element-of-array, add-null, ...

(deftest limits-test ()
  (macrolet ((test (limits form)
               `(expect (equal ',limits (multiple-value-list (limits ',form))))))
    (test (0 255) (unsigned-byte 8))
    (test (-128 127) (signed-byte 8))
    (test (0 1) bit)
    (test (-128 1023) (or (signed-byte 8) (mod 1024)))
    (test (1 1) (eql 1))
    (test (-222 333) (member -222 -111 0 333))
    (test (128 1023) (and (not (signed-byte 8)) (mod 1024)))))

(deftest bit-length-test ()
  (macrolet ((test (values form)
               `(expect (equal ',values (multiple-value-list (bit-length ',form))))))
    (test (8 unsigned-byte 0 255) (unsigned-byte 8))
    (test (8 signed-byte -128 127) (signed-byte 8))
    (test (10 unsigned-byte 10 1023) (integer 10 (1024)))
    (test (8 signed-byte -127 10) (integer -127 10))
    (test (7 signed-byte -63 10) (integer (-64) 10))
    (test (1 unsigned-byte 0 1) bit)))

(deftest bit-length-test2 ()
  (macrolet ((test (values form)
               `(expect (equal ',values
                               (multiple-value-list
                                (bit-length ',form :representation :relative))))))
    (test (8 unsigned-byte 0 255) (unsigned-byte 8))
    (test (8 signed-byte -128 127) (signed-byte 8))
    (test (10 unsigned-byte 10 1023) (integer 10 (1024)))
    (test (10 unsigned-byte 10 1024) (integer 10 1024))
    (test (7 signed-byte -117 10) (integer -117 10))
    (test (6 signed-byte -53 10) (integer (-54) 10))
    (test (1 unsigned-byte 0 1) bit)))

(deftype nil-type () '(member nil))
(deftype foo-type () '(member :foo))
(deftype quaz-type () '(member :quaz))
(deftype foo-bar () '(member :foo :bar))
(deftype foo-bar-baz () '(member :foo :bar :baz))
(deftype foo-bar-baz* () '(or null (member :foo :bar :baz)))
(deftype baz-foo-bar-baz* () '(or null (member :baz :foo :bar :baz)))

(deftest bit-length-member-test ()
  "Test that bit-length returns the right bit length for member types.
Also, test that the elements are correct, not duplicated, and in right order."
  (flet ((members (type)
           "Returns the member elements of the TYPE."
           (multiple-value-bind (len type members) (bit-length type)
             (if len
                 (expect (eq type 'member))
                 (expect (eq type nil)))
             members)))
    (expect (eq nil (bit-length 'nil)))
    (expect (eq nil (bit-length '(and))))
    (expect (= 1 (bit-length '(or (and) bit))))
    (expect (= 0 (bit-length 'null)))
    (expect (= 0 (bit-length '(or null))))
    (expect (= 0 (bit-length '(member nil))))
    (expect (= 0 (bit-length '(member :foo))))
    (expect (= 1 (bit-length '(member :foo :bar))))
    (expect (= 1 (bit-length '(member :foo :bar :foo))))
    (expect (= 2 (bit-length '(member :foo :bar :baz))))
    (expect (= 2 (bit-length '(member nil :foo :bar :baz))))

    (expect (= 0 (bit-length 'nil-type)))
    (expect (= 0 (bit-length 'foo-type)))
    (expect (= 1 (bit-length 'foo-bar)))
    (expect (= 1 (bit-length 'bit)))
    (expect (= 1 (bit-length '(or bit))))
    (expect (= 1 (bit-length '(and bit))))
    (expect (= 1 (bit-length 'boolean)))
    (expect (= 1 (bit-length '(or boolean))))
    (expect (= 2 (bit-length '(or boolean bit))))
    (expect (= 1 (bit-length '(or (and) boolean))))
    (expect (= 1 (bit-length '(and boolean))))
    (expect (= 1 (bit-length '(or (and boolean) boolean))))
    (expect (= 1 (bit-length '(or (and boolean bit) boolean))))
    (expect (= 1 (bit-length '(and (or boolean bit) boolean))))
    (expect (= 2 (bit-length 'foo-bar-baz)))
    (expect (= 2 (bit-length 'foo-bar-baz*)))

    (expect (equal 'nil (members 'nil)))
    (expect (equal 'nil (members '(and))))
    (expect (equal '(0 1) (members '(or (and) bit))))
    (expect (equal '(nil t) (members '(or (and) boolean))))
    (expect (equal '(nil t) (members '(or (and boolean) boolean))))
    (expect (equal '(nil t 0 1) (members '(or (and boolean) boolean bit))))
    (expect (equal '(nil t) (members '(and (or boolean bit) boolean))))
    (expect (equal '(nil t) (members '(and boolean (or boolean bit)))))
    (expect (equal '(nil) (members '(member nil))))
    (expect (equal '(nil) (members 'null)))
    (expect (equal '(nil) (members '(or null))))
    (expect (equal '(:foo) (members '(member :foo))))
    (expect (equal '(:foo :bar) (members '(member :foo :bar))))
    (expect (equal '(:foo :bar) (members '(member :foo :bar :foo))))
    (expect (equal '(:foo :bar :baz) (members '(member :foo :bar :baz))))
    (expect (equal '(nil :foo :bar :baz)
                   (members '(member nil :foo :bar :baz))))

    (expect (equal '(nil) (members 'nil-type)))
    (expect (equal '(:foo) (members 'foo-type)))
    (expect (equal '(:foo :bar) (members 'foo-bar)))
    (expect (equal '(:foo :bar :baz) (members 'foo-bar-baz)))
    (expect (equal '(nil :foo :bar :baz) (members 'foo-bar-baz*)))

    (expect (= 1 (bit-length '(or foo-type quaz-type))))
    (expect (= 1 (bit-length '(or quaz-type foo-type))))
    (expect (equal '(:foo :quaz) (members '(or foo-type quaz-type))))
    (expect (equal '(:quaz :foo) (members '(or quaz-type foo-type))))

    ;; TODO(czak): Maybe support this, too.
    (expect (null (bit-length '(or foo-type quaz-type (mod 10)))))

    (expect (= 2 (bit-length '(or foo-type bit))))
    (expect (= 2 (bit-length '(or bit foo-type))))
    (expect (equal '(:foo 0 1) (members '(or foo-type bit))))
    (expect (equal '(0 1 :foo) (members '(or bit foo-type))))

    (expect (= 2 (bit-length '(or foo-bar quaz-type))))
    (expect (= 2 (bit-length '(or quaz-type foo-bar))))
    (expect (equal '(:foo :bar :quaz) (members '(or foo-bar quaz-type))))
    (expect (equal '(:quaz :foo :bar) (members '(or quaz-type foo-bar))))

    (expect (eql nil (bit-length '(and foo-type quaz-type))))
    (expect (eql nil (bit-length '(and quaz-type foo-type))))

    (expect (= 1 (bit-length '(and foo-bar foo-bar-baz*))))
    (expect (= 1 (bit-length '(and foo-bar-baz* foo-bar))))
    (expect (equal '(:foo :bar) (members '(and foo-bar foo-bar-baz*))))
    (expect (equal '(:foo :bar) (members '(and foo-bar-baz* foo-bar))))

    (expect (= 1 (bit-length '(and foo-bar baz-foo-bar-baz*))))
    (expect (= 1 (bit-length '(and baz-foo-bar-baz* foo-bar))))
    (expect (equal '(:foo :bar) (members '(and foo-bar baz-foo-bar-baz*))))
    (expect (equal '(:foo :bar) (members '(and baz-foo-bar-baz* foo-bar))))

    (expect (= 2 (bit-length '(or foo-bar quaz-type (eql :baz) nil))))
    (expect (= 2 (bit-length '(or nil quaz-type (eql :baz) foo-bar))))
    (expect (equal '(:foo :bar :quaz :baz)
                   (members '(or foo-bar quaz-type (eql :baz) nil))))
    (expect (equal '(:quaz :baz :foo :bar)
                   (members '(or nil quaz-type (eql :baz) foo-bar))))

    (expect (= 2 (bit-length '(and (not null) foo-bar-baz*))))
    (expect (equal '(:foo :bar :baz) (members '(and (not null) foo-bar-baz*))))

    (expect (= 2 (bit-length '(and foo-bar-baz* (not null)))))
    (expect (equal '(:foo :bar :baz) (members '(and foo-bar-baz* (not null)))))))

(deftest upgraded-type-of-test ()
  (expect (eq 'string    (upgraded-type-of "TEST")))
  (expect (eq 'integer   (upgraded-type-of 1)))
  (expect (eq 'character (upgraded-type-of #\A)))
  (expect (eq 'list      (upgraded-type-of (list 1 2 3 4))))
  (expect (eq 'symbol    (upgraded-type-of 'test)))
  (expect (eq 'keyword   (upgraded-type-of :test))))

;;;
;;; Tests for FUNCTION-FORM-ARGUMENT-TYPES
;;;

(deftype foo () '(integer -23 42))

(defun* dummy-keyword-function1 (x &key y)
  (declare (self (foo &key fixnum) list))
  (when (plusp y) (list x y)))

(deftest test-function-form-argument-types1 ()
  (expect (equal '(foo keyword fixnum)
                 (function-form-argument-types '(dummy-keyword-function1 x :y z))))
  (assert-error
    (function-form-argument-types '(dummy-keyword-function1 x 1 :y z)))
  (assert-error
    (function-form-argument-types '(dummy-keyword-function1 x :y 2 :z z))))

(defun* dummy-keyword-function2 (x &key y &allow-other-keys)
  (declare (self (foo &key fixnum) list))
  (when (plusp y) (list x y)))

(deftest test-function-form-argument-types2 ()
  (expect (equal '(foo keyword fixnum)
                 (function-form-argument-types '(dummy-keyword-function2 x :y z))))
  (assert-error (function-form-argument-types '(dummy-keyword-function2 x 1 :y z)))
  (expect (equal '(foo keyword fixnum keyword nil)
                 (function-form-argument-types '(dummy-keyword-function2 x :y 2 :z z)))))

(defun* dummy-keyword-function3 (x &rest keys &key y &allow-other-keys)
  (declare (self (foo &rest t &key fixnum) list))
  (when keys (list x y)))

(deftest test-function-form-argument-types3 ()
  (expect (equal '(foo keyword fixnum)
                 (function-form-argument-types '(dummy-keyword-function3 x :y z))))
  (assert-error (function-form-argument-types '(dummy-keyword-function3 x 1 :y z)))
  (expect (equal '(foo keyword fixnum keyword t)
                 (function-form-argument-types '(dummy-keyword-function3 x :y 2 :z z)))))

(defun* dummy-keyword-function4 (x &optional y z &rest rest)
  (declare (self (foo &optional foo foo &rest fixnum) list))
  (when rest (list x y z)))

(deftest test-function-form-argument-types4 ()
  (expect (equal '(foo foo foo)
                 (function-form-argument-types '(dummy-keyword-function4 x :y z))))
  (expect (equal '(foo foo foo fixnum)
                 (function-form-argument-types '(dummy-keyword-function4 x :y z 2))))
  (expect (equal '(foo foo)
                 (function-form-argument-types '(dummy-keyword-function4 x :y)))))

(defmacro %form-typep (form type &environment env)
  (form-typep form type env))

(defmacro %know-form-typep (form type &environment env)
  (nth-value 1 (form-typep form type env)))

(defun* foo ()
  (declare (self () string))
  "foo")

(defun* baz8 ()
  (declare (self () (mod 8)))
  0)

(deftest form-typep-test ()
  (expect (form-typep 'undefined 't))
  (expect (not (nth-value 1 (form-typep 'undefined 'fixnum))))
  (expect (not (nth-value 1 (form-typep 'undefined '(mod 8)))))

  (expect (form-typep '(the fixnum undefined) 'fixnum))
  (expect (not (form-typep '(the fixnum undefined) 'string)))
  (expect (nth-value 1 (form-typep '(the fixnum undefined) 'string)))

  (expect (form-typep '(the (mod 8) undefined) '(mod 8)))
  (expect (form-typep '(the (mod 8) undefined) '(mod 16)))
  (expect (not (form-typep '(the (mod 8) undefined) 'string)))
  (expect (nth-value 1 (form-typep '(the (mod 8) undefined) 'string)))

  (expect (form-typep 10 'fixnum))
  (expect (form-typep 10 '(mod 16)))
  (expect (not (form-typep 10 '(mod 8))))
  (expect (not (form-typep 10 'string)))
  (expect (nth-value 1 (form-typep 10 'string)))

  (expect (form-typep '*special* 'fixnum))
  (expect (not (form-typep '*special* 'string)))
  (expect (nth-value 1 (form-typep '*special* 'string)))

  (expect (form-typep '*special8* 'fixnum))
  (expect (form-typep '*special8* '(mod 8)))
  (expect (form-typep '*special8* '(mod 16)))
  (expect (not (form-typep '*special8* '(mod 4))))
  (expect (not (form-typep '*special8* 'string)))
  (expect (nth-value 1 (form-typep '*special8* 'string)))

  (let ((count 0)
        (count8 0))
    (declare (fixnum count)
             (type (mod 8) count8)
             (ignore count count8))
    (expect (%form-typep count fixnum))
    (expect (not (%form-typep count string)))
    (expect (%know-form-typep count string))

    (expect (%form-typep count8 fixnum))
    (expect (%form-typep count8 (mod 8)))
    (expect (not (%form-typep count8 (mod 4))))
    (expect (%know-form-typep count8 (mod 4)))
    (expect (not (%form-typep count8 string)))
    (expect (%know-form-typep count8 string)))

  (expect (form-typep '(foo) 'string))
  (expect (not (form-typep '(foo) 'fixnum)))
  (expect (nth-value 1 (form-typep '(foo) '(mod 8))))
  (expect (not (form-typep '(foo) '(mod 8))))
  (expect (nth-value 1 (form-typep '(foo) '(mod 8))))

  (expect (form-typep '(baz8) '(mod 8)))
  (expect (form-typep '(baz8) '(mod 16)))
  (expect (not (form-typep '(baz8) '(mod 4))))
  (expect (nth-value 1 (form-typep '(baz8) '(mod 4))))
  (expect (not (form-typep '(baz8) 'string)))
  (expect (nth-value 1 (form-typep '(baz8) 'string)))

  (flet ((foo (x) (+ 1 x))
         (bar (x) (+ 0 x)))
    (declare (ftype (function (fixnum) (values fixnum)) foo)
             (ftype (function (fixnum) (values (mod 8))) bar))
    (expect (%form-typep (foo 1) fixnum))
    (expect (not (%form-typep (foo 1) string)))
    (expect (%know-form-typep (foo 1) string))

    (expect (%form-typep (bar 1) fixnum))
    (expect (%form-typep (bar 1) (mod 8)))
    (expect (not (%form-typep (bar 1) (mod 4))))
    (expect (%know-form-typep (bar 1) (mod 4)))
    (expect (not (%form-typep (bar 1) string)))
    (expect (%know-form-typep (bar 1) string))

    (foo (bar 1)))

  (expect (form-typep '(+ 10 10) 'fixnum))
  (expect (form-typep '(+ 10 10) '(mod 32)))
  (expect (not (form-typep '(+ 10 10) '(mod 16))))
  (expect (nth-value 1 (form-typep '(+ 10 10) '(mod 16))))

  (expect (form-typep '(+ 10 x) 'number))
  (expect (form-typep '(string x) 'string))
  (expect (not (form-typep '(string x) 'number)))
  (expect (nth-value 1 (form-typep '(string x) 'number)))

  (expect (equal '(NIL NIL)
                 (multiple-value-list
                  (form-typep '(foo-foo-foo x) 'string)))))
