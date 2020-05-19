;;; Copyright 2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

;;;;
;;;; Testing utilities related to the package system.
;;;;

(defpackage #:ace.core.package-test.baz)

(defpackage #:ace.core.package-test.baz.qux)

(ace.core:defpackage* #:ace.core.package-test.sub
    (:use #:common-lisp))

(ace.core:defpackage* #:ace.core.package-test.quaz
    (:use #:common-lisp))

(ace.core:defpackage* #:ace.core.package-test
  (:use #:common-lisp
        #:ace.test
        #:ace.core.package)
  (:use-alias (#:foo #:ace.test)
              (#:bar #:ace.test)
              ;; Shortcut for (#:quaz #:ace.core.package-test.quaz)
              #:ace.core.package-test.quaz)
  (:use-namespace
   #:ace.core.package-test
   #:ace.core.package-test.baz))

(in-package #:ace.core.package-test)

(defun qux::fun-in-baz.qux () "BAZ.QUX")

(foo:deftest test-fun-in-baz ()
  (expect (string= "BAZ.QUX" (QUX::fun-in-baz.qux))))

(foo:deftest test-package-alias ()
  "Uses FOO as an alias for ACE.TEST"
  (expect t))

(foo:deftest test-add-alias ()
  "Tests that FOO can only be aliased to one package in ace.core.package-test.baz."
  (add-alias :foo :ace.test :ace.core.package-test.baz)
  (expect-error (add-alias :foo :ace.core.package :ace.core.package-test.baz)))

(bar:deftest sub::test-sub-package ()
  "Test different ways to refer to ace.core.package-test.sub."
  (expect (find-symbol "TEST-SUB-PACKAGE" (find-package "ACE.CORE.PACKAGE-TEST.SUB")))
  (expect (eq 'ace.core.package-test.sub::test-sub-package 'sub::test-sub-package)))

(bar:deftest quaz::test-quaz-package ()
  "Test different ways to refer to ace.core.package-test.quaz."
  (expect (find-symbol "TEST-QUAZ-PACKAGE" (find-package "ACE.CORE.PACKAGE-TEST.QUAZ")))
  (expect (eq 'ace.core.package-test.quaz::test-quaz-package 'quaz::test-quaz-package)))
