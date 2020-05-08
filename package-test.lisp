;;;;
;;;; Testing utilities related to the package system.
;;;;

(defpackage #:google.core.package-test.baz)

(defpackage #:google.core.package-test.baz.qux)

(ace.core:defpackage* #:google.core.package-test.sub
    (:use #:common-lisp))

(ace.core:defpackage* #:google.core.package-test.quaz
    (:use #:common-lisp))

(ace.core:defpackage* #:google.core.package-test
  (:use #:common-lisp
        #:google.test
        #:google.core.package)
  (:use-alias (#:foo #:google.test)
              (#:bar #:google.test)
              ;; Shortcut for (#:quaz #:google.core.package-test.quaz)
              #:google.core.package-test.quaz)
  (:use-namespace
   #:google.core.package-test
   #:google.core.package-test.baz))

(in-package #:google.core.package-test)

(defun qux::fun-in-baz.qux () "BAZ.QUX")

(foo:deftest test-fun-in-baz ()
  (expect (string= "BAZ.QUX" (QUX::fun-in-baz.qux))))

(foo:deftest test-package-alias ()
  "Uses FOO as an alias for GOOGLE.TEST"
  (expect t))

(foo:deftest test-add-alias ()
  "Tests that FOO can only be aliased to one package in google.core.package-test.baz."
  (add-alias :foo :google.test :google.core.package-test.baz)
  (expect-error (add-alias :foo :google.core.package :google.core.package-test.baz)))

(bar:deftest sub::test-sub-package ()
  "Test different ways to refer to google.core.package-test.sub."
  (expect (find-symbol "TEST-SUB-PACKAGE" (find-package "GOOGLE.CORE.PACKAGE-TEST.SUB")))
  (expect (eq 'google.core.package-test.sub::test-sub-package 'sub::test-sub-package)))

(bar:deftest quaz::test-quaz-package ()
  "Test different ways to refer to google.core.package-test.quaz."
  (expect (find-symbol "TEST-QUAZ-PACKAGE" (find-package "GOOGLE.CORE.PACKAGE-TEST.QUAZ")))
  (expect (eq 'google.core.package-test.quaz::test-quaz-package 'quaz::test-quaz-package)))
