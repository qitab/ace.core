;;; Test for #:ace.core.os.

(defpackage #:ace.core.os-test
  (:use #:common-lisp
        #:ace.core.os
        #:ace.test))

(in-package #:ace.core.os-test)

(deftest cwd-test ()
  (expect (equal (namestring *default-pathname-defaults*) (cwd))))

(deftest program-name-test ()
  (expect (equal "os-test" (pathname-name (program-name)))))
