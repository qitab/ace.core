;;; Test for #:google.core.os.

(defpackage #:google.core.os-test
  (:use #:common-lisp
        #:google.core.os
        #:google.test))

(in-package #:google.core.os-test)

(deftest cwd-test ()
  (expect (equal (namestring *default-pathname-defaults*) (cwd))))

(deftest program-name-test ()
  (expect (equal "os-test" (pathname-name (program-name)))))
