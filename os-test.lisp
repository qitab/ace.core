;;; Copyright 2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

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
