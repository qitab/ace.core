;;; Copyright 2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

;;;
;;; Test package for TTY package.
;;;

(defpackage #:ace.core.tty-test
  (:use #:ace.core.tty #:ace.test #:common-lisp))

(in-package #:ace.core.tty-test)

(deftest test-ansi ()
  (format t "~31/ansi/~%" :TEST)
  (let ((*print-ansi* t))
    (expect
     (string= (format nil "~c[31mTEST~c[0m" #\ESC #\ESC)
              (format nil "~31/ansi/" :TEST)))))

(deftest test-ansi2 ()
  (format t "~30,47/ansi/~%" :TEST)
  (let ((*print-ansi* t))
    (expect
     (string= (format nil "~c[30;47mTEST~c[0m" #\ESC #\ESC)
              (format nil "~30,47/ansi/" :TEST)))))
