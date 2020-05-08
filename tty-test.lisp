;;;
;;; Test package for TTY package.
;;;

(defpackage #:google.core.tty-test
  (:use #:common-lisp #:google.test #:google.core.tty))

(in-package #:google.core.tty-test)

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
