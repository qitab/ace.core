;;; Copyright 2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

;;; Test for #:ace.core.with.

(defpackage #:ace.core.with-test
  (:use #:ace.core
        #:ace.core.with
        #:ace.test
        #:cl))

(in-package #:ace.core.with-test)

(defun init (&rest args) (values-list args))

(defparameter *cleanup-done* nil)

(defcleanup init (&rest vars)
  `(setf *cleanup-done* (list ,@vars)))

(deftest macroexpand-test ()
  (expect (macroexpand
           '(defcleanup init-one (foo)
             `(setf *cleanup-done* ,foo))))
  (expect (macroexpand
           '(defcleanup init (&rest vars)
             `(setf *cleanup-done* (list ,@vars)))))

  (expect (macroexpand
           '(with ((bar (init nil))
                   ((foo baz) (init :foo)))
             (setf body-executed t)))))

(deftest with-test ()
  (let ((*cleanup-done* :nope))
    (with ((foo (init :foo)))
      (expect (eq foo :foo)))
    (expect (equal *cleanup-done* '(:foo))))

  (let ((*cleanup-done* :nope)
        (body-executed nil))
    (with ((foo (init :foo))
           (bar (init nil)))
      (setf body-executed t))
    (expect (equal *cleanup-done* '(:foo)))
    (expect (not body-executed)))

  (let ((*cleanup-done* :nope)
        (body-executed nil))
    (with ((bar (init nil))
           (foo (init :foo)))
      (setf body-executed t))
    (expect (eq *cleanup-done* :nope))
    (expect (not body-executed)))

  (let ((*cleanup-done* :baz))
    (with (((foo bar) (init :foo :bar)))
      (expect (eq foo :foo))
      (expect (eq bar :bar)))
    (expect (equal *cleanup-done* '(:foo :bar))))

  (let ((*cleanup-done* :baz))
    (ignore-errors
     (with ((foo (init :foo)))
       (error "This is ignored.")))
    (expect (equal *cleanup-done* '(:foo)))))
