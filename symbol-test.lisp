;;;;
;;;; Test for macro helpers
;;;;

(ace.core.package:defpackage* #:ace.core.symbol-test
  (:use #:common-lisp
        #:ace.test)
  (:use-alias (#:symbol #:ace.core.symbol)))

(in-package #:ace.core.symbol-test)

(deftest cat-test ()
  (let ((*package* (symbol-package 'cat-test)))
    (expect (eq 'test (symbol:cat t :test)))
    (expect (eq 'test-me (symbol:cat t :test :-me)))
    (expect (eq 'test-me* (symbol:cat t :test :-me #\*)))

    (expect (eq 'test (symbol:cat! :test)))
    (expect (eq 'test-me (symbol:cat! :test :-me)))
    (expect (eq 'test-me* (symbol:cat! :test :-me #\*)))

    (expect (eq :test (symbol:cat* :test)))
    (expect (eq :test-me (symbol:cat* :test :-me)))
    (expect (eq :test-me* (symbol:cat* :test :-me #\*)))

    (expect (eq 'test-me (symbol:cat$ :test- 'me)))
    (expect (eq 'test-me* (symbol:cat$ :test '-me #\*)))

    (expect (null (symbol-package (symbol:cat nil :test))))))

(deftest test-format! ()
  (expect (eq 'test (symbol:format! "TEST")))
  (expect (eq 'test-symbol (symbol:format! "TEST-~A" 'symbol)))
  (expect (eq 'test-a-b-c (symbol:format! "TEST~{-~A~}" '(a b c)))))

(deftest test-format* ()
  (expect (eq 'symbol::test-format* (symbol:format* "TEST-~A" 'symbol:format*)))
  (expect (eq 'test-a-b-fixnum-d (symbol:format* "TEST~A~{-~A~}" '-a '(b fixnum #:|D|)))))

