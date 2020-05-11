;;;;
;;;; Tests for the strings package of basic string utilities.
;;;;

(cl:defpackage #:ace.core.string-test
  (:use #:common-lisp
        #:ace.core.string
        #:ace.test)
  (:import-from #:ace.core.collect #:with-collectors)
  (:import-from #:ace.core.string
                ;; For more readable macro-expansion.
                #:token
                #:%token-start
                #:%token-end
                #:%%delimit
                #:index?
                #:%string
                #:%start
                #:%end))

(cl:in-package #:ace.core.string-test)

;;;
;;; PREFIXP tests
;;;

(deftest prefix-test ()
  (expect (prefixp "" ""))
  (expect (prefixp "" "test"))
  (expect (prefixp "pre" "prefix"))
  (expect (not (prefixp "PRE" "prefix")))
  (expect (prefixp "prefix" "prefix"))
  (expect (prefixp "prefix" "prefix-test"))
  (expect (not (prefixp "preFIX" "prefix-test"))))

(deftest prefix-ignore-case-test ()
  (expect (prefixp "" "" :ignore-case t))
  (expect (prefixp "" "teST" :ignore-case t))
  (expect (prefixp "PRE" "prefix" :ignore-case t))
  (expect (prefixp "preFIX" "prefix" :ignore-case t))
  (expect (prefixp "prefix" "PREFIX-test" :ignore-case t)))

(deftest prefix-not-test ()
  (expect (not (prefixp "x" "")))
  (expect (not (prefixp "tast" "test")))
  (expect (not (prefixp "prefix+" "prefix")))
  (expect (not (prefixp "prefix+" "prefix-test"))))


(deftest prefix-start-test ()
  (expect (prefixp "" "" :start2 0))
  (expect (prefixp "" "test" :start2 0))
  (expect (prefixp "pre" "prefix" :start2 0))
  (expect (prefixp "prefix" "prefix" :start2 0))
  (expect (prefixp "prefix" "prefix-test" :start2 0))
  (expect (prefixp "" "test" :start2 2))
  (expect (prefixp "pre" "startprefix" :start2 5))
  (expect (prefixp "prefix" "startprefix" :start2 5))
  (expect (prefixp "prefix" "startprefix-test" :start2 5))

  (expect (prefixp "foo-" "" :start1 4 :start2 0))
  (expect (prefixp "foo-" "test" :start1 4 :start2 0))
  (expect (prefixp "foo-pre" "prefix" :start1 4 :start2 0))
  (expect (prefixp "foo-prefix" "prefix" :start1 4 :start2 0))
  (expect (prefixp "foo-prefix" "prefix-test" :start1 4 :start2 0))
  (expect (prefixp "foo-" "test" :start1 4 :start2 2))
  (expect (prefixp "foo-pre" "startprefix" :start1 4 :start2 5))
  (expect (prefixp "foo-prefix" "startprefix" :start1 4 :start2 5))
  (expect (prefixp "foo-prefix" "startprefix-test" :start1 4 :start2 5)))

(deftest prefix-not-start-test ()
  (expect (not (prefixp "x" "" :start2 0)))
  (expect (not (prefixp "tast" "test" :start2 0)))
  (expect (not (prefixp "prefix+" "prefix" :start2 0)))
  (expect (not (prefixp "prefix+" "prefix-test" :start2 0)))

  ;; Start2 is exceeding the length of the second argument.
  ;; This test is only done in the debug code.
  (expect-macro-warning (prefixp "t" "test" :start2 10))
  (expect-macro-warning (prefixp "test" "test" :start2 10))

  (expect-macro-warning (prefixp "x" "" :start2 1))
  (expect (not (prefixp "tast" "test" :start2 2)))
  (expect (not (prefixp "prefix+" "prefix" :start2 3)))
  (expect (not (prefixp "prefix+" "prefix-test" :start2 4))))

(deftest prefix-start-error-test ()
  (assert-error (eval '(prefixp "" "" :start2 -1))))

(deftest prefix-end-test ()
  (expect (prefixp "te" "test" :end2 2))
  (expect (prefixp "te" "test" :end2 4))
  ;; Start2 is exceeding the length of the second argument.
  ;; This test is only done in the debug code.
  (expect-macro-warning (prefixp "te" "test" :end2 10)))

(deftest prefix-not-end-test ()
  (expect (not (prefixp "te" "test" :end2 1)))
  (expect (not (prefixp "pre" "prefix" :end2 2)))
  (expect (not (prefixp "prefix" "prefix" :end2 5)))
  (expect (not (prefixp "prefix" "prefix-test" :end2 5))))

;;;
;;; SUFFIXP tests
;;;

(deftest suffixp-test ()
  (expect (suffixp "" ""))
  (expect (suffixp "" "teST"))
  (expect (suffixp "fix" "suffix"))
  (expect (not (suffixp "FIX" "suffix")))
  (expect (suffixp "suffix" "suffix"))
  (expect (suffixp "with" "ends-with"))
  (expect (not (suffixp "with" "ends-WITH"))))

(deftest suffixp-ignore-case-test ()
  (expect (suffixp "" "" :ignore-case t))
  (expect (suffixp "" "TEST" :ignore-case t))
  (expect (suffixp "FIX" "suffix" :ignore-case t))
  (expect (suffixp "suffix" "sufFIX" :ignore-case t))
  (expect (suffixp "with" "ends-WITH" :ignore-case t)))

(deftest suffixp-not-test ()
  (expect (not (suffixp "x" "")))
  (expect (not (suffixp "x" "test")))
  (expect (not (suffixp "xfix" "suffix")))
  (expect (not (suffixp "fixx" "suffix")))
  (expect (not (suffixp "xsuffix" "suffix")))
  (expect (not (suffixp "suffixx" "suffix")))
  (expect (not (suffixp "xwith" "ends-with")))
  (expect (not (suffixp "wiith" "ends-with"))))

(deftest suffixp-start-test ()
  (expect (suffixp "" "" :start2 0))
  (expect (suffixp "" "test" :start2 0))
  (expect (suffixp "fix" "suffix" :start2 0))
  (expect (suffixp "suffix" "suffix" :start2 0))
  (expect (suffixp "with" "ends-with" :start2 0))
  (expect (suffixp "" " " :start2 1))
  (expect (suffixp "" "test" :start2 2))
  (expect (suffixp "fix" "suffix" :start2 3))
  (expect (suffixp "suffix" "suffix" :start2 0))
  (expect (suffixp "with" "ends-with" :start2 4)))

(deftest suffixp-not-start-test ()
  (expect (not (suffixp "fix" "suffix" :start2 4)))
  (expect (not (suffixp "suffix" "suffix" :start2 1)))
  (expect (not (suffixp "with" "ends-with" :start2 6)))

  (expect (not (suffixp "x" "" :start2 0)))
  (expect (not (suffixp "x" "test" :start2 1)))
  (expect (not (suffixp "xfix" "suffix" :start2 2)))
  (expect (not (suffixp "fixx" "suffix" :start2 1)))
  (expect (not (suffixp "xsuffix" "suffix" :start2 1)))
  (expect (not (suffixp "suffixx" "suffix" :start2 0)))
  (expect (not (suffixp "xwith" "ends-with" :start2 1)))
  (expect (not (suffixp "wiith" "ends-with" :start2 3))))

(deftest suffixp-end-test ()
  (expect (suffixp "" "" :end2 0))
  (expect (suffixp "" "test" :end2 0))
  (expect (suffixp "fix" "suffix" :end2 6))
  (expect (suffixp "suffix" "suffix" :end2 6))
  (expect (suffixp "with" "ends-with" :end2 9))

  (expect (suffixp "" "x" :end2 0))
  (expect (suffixp "" "x" :end2 1))
  (expect (suffixp "" "test" :end2 4))
  (expect (suffixp "fix" "suffixend" :end2 6))
  (expect (suffixp "suffix" "suffixend" :end2 6))
  (expect (suffixp "with" "ends-withend" :end2 9)))

(deftest suffixp-not-end-test ()
  (expect (not (suffixp "x" "test" :end2 2)))
  (expect (not (suffixp "xfix" "suffix" :end2 5)))
  (expect (not (suffixp "fixx" "suffix" :end2 5)))
  (expect (not (suffixp "xsuffix" "suffix" :end2 1)))
  (expect (not (suffixp "suffixx" "suffix" :end2 1)))
  (expect (not (suffixp "xwith" "ends-with" :end2 1)))
  (expect (not (suffixp "wiith" "ends-with" :end2 7))))

(deftest suffixp-end-error-test ()
  (assert-error (eval '(suffixp "x" "" :end2 1))))


;;;
;;; Predicates
;;;

(deftest base-char-p-test ()
  (expect (every #'base-char-p "AaBbCcDdEeFfGgHhIiJjKkLlMmNnOoPpQqRrSsTtUuVvWwXxYyZz"))
  (expect (notany #'base-char-p "ΑαΒβΓγΔδΕεΖζΗηΘθΙιΚκΛλΜμΝνΞξΟοΠπΡρΣσΤτΥυΦφΧχΨψΩω")))

(deftest whitespace-p-test ()
  (expect (every #'whitespacep
                 '(#\Space #\Backspace #\Tab #\Newline #\Linefeed #\Page #\Return)))
  (expect (not (some #'whitespacep
                     "ABCSD215136346==.awt3.#$%@!$^!#$^\n\t25211sqw%#!@$YGDVQ#$%^&@^sfasgqe"))))

;;;
;;; String Split.
;;;


(deftest split-by-space-test ()
  (expect (equal '() (split nil)))
  (expect (equal '() (split "")))
  (expect (equal '() (split " ")))
  (expect (equal '() (split "  ")))

  (expect (equal '("A") (split "A")))
  (expect (equal '("A") (split "A ")))
  (expect (equal '("A") (split " A")))
  (expect (equal '("A") (split " A ")))

  (expect (equal '("AB") (split "AB")))
  (expect (equal '("AB") (split "AB ")))
  (expect (equal '("AB") (split " AB")))
  (expect (equal '("AB") (split " AB ")))

  (expect (equal '("ABC") (split "ABC")))
  (expect (equal '("ABC") (split "ABC ")))
  (expect (equal '("ABC") (split " ABC")))
  (expect (equal '("ABC") (split " ABC ")))

  (expect (equal '("A""B") (split "A B")))
  (expect (equal '("A""B") (split "A B ")))
  (expect (equal '("A""B") (split " A B")))
  (expect (equal '("A""B") (split " A B ")))

  (expect (equal '("A""B""C") (split "A B C")))
  (expect (equal '("A""B""C") (split "A B C ")))
  (expect (equal '("A""B""C") (split " A B C")))
  (expect (equal '("A""B""C") (split " A B C ")))

  (expect (equal '("A1""B1""C1") (split "A1 B1 C1")))
  (expect (equal '("A1""B1""C1") (split "A1 B1 C1 ")))
  (expect (equal '("A1""B1""C1") (split " A1 B1 C1")))
  (expect (equal '("A1""B1""C1") (split " A1 B1 C1 ")))

  (expect (equal '("A2""B2""C2") (split "A2  B2  C2")))
  (expect (equal '("A2""B2""C2") (split "A2  B2  C2  ")))
  (expect (equal '("A2""B2""C2") (split "  A2  B2  C2")))
  (expect (equal '("A2""B2""C2") (split "  A2  B2  C2  ")))

  (expect (equal '("A23""B23""C23") (split "A23   B23  C23")))
  (expect (equal '("A23""B23""C23") (split "A23   B23  C23   ")))
  (expect (equal '("A23""B23""C23") (split "   A23   B23  C23")))
  (expect (equal '("A23""B23""C23") (split "  A23   B23  C23   "))))

(deftest split-by-space2-test ()
  (expect (equal '() (split nil :by #\Space)))
  (expect (equal '() (split "" :by #\Space)))
  (expect (equal '() (split " " :by #\Space)))
  (expect (equal '() (split "  " :by #\Space)))

  (expect (equal '("A") (split "A" :by #\Space)))
  (expect (equal '("A") (split "A " :by #\Space)))
  (expect (equal '("A") (split " A" :by #\Space)))
  (expect (equal '("A") (split " A " :by #\Space)))

  (expect (equal '("AB") (split "AB" :by #\Space)))
  (expect (equal '("AB") (split "AB " :by #\Space)))
  (expect (equal '("AB") (split " AB" :by #\Space)))
  (expect (equal '("AB") (split " AB " :by #\Space)))

  (expect (equal '("ABC") (split "ABC" :by #\Space)))
  (expect (equal '("ABC") (split "ABC " :by #\Space)))
  (expect (equal '("ABC") (split " ABC" :by #\Space)))
  (expect (equal '("ABC") (split " ABC " :by #\Space)))


  (expect (equal '("A""B") (split "A B" :by #\Space)))
  (expect (equal '("A""B") (split "A B " :by #\Space)))
  (expect (equal '("A""B") (split " A B" :by #\Space)))
  (expect (equal '("A""B") (split " A B " :by #\Space)))

  (expect (equal '("A""B""C") (split "A B C" :by #\Space)))
  (expect (equal '("A""B""C") (split "A B C " :by #\Space)))
  (expect (equal '("A""B""C") (split " A B C" :by #\Space)))
  (expect (equal '("A""B""C") (split " A B C " :by #\Space)))

  (expect (equal '("A1""B1""C1") (split "A1 B1 C1" :by #\Space)))
  (expect (equal '("A1""B1""C1") (split "A1 B1 C1 " :by #\Space)))
  (expect (equal '("A1""B1""C1") (split " A1 B1 C1" :by #\Space)))
  (expect (equal '("A1""B1""C1") (split " A1 B1 C1 " :by #\Space)))

  (expect (equal '("A2""B2""C2") (split "A2  B2  C2" :by #\Space)))
  (expect (equal '("A2""B2""C2") (split "A2  B2  C2  " :by #\Space)))
  (expect (equal '("A2""B2""C2") (split "  A2  B2  C2" :by #\Space)))
  (expect (equal '("A2""B2""C2") (split "  A2  B2  C2  " :by #\Space)))

  (expect (equal '("A23""B23""C23") (split "A23   B23  C23" :by #\Space)))
  (expect (equal '("A23""B23""C23") (split "A23   B23  C23   " :by #\Space)))
  (expect (equal '("A23""B23""C23") (split "   A23   B23  C23" :by #\Space)))
  (expect (equal '("A23""B23""C23") (split "  A23   B23  C23   " :by #\Space))))


(deftest split-by-space-with-end-test ()
  (expect (equal '() (split nil :end 0)))
  (expect (equal '() (split "" :end 0)))
  (expect (equal '() (split " " :end 1)))
  (expect (equal '() (split "  " :end 1)))

  (expect (equal '("A") (split "A" :end 1)))
  (expect (equal '() (split "A" :end 0)))
  (expect (equal '("A") (split "A " :end 2)))
  (expect (equal '("A") (split "A " :end 1)))
  (expect (equal '("A") (split " A" :end 2)))
  (expect (equal '("A") (split " A " :end 2)))
  (expect (equal '() (split " A" :end 1)))
  (expect (equal '() (split " A " :end 1)))

  (expect (equal '("AB") (split "AB" :end 2)))
  (expect (equal '("AB") (split "AB " :end 2)))
  (expect (equal '("AB") (split " AB" :end 3)))
  (expect (equal '("AB") (split " AB " :end 3)))

  (expect (equal '("A") (split "AB" :end 1)))
  (expect (equal '("A") (split "AB " :end 1)))
  (expect (equal '("AB") (split " AB" :end 3)))
  (expect (equal '("AB") (split " AB " :end 3)))
  (expect (equal '("A") (split " AB " :end 2)))
  (expect (equal '() (split " AB " :end 1)))

  (expect (equal '("ABC") (split "ABC" :end 3)))
  (expect (equal '("ABC") (split "ABC " :end 3)))
  (expect (equal '("ABC") (split " ABC" :end 4)))
  (expect (equal '("ABC") (split " ABC " :end 4)))

  (expect (equal '("AB") (split "ABC" :end 2)))
  (expect (equal '("AB") (split "ABC " :end 2)))
  (expect (equal '("A") (split " ABC" :end 2)))
  (expect (equal '("A") (split " ABC " :end 2)))

  (expect (equal '("A" "B") (split "A B" :end 3)))
  (expect (equal '("A" "B") (split "A B " :end 3)))
  (expect (equal '("A" "B") (split " A B" :end 4)))
  (expect (equal '("A" "B") (split " A B " :end 4)))

  (expect (equal '("A") (split "A B" :end 2)))
  (expect (equal '("A") (split "A B " :end 2)))
  (expect (equal '("A") (split " A B" :end 3)))
  (expect (equal '("A") (split " A B " :end 3)))

  (expect (equal '("A" "B" "C") (split "A B C" :end 5)))
  (expect (equal '("A" "B" "C") (split "A B C " :end 5)))
  (expect (equal '("A" "B" "C") (split " A B C" :end 6)))
  (expect (equal '("A" "B" "C") (split " A B C " :end 6)))

  (expect (equal '("A" "B") (split "A B C" :end 3)))
  (expect (equal '("A" "B") (split "A B C " :end 4)))
  (expect (equal '("A" "B") (split " A B C" :end 5)))
  (expect (equal '("A") (split " A B C " :end 3)))
  (expect (equal '() (split " A B C " :end 1))))

(deftest split-by-space-with-start-test ()
  (expect (equal '() (split nil :start 0)))
  (expect (equal '() (split "" :start 0)))
  (expect (equal '() (split " " :start 1)))
  (expect (equal '() (split "  " :start 1)))

  (expect (equal '() (split "A" :start 1)))
  (expect (equal '("A") (split "A" :start 0)))
  (expect (equal '() (split "A " :start 2)))
  (expect (equal '() (split "A " :start 1)))
  (expect (equal '() (split " A" :start 2)))
  (expect (equal '() (split " A " :start 2)))
  (expect (equal '("A") (split " A" :start 1)))
  (expect (equal '("A") (split " A " :start 1)))

  (expect (equal '("AB") (split "AB" :start 0)))
  (expect (equal '("AB") (split "AB " :start 0)))
  (expect (equal '("AB") (split " AB" :start 1)))
  (expect (equal '("AB") (split " AB " :start 1)))

  (expect (equal '("B") (split "AB" :start 1)))
  (expect (equal '("B") (split "AB " :start 1)))
  (expect (equal '() (split " AB" :start 3)))
  (expect (equal '() (split " AB " :start 3)))
  (expect (equal '("B") (split " AB " :start 2)))
  (expect (equal '("AB") (split " AB " :start 1)))

  (expect (equal '("ABC") (split "ABC" :start 0)))
  (expect (equal '("ABC") (split "ABC " :start 0)))
  (expect (equal '() (split " ABC" :start 4)))
  (expect (equal '() (split " ABC " :start 4)))

  (expect (equal '("C") (split "ABC" :start 2)))
  (expect (equal '("C") (split "ABC " :start 2)))
  (expect (equal '("BC") (split " ABC" :start 2)))
  (expect (equal '("BC") (split " ABC " :start 2)))

  (expect (equal '() (split "A B" :start 3)))
  (expect (equal '() (split "A B " :start 3)))
  (expect (equal '("A" "B") (split " A B" :start 1)))
  (expect (equal '("A" "B") (split " A B " :start 0)))

  (expect (equal '("B") (split "A B" :start 2)))
  (expect (equal '("B") (split "A B " :start 2)))
  (expect (equal '("B") (split " A B" :start 3)))
  (expect (equal '("B") (split " A B " :start 3)))

  (expect (equal '("A" "B" "C") (split "A B C" :start 0)))
  (expect (equal '("A" "B" "C") (split "A B C " :start 0)))
  (expect (equal '() (split " A B C" :start 6)))
  (expect (equal '() (split " A B C " :start 6)))

  (expect (equal '("C") (split "A B C" :start 3)))
  (expect (equal '("C") (split "A B C " :start 4)))
  (expect (equal '("C") (split " A B C" :start 5)))
  (expect (equal '("B" "C") (split " A B C " :start 3)))
  (expect (equal '("A" "B" "C") (split " A B C " :start 1))))

(deftest split-with-start-end-adjusted ()
  (let* ((%string (make-array 15 :element-type 'base-char :adjustable t
                                 :initial-contents "ABBRA-KA-DA-BRA"))
         (string (make-array 9 :element-type 'base-char
                               :displaced-to %string :displaced-index-offset 6)))
    (expect (equal "KA-DA-BRA" string))
    (expect (equal '("A" "DA" "BRA") (split string :by #\- :start 1)))
    (expect (equal '("KA" "DA") (split string :by #\- :end 6)))))

(deftest split-with-empty-test ()
  (expect (equal '("A" "B" "C") (split "A,B,C" :by #\, :keep-empty t)))
  (expect (equal '("A" "" "C") (split "A,,C" :by #\, :keep-empty t)))
  (expect (equal '("" "B" "C") (split ",B,C" :by #\, :keep-empty t)))
  (expect (equal '("A" "B" "") (split "A,B," :by #\, :keep-empty t)))
  (expect (equal '("" "" "") (split ",," :by #\, :keep-empty t)))
  (expect (equal '("" "") (split "," :by #\, :keep-empty t)))
  (expect (equal '("") (split "" :by #\, :keep-empty t)))

  (expect (equal '("" "B" "C") (split "A,B,C" :by #\, :keep-empty t :start 1)))
  (expect (equal '("" "C") (split "A,,C" :by #\, :keep-empty t :start 2)))
  (expect (equal '("C") (split ",B,C" :by #\, :keep-empty t :start 3)))
  (expect (equal '("B" "") (split "A,B," :by #\, :keep-empty t :start 2)))
  (expect (equal '("" "" "") (split ",," :by #\, :keep-empty t :start 0)))
  (expect (equal '("") (split "," :by #\, :keep-empty t :start 1)))
  (expect (equal '("") (split "" :by #\, :keep-empty t :start 0)))

  (expect (equal '("A") (split "A,B,C" :by #\, :keep-empty t :end 1)))
  (expect (equal '("A" "") (split "A,,C" :by #\, :keep-empty t :end 2)))
  (expect (equal '("" "B" "") (split ",B,C" :by #\, :keep-empty t :end 3)))
  (expect (equal '("A" "") (split "A,B," :by #\, :keep-empty t :end 2)))
  (expect (equal '("" "") (split ",," :by #\, :keep-empty t :end 1)))
  (expect (equal '("" "") (split "," :by #\, :keep-empty t :end 1)))
  (expect (equal '("A") (split "A" :by #\, :keep-empty t :end 1)))
  (expect (equal '("") (split "" :by #\, :keep-empty t :end 0))))

(deftest split-with-count-test ()
  (expect (equal '() (split "A B C" :count 0)))
  (expect (equal '("A") (split "A B C" :count 1)))
  (expect (equal '("A" "B") (split "A B C" :count 2)))
  (expect (equal '("A" "B" "C") (split "A B C" :count 3)))
  (expect (equal '("A" "B" "C") (split "A B C" :count nil)))

  (expect (equal '() (split "  A23   B23  C23   " :count 0)))
  (expect (equal '("A23") (split "  A23   B23  C23   " :count 1)))
  (expect (equal '("A23" "B23") (split "  A23   B23  C23   " :count 2)))
  (expect (equal '("A23" "B23" "C23") (split "  A23   B23  C23   " :count 3)))
  (expect (equal '("A23" "B23" "C23") (split "  A23   B23  C23   " :count nil)))

  (expect (equal '() (split "  A23   B23  C23   " :count 0 :end 7)))
  (expect (equal '("A23") (split "  A23   B23  C23   " :count 1 :end nil)))
  (expect (equal '("A23") (split "  A23   B23  C23   " :count 1 :end 7)))
  (expect (equal '("A23") (split "  A23   B23  C23   " :count 2 :end 7)))
  (expect (equal '("A23" "B23") (split "  A23   B23  C23   " :count 2 :end nil)))
  (expect (equal '("A23") (split "  A23   B23  C23   " :count 3 :end 7)))
  (expect (equal '("A23") (split "  A23   B23  C23   " :count nil :end 7))))

(deftest split-with-count-and-keep-rest ()
  (expect (equal '("A B C") (split "A B C" :count 0 :keep-rest t)))
  (expect (equal '("A" "B C") (split "A B C" :count 1 :keep-rest t)))
  (expect (equal '("A" "B" "C") (split "A B C" :count 2 :keep-rest t)))
  (expect (equal '("A" "B" "C") (split "A B C" :count 3 :keep-rest t)))
  (expect (equal '("A" "B" "C") (split "A B C" :count nil :keep-rest t)))

  (expect (equal '("  A23   B23  C23   ") (split "  A23   B23  C23   " :count 0 :keep-rest t)))
  (expect (equal '("A23" "  B23  C23   ") (split "  A23   B23  C23   " :count 1 :keep-rest t)))
  (expect (equal '("A23" "B23" " C23   ") (split "  A23   B23  C23   " :count 2 :keep-rest t)))
  (expect (equal '("A23" "B23" "C23" "  ") (split "  A23   B23  C23   " :count 3 :keep-rest t)))
  (expect (equal '("A23" "B23" "C23") (split "  A23   B23  C23   " :count nil :keep-rest t)))

  (expect (equal '("  A23  ") (split "  A23   B23  C23   " :count 0 :keep-rest t :end 7)))
  (expect (equal '("A23" "  B23  C23   ")
                 (split "  A23   B23  C23   " :count 1 :keep-rest t :end nil)))
  (expect (equal '("A23" " ") (split "  A23   B23  C23   " :count 1 :keep-rest t :end 7)))
  (expect (equal '("A23" " ") (split "  A23   B23  C23   " :count 2 :keep-rest t :end 7)))
  (expect (equal '("A23" "B23" " C23   ")
                 (split "  A23   B23  C23   " :count 2 :keep-rest t :end nil)))
  (expect (equal '("A23" " ") (split "  A23   B23  C23   " :count 3 :keep-rest t :end 7)))
  (expect (equal '("A23") (split "  A23   B23  C23   " :count nil :keep-rest t :end 7))))

(deftest split-with-keep-empty-and-count ()
  (expect (equal '() (split "  A23   B23  C23   " :count 0 :keep-empty t)))
  (expect (equal '("") (split "  A23   B23  C23   " :count 1 :keep-empty t)))
  (expect (equal  '("" "") (split "  A23   B23  C23   " :count 2 :keep-empty t)))
  (expect (equal  '("" "" "A23") (split "  A23   B23  C23   " :count 3 :keep-empty t)))
  (expect (equal  '("" "" "A23" "" "" "B23" "" "C23" "" "" "")
                  (split "  A23   B23  C23   " :count nil :keep-empty t)))

  (expect (equal '() (split "  A23   B23  C23   " :count 0 :end 7 :keep-empty t)))
  (expect (equal  '("") (split "  A23   B23  C23   " :count 1 :end nil :keep-empty t)))
  (expect (equal  '("") (split "  A23   B23  C23   " :count 1 :end 7 :keep-empty t)))
  (expect (equal  '("" "") (split "  A23   B23  C23   " :count 2 :end 7 :keep-empty t)))
  (expect (equal  '("" "") (split "  A23   B23  C23   " :count 2 :end nil :keep-empty t)))
  (expect (equal  '("" "" "A23") (split "  A23   B23  C23   " :count 3 :end 7 :keep-empty t)))
  (expect (equal  '("" "" "A23" "" "")
                  (split "  A23   B23  C23   " :count nil :end 7 :keep-empty t))))

(deftest split-by-digit-test ()
  (let ((string (format nil "A~DB~DC~D" (random 100) (random 100) (random 100))))
    (expect (equal '("A" "B" "C") (split string :by #'digit-char-p)))
    (expect (equal '("A" "B" "C") (split string :by (lambda (c) (digit-char-p c)))))
    (expect (equal '("A" "B" "C") (split string :by 'digit-char-p)))
    (expect (equal '("A" "B" "C") (split string :by "0123456789")))
    (expect (equal '("A" "B" "C") (split string :by #(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))))
    (expect (equal '("A" "B" "C") (split string :by '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))))))

;;;
;;; String Tokenize.
;;;
;;; Note that this part only tests the return value.
;;; The SPLIT function uses TOKENIZE and already tests for the parts returned.
;;;

(defun nop (&rest ignore) (declare (ignore ignore)) nil)

(deftest tokenize-by-space-test ()
  (expect (= 0 (tokenize #'nop nil)))
  (expect (= 1 (tokenize #'nop "")))
  (expect (= 2 (tokenize #'nop " ")))
  (expect (= 3 (tokenize #'nop "  ")))

  (expect (= 2 (tokenize #'nop "A")))
  (expect (= 2 (tokenize #'nop "A ")))
  (expect (= 3 (tokenize #'nop " A")))
  (expect (= 3 (tokenize #'nop " A ")))

  (expect (= 3 (tokenize #'nop "AB")))
  (expect (= 3 (tokenize #'nop "AB ")))
  (expect (= 4 (tokenize #'nop " AB")))
  (expect (= 4 (tokenize #'nop " AB ")))

  (expect (= 4 (tokenize #'nop "ABC")))
  (expect (= 4 (tokenize #'nop "ABC ")))
  (expect (= 5 (tokenize #'nop " ABC")))
  (expect (= 5 (tokenize #'nop " ABC ")))


  (expect (= 4 (tokenize #'nop "A B")))
  (expect (= 4 (tokenize #'nop "A B ")))
  (expect (= 5 (tokenize #'nop " A B")))
  (expect (= 5 (tokenize #'nop " A B ")))

  (expect (= 6 (tokenize #'nop "A B C")))
  (expect (= 6 (tokenize #'nop "A B C ")))
  (expect (= 7 (tokenize #'nop " A B C")))
  (expect (= 7 (tokenize #'nop " A B C ")))

  (expect (= 9 (tokenize #'nop "A1 B1 C1")))
  (expect (= 9 (tokenize #'nop "A1 B1 C1 ")))
  (expect (= 10 (tokenize #'nop " A1 B1 C1")))
  (expect (= 10 (tokenize #'nop " A1 B1 C1 ")))

  (expect (= 11 (tokenize #'nop "A2  B2  C2")))
  (expect (= 11 (tokenize #'nop "A2  B2  C2  ")))
  (expect (= 13 (tokenize #'nop "  A2  B2  C2")))
  (expect (= 13 (tokenize #'nop "  A2  B2  C2  ")))

  (expect (= 15 (tokenize #'nop "A23   B23  C23")))
  (expect (= 15 (tokenize #'nop "A23   B23  C23   ")))
  (expect (= 18 (tokenize #'nop "   A23   B23  C23")))
  (expect (= 17 (tokenize #'nop "  A23   B23  C23   "))))

(deftest tokenize-by-space-with-end-test ()
  (expect (= 0 (tokenize #'nop nil :end 0)))
  (expect (= 1 (tokenize #'nop "" :end 0)))
  (expect (= 2 (tokenize #'nop " " :end 1)))
  (expect (= 2 (tokenize #'nop "  " :end 1)))

  (expect (= 2 (tokenize #'nop "A" :end 1)))
  (expect (= 1 (tokenize #'nop "A" :end 0)))
  (expect (= 2 (tokenize #'nop "A " :end 2)))
  (expect (= 2 (tokenize #'nop "A " :end 1)))
  (expect (= 3 (tokenize #'nop " A" :end 2)))
  (expect (= 3 (tokenize #'nop " A " :end 2)))
  (expect (= 2 (tokenize #'nop " A" :end 1)))
  (expect (= 2 (tokenize #'nop " A " :end 1)))

  (expect (= 3 (tokenize #'nop "AB" :end 2)))
  (expect (= 3 (tokenize #'nop "AB " :end 2)))
  (expect (= 4 (tokenize #'nop " AB" :end 3)))
  (expect (= 4 (tokenize #'nop " AB " :end 3)))

  (expect (= 2 (tokenize #'nop "AB" :end 1)))
  (expect (= 2 (tokenize #'nop "AB " :end 1)))
  (expect (= 4 (tokenize #'nop " AB" :end 3)))
  (expect (= 4 (tokenize #'nop " AB " :end 3)))
  (expect (= 3 (tokenize #'nop " AB " :end 2)))
  (expect (= 2 (tokenize #'nop " AB " :end 1)))

  (expect (= 4 (tokenize #'nop "ABC" :end 3)))
  (expect (= 4 (tokenize #'nop "ABC " :end 3)))
  (expect (= 5 (tokenize #'nop " ABC" :end 4)))
  (expect (= 5 (tokenize #'nop " ABC " :end 4)))

  (expect (= 3 (tokenize #'nop "ABC" :end 2)))
  (expect (= 3 (tokenize #'nop "ABC " :end 2)))
  (expect (= 3 (tokenize #'nop " ABC" :end 2)))
  (expect (= 3 (tokenize #'nop " ABC " :end 2)))

  (expect (= 4 (tokenize #'nop "A B" :end 3)))
  (expect (= 4 (tokenize #'nop "A B " :end 3)))
  (expect (= 5 (tokenize #'nop " A B" :end 4)))
  (expect (= 5 (tokenize #'nop " A B " :end 4)))

  (expect (= 2 (tokenize #'nop "A B" :end 2)))
  (expect (= 2 (tokenize #'nop "A B " :end 2)))
  (expect (= 3 (tokenize #'nop " A B" :end 3)))
  (expect (= 3 (tokenize #'nop " A B " :end 3)))

  (expect (= 6 (tokenize #'nop "A B C" :end 5)))
  (expect (= 6 (tokenize #'nop "A B C " :end 5)))
  (expect (= 7 (tokenize #'nop " A B C" :end 6)))
  (expect (= 7 (tokenize #'nop " A B C " :end 6)))

  (expect (= 4 (tokenize #'nop "A B C" :end 3)))
  (expect (= 4 (tokenize #'nop "A B C " :end 4)))
  (expect (= 5 (tokenize #'nop " A B C" :end 5)))
  (expect (= 3 (tokenize #'nop " A B C " :end 3)))
  (expect (= 2 (tokenize #'nop " A B C " :end 1))))

(deftest tokenize-with-start-end-adjusted ()
  (let* ((%string (make-array 15 :element-type 'base-char :adjustable t
                                 :initial-contents "ABBRA-KA-DA-BRA"))
         (string (make-array 9 :element-type 'base-char
                               :displaced-to %string :displaced-index-offset 6)))
    (expect (string= "KA-DA-BRA" string))
    (expect (= 10 (tokenize #'nop "KA-DA-BRA" :by #\- :start 1)))
    (expect (= 10 (tokenize #'nop string :by #\- :start 1)))
    (expect (= 6 (tokenize #'nop "KA-DA-BRA" :by #\- :end 6)))
    (expect (= 6 (tokenize #'nop string :by #\- :end 6)))))

(deftest tokenize-with-empty-test ()
  (expect (= 6 (tokenize #'nop "A,B,C" :by #\, :keep-empty t)))
  (expect (= 5 (tokenize #'nop "A,,C" :by #\, :keep-empty t)))
  (expect (= 5 (tokenize #'nop ",B,C" :by #\, :keep-empty t)))
  (expect (= 5 (tokenize #'nop "A,B," :by #\, :keep-empty t)))
  (expect (= 3 (tokenize #'nop ",," :by #\, :keep-empty t)))
  (expect (= 2 (tokenize #'nop "," :by #\, :keep-empty t)))
  (expect (= 1 (tokenize #'nop "" :by #\, :keep-empty t)))

  (expect (= 6 (tokenize #'nop "A,B,C" :by #\, :keep-empty t :start 1)))
  (expect (= 5 (tokenize #'nop "A,,C" :by #\, :keep-empty t :start 2)))
  (expect (= 5 (tokenize #'nop ",B,C" :by #\, :keep-empty t :start 3)))
  (expect (= 5 (tokenize #'nop "A,B," :by #\, :keep-empty t :start 2)))
  (expect (= 3 (tokenize #'nop ",," :by #\, :keep-empty t :start 0)))
  (expect (= 2 (tokenize #'nop "," :by #\, :keep-empty t :start 1)))
  (expect (= 1 (tokenize #'nop "" :by #\, :keep-empty t :start 0)))

  (expect (= 2 (tokenize #'nop "A,B,C" :by #\, :keep-empty t :end 1)))
  (expect (= 3 (tokenize #'nop "A,,C" :by #\, :keep-empty t :end 2)))
  (expect (= 4 (tokenize #'nop ",B,C" :by #\, :keep-empty t :end 3)))
  (expect (= 3 (tokenize #'nop "A,B," :by #\, :keep-empty t :end 2)))
  (expect (= 2 (tokenize #'nop ",," :by #\, :keep-empty t :end 1)))
  (expect (= 2 (tokenize #'nop "," :by #\, :keep-empty t :end 1)))
  (expect (= 2 (tokenize #'nop "A" :by #\, :keep-empty t :end 1)))
  (expect (= 1 (tokenize #'nop "" :by #\, :keep-empty t :end 0))))

(deftest tokenize-with-count-test ()
  (expect (= 0 (tokenize #'nop "A B C" :count 0)))
  (expect (= 2 (tokenize #'nop "A B C" :count 1)))
  (expect (= 4 (tokenize #'nop "A B C" :count 2)))
  (expect (= 6 (tokenize #'nop "A B C" :count 3)))
  (expect (= 6 (tokenize #'nop "A B C" :count nil)))

  (expect (= 0 (tokenize #'nop "  A23   B23  C23   " :count 0)))
  (expect (= 6 (tokenize #'nop "  A23   B23  C23   " :count 1)))
  (expect (= 12 (tokenize #'nop "  A23   B23  C23   " :count 2)))
  (expect (= 17 (tokenize #'nop "  A23   B23  C23   " :count 3)))
  (expect (= 17 (tokenize #'nop "  A23   B23  C23   " :count nil)))

  (expect (= 0 (tokenize #'nop "  A23   B23  C23   " :count 0 :end 7)))
  (expect (= 6 (tokenize #'nop "  A23   B23  C23   " :count 1 :end nil)))
  (expect (= 6 (tokenize #'nop "  A23   B23  C23   " :count 1 :end 7)))
  (expect (= 6 (tokenize #'nop "  A23   B23  C23   " :count 2 :end 7)))
  (expect (= 12 (tokenize #'nop "  A23   B23  C23   " :count 2 :end nil)))
  (expect (= 6 (tokenize #'nop "  A23   B23  C23   " :count 3 :end 7)))
  (expect (= 6 (tokenize #'nop "  A23   B23  C23   " :count nil :end 7))))

(deftest tokenize-with-count-and-keep-rest ()
  (expect (= 0 (tokenize #'nop "A B C" :count 0 :keep-rest t)))
  (expect (= 2 (tokenize #'nop "A B C" :count 1 :keep-rest t)))
  (expect (= 4 (tokenize #'nop "A B C" :count 2 :keep-rest t)))
  (expect (= 6 (tokenize #'nop "A B C" :count 3 :keep-rest t)))
  (expect (= 6 (tokenize #'nop "A B C" :count nil :keep-rest t)))

  (expect (= 0 (tokenize #'nop "  A23   B23  C23   " :count 0 :keep-rest t)))
  (expect (= 6 (tokenize #'nop "  A23   B23  C23   " :count 1 :keep-rest t)))
  (expect (= 12 (tokenize #'nop "  A23   B23  C23   " :count 2 :keep-rest t)))
  (expect (= 17 (tokenize #'nop "  A23   B23  C23   " :count 3 :keep-rest t)))
  (expect (= 17 (tokenize #'nop "  A23   B23  C23   " :count nil :keep-rest t)))

  (expect (= 0 (tokenize #'nop "  A23   B23  C23   " :count 0 :keep-rest t :end 7)))
  (expect (= 6 (tokenize #'nop "  A23   B23  C23   " :count 1 :keep-rest t :end nil)))
  (expect (= 6 (tokenize #'nop "  A23   B23  C23   " :count 1 :keep-rest t :end 7)))
  (expect (= 6 (tokenize #'nop "  A23   B23  C23   " :count 2 :keep-rest t :end 7)))
  (expect (= 12 (tokenize #'nop "  A23   B23  C23   " :count 2 :keep-rest t :end nil)))
  (expect (= 6 (tokenize #'nop "  A23   B23  C23   " :count 3 :keep-rest t :end 7)))
  (expect (= 6 (tokenize #'nop "  A23   B23  C23   " :count nil :keep-rest t :end 7))))

(deftest tokenize-with-keep-empty-and-count ()
  (expect (= 0 (tokenize #'nop "  A23   B23  C23   " :count 0 :keep-empty t)))
  (expect (= 1 (tokenize #'nop "  A23   B23  C23   " :count 1 :keep-empty t)))
  (expect (= 2 (tokenize #'nop "  A23   B23  C23   " :count 2 :keep-empty t)))
  (expect (= 6 (tokenize #'nop "  A23   B23  C23   " :count 3 :keep-empty t)))
  (expect (= 20 (tokenize #'nop "  A23   B23  C23   " :count nil :keep-empty t)))

  (expect (= 0 (tokenize #'nop "  A23   B23  C23   " :count 0 :end 7 :keep-empty t)))
  (expect (= 1 (tokenize #'nop "  A23   B23  C23   " :count 1 :end nil :keep-empty t)))
  (expect (= 1 (tokenize #'nop "  A23   B23  C23   " :count 1 :end 7 :keep-empty t)))
  (expect (= 2 (tokenize #'nop "  A23   B23  C23   " :count 2 :end 7 :keep-empty t)))
  (expect (= 2 (tokenize #'nop "  A23   B23  C23   " :count 2 :end nil :keep-empty t)))
  (expect (= 6 (tokenize #'nop "  A23   B23  C23   " :count 3 :end 7 :keep-empty t)))
  (expect (= 8 (tokenize #'nop "  A23   B23  C23   " :count nil :end 7 :keep-empty t))))


;;;
;;; do-tokens
;;;

(deftest do-tokens-test ()
  (with-collectors (tokens)
    (do-tokens (token "(this, is, a, token, string)" :by #'end-of-token-p)
      (tokens token))
    (expect (equal '("this" "is" "a" "token" "string") tokens))))

(deftest do-tokens-test2 ()
  (let* ((string "(this, is, a, token, string)"))
    (with-collectors (tokens)
      (do-tokens ((start end) string :by #'end-of-token-p)
        (tokens (subseq string start end)))
      (expect (equal '("this" "is" "a" "token" "string") tokens)))))

(deftest join-test ()
  (expect (equal "a-b-c" (join '- '("a" "b" "c"))))
  (expect (equal "" (join ":" '())))
  (expect (equal "a" (join "--" '("a"))))
  (expect (equal "a--b" (join "--" '("a" "b")))))

(deftest cat-test ()
  (expect (equal "" (cat)))
  (expect (equal "A" (cat "A")))
  (expect (equal "AB" (cat "AB")))
  (expect (equal "AB" (cat "A" "B")))
  (expect (equal "ABC" (cat "ABC")))
  (expect (equal "ABC" (cat "AB" "C")))
  (expect (equal "ABC" (cat "A" "BC")))
  (expect (equal "ABC" (cat "A" "B" "C"))))

(deftest cat2-test ()
  (declare (notinline cat))
  (expect (equal "" (cat)))
  (expect (equal "A" (cat "A")))
  (expect (equal "AB" (cat "AB")))
  (expect (equal "AB" (cat "A" "B")))
  (expect (equal "ABC" (cat "ABC")))
  (expect (equal "ABC" (cat "AB" "C")))
  (expect (equal "ABC" (cat "A" "BC")))
  (expect (equal "ABC" (cat "A" "B" "C"))))

(deftest read-as-keyword-test ()
  (expect (eq :test (read-as-keyword "TEST")))
  (expect (eq :test (read-as-keyword "test")))
  (expect (eq :test (read-as-keyword "tEsT")))
  (expect (eq :test (read-as-keyword ":TEST")))
  (expect (eq :|tEsT| (read-as-keyword ":|tEsT|")))
  (expect (eq :test (read-as-keyword "::TEST")))
  (expect (eq :|ABC#..| (read-as-keyword "abc#..")))

  (expect (signalsp read-as-keyword-error
            (read-as-keyword ":::TEST")))

  (expect (signalsp read-as-keyword-error
            (read-as-keyword "a:TEST")))

  (expect (signalsp read-as-keyword-error
            (read-as-keyword "a::TEST")))

  (expect (signalsp read-as-keyword-error
            (read-as-keyword "1.0d0")))

  (expect (signalsp read-as-keyword-error
            (read-as-keyword "abc#.(+ 1 2)")))

  (expect (signalsp read-as-keyword-error
            (read-as-keyword "abc def")))

  (expect (signalsp read-as-keyword-error
            (read-as-keyword "abc(123)"))))

 (deftest search-replace-test ()
   (expect (string= (search-replace "OLD" "NEW" "THIS-OLD-STRING") "THIS-NEW-STRING"))
   (expect (string= (search-replace "OLD" "NEW" "OLD-STRING") "NEW-STRING"))
   (expect (string= (search-replace "OLD" "NEW" "THIS-OLD") "THIS-NEW"))
   (expect (string= (search-replace "OLD" "NEW" "OLD") "NEW"))
   (expect (string= (search-replace "OLD" "NEW" "FOO") "FOO"))

   (expect (string= (search-replace "Old" "NEW" "this-old-string" :test #'char-equal)
                    "this-NEW-string"))
   (expect (string= (search-replace "OLD" "NEW" "OLD-string" :test #'char-equal) "NEW-string"))
   (expect (string= (search-replace "old" "NEW" "this-Old" :test #'char-equal) "this-NEW"))
   (expect (string= (search-replace "old" "NEW" "OLD" :test #'char-equal) "NEW"))
   (expect (string= (search-replace "OLD" "NEW" "FOO" :test #'char-equal) "FOO")))
