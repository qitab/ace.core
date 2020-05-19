;;; Copyright 2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

;;; Tests the macro utilities.
;;;

(defpackage #:ace.core.macro-test
  (:use #:common-lisp
        #:ace.core.macro
        #:ace.test))

(in-package :ace.core.macro-test)


(defmacro line-and-column-macro (&whole whole)
  `',(multiple-value-list (line-and-column-numbers whole)))

(deftest line-and-numbers-test ()
  (let ((actual (line-and-column-macro)))
    (expect (equal '(22 16 22 39) actual))))

#+nil foo

(deftest line-and-numbers-test2 ()
  #+nil bar
  (let ((actual (line-and-column-macro)))
    (expect (equal '(29 16 29 39) actual))))

(defparameter *ttt* (line-and-column-macro))
(defparameter *uuu*
  (line-and-column-macro))

 (defparameter *vvv* (line-and-column-macro))
 (defparameter *www* (line-and-column-macro))

(deftest line-and-columns-top-level ()
  (expect (equal '(32 20 32 43) *ttt*))
  (expect (equal '(34 2 34 25) *uuu*))
  (expect (equal '(36 21 36 44) *vvv*))
  (expect (equal '(37 21 37 44) *www*)))

(deftest lmap-test ()
  (expect (equal '(2 3 4 5) (lmap (i '(1 2 3 4))
                              (1+ i))))
  (expect (equal '(2 3 4 5) (lmap ((i) '((1 :a) (2 :b) (3) (4 :d)))
                              (1+ i))))

  (expect (equal '(1 1 1 1) (lmap (i (list 1 2 3 4)) (j '(0 1 2 3))
                               (- i j)))))

(deftest lconc-test ()
  (expect (equal '(2 3 4 5) (lconc (i '(1 2 3 4)) (list (1+ i)))))
  (expect (equal '(2 3 4 5) (lconc ((i) '((1 :a) (2 :b) (3) (4 :d)))
                              (list (1+ i)))))

  (expect (equal '(1 :a 2 :b 4 :d)
                 (lconc ((i k) '((1 :a) (2 :b) (3) (4 :d)))
                   (and k (list i k)))))

  (expect (equal '(1 1 1 1) (lconc (i (list 1 2 3 4)) (j '(0 1 2 3))
                                   (list (- i j))))))

(deftest with-gensyms-test ()
  (expect (gensymp (with-gensyms (s) s)))
  (expect (gensymp (with-gensyms ((s "FOO")) s)))
  (let* ((foo :foo)
         (sym (with-gensyms ((s foo)) s)))
    (expect (string-equal :foo sym :end2 3))
    (expect (gensymp (with-gensyms ((s foo)) s))))

  (expect (macroexpand '(with-gensyms (s) s)))
  (expect (macroexpand '(with-gensyms ((s "FOO")) s))))

(defun! compile-time-function (&optional (result "compile-time"))
  (declare (string result))
  result)

(defmacro test-compile-time-function ()
  `(equal "compile-time" ,(compile-time-function)))


(deftest defun!-test ()
  (expect (test-compile-time-function)))


(deftest find-declaration-test ()
  (expect (equal '(string result)
                 (find-declaration 'string
                                   '((declare (string result))
                                     (check-type result string)
                                     result))))

  (expect (null (find-declaration 'result
                                  '((declare (string result))
                                    (check-type result string)
                                    (or result nil)))))

  (expect (null (find-declaration 'or
                                  '((declare (string result))
                                    (check-type result string)
                                    (or result nil))))))


(deftest find-type-declaration-test ()
  (mapc (lambda (expected+var+form)
          (let ((expected (first expected+var+form))
                (var (second expected+var+form))
                (form (third expected+var+form)))
            (expect (eq expected (find-type-declaration var form)))))
        '((string result ((declare (string result))
                           (check-type result string)
                           result))

          (nil string ((declare (string result))
                        (check-type result string)
                        (or result nil)))

          (nil or ((declare (string result))
                    (check-type result string)
                    (or result nil)))

          (string string ((declare (string result))
                           (declare (string string))
                           (check-type result string)
                           (or result nil)))

          (boolean or ((declare (string result))
                       (declare (boolean or))
                       (check-type result string)
                       (or result nil)))

          (string result ((declare (string string result))
                          (check-type result string)
                          result))

          (string string ((declare (string string result))
                          (check-type result string)
                          (or result nil)))

          (nil or ((declare (string string result))
                   (check-type result string)
                   (or result nil)))

          (string result ((declare (type string result))
                          (check-type result string)
                          result))

          (nil string ((declare (type string result))
                       (check-type result string)
                       (or result nil)))
          (nil type ((declare (type string result))
                     (check-type result string)
                     (or result nil)))))
  (values))



(deftest remove-type-declarations-test ()
  (mapc (lambda (var+input+expected)
          (let ((var (first var+input+expected))
                (form (second var+input+expected))
                (expected (third var+input+expected)))
            (expect (equal expected (remove-type-declarations var form)))))
        '((result
           ((declare (string result))
            (check-type result string)
            result)
           ((check-type result string)
            result))

          (string
           ((declare (string result))
            (check-type result string)
            (or result nil))
           ((declare (string result))
            (check-type result string)
            (or result nil)))

          (or
           ((declare (string or result or))
            (check-type result string)
            (or result nil))
           ((declare (string result))
            (check-type result string)
            (or result nil)))

          (string
           ((declare (string result))
            (declare (string string))
            (declare (ignore result))
            (check-type result string)
            (or result nil))
           ((declare (string result))
            (declare (ignore result))
            (check-type result string)
            (or result nil)))

          (or
           ((declare (string result))
            (declare (boolean yes or why not) (type fixnum or) (fixnum x or y))
            "doc"
            (declare (ignore result))
            (check-type result string)
            (or result nil))
           ((declare (string result))
            (declare (boolean yes why not) (fixnum x y))
            "doc"
            (declare (ignore result))
            (check-type result string)
            (or result nil)))

          (result
           ((declare (string string result))
            (check-type result string)
            result)
           ((declare (string string))
            (check-type result string)
            result))

          (string
           ((declare (fixnum string result)))
           ((declare (fixnum result))))


          (result
           ((declare (type string result))
            (check-type result string)
            result)
           ((check-type result string)
            result))

          (string
           ((declare (type string result))
            (check-type result string)
            (or result nil))
           ((declare (type string result))
            (check-type result string)
            (or result nil)))

          (type
           ((declare (type string result))
            (check-type result string)
            (or result nil))
           ((declare (type string result))
            (check-type result string)
            (or result nil)))))
  (values))


(deftest with-lambda-body-test ()
  (let ((body '((declare (string result))
                "A test body."
                (declare (fixnum x))
                (check-type result string)
                (or result nil))))

    (with-split-body (body declarations docs)
      (expect (equal '((check-type result string)
                       (or result nil))
                     body))
      (expect (equal '((declare (string result))
                       (declare (fixnum x)))
                     declarations))
      (expect (equal '("A test body.") docs))))

  (let ((body '((declare (string result))
                "A test body.")))

    (with-split-body (body declarations docs)
      (expect (equal '("A test body.") body))
      (expect (equal '((declare (string result)))
                     declarations))
      (expect (null docs)))))

(deftest optimize-for-speed-test ()
  (declare (optimize (speed 3) (debug 1) (safety 1) (space 1)))
  (expect (optimize-for-speed-p!)))

(deftest no-optimize-for-speed-test ()
  (declare (optimize (speed 0) (debug 2) (safety 3) (space 1)))
  (expect (not (optimize-for-speed-p!))))

(deftest optimize-for-debug-test ()
  (declare (optimize (speed 1) (debug 2) (safety 1) (space 1)))
  (expect (optimize-for-debug-p!)))

(deftest no-optimize-for-debug-test ()
  (declare (optimize (speed 3) (debug 1) (safety 1) (space 1)))
  (expect (not (optimize-for-debug-p!))))

#+opt
(deftest optimize-for-speed-test2 ()
  (expect (optimize-for-speed-p!)))

#-opt
(deftest no-optimize-for-speed-test2 ()
  (expect (not (optimize-for-speed-p!))))

#-opt
(deftest optimize-for-debug-test2 ()
  (expect (optimize-for-debug-p!)))

#+opt
(deftest no-optimize-for-debug-test2 ()
  (expect (not (optimize-for-debug-p!))))

(declaim (inline inline-function-p-test))
(deftest inline-function-p-test ()
  (expect (not (inline-function-p 'with-lambda-body-test)))
  (expect (inline-function-p 'inline-function-p-test)))


(deftest remove-declarations-test ()
  (expect (equal '("doc" (declare (fixnum x)) (* x x))
                 (remove-declarations
                  'timeout '("doc"
                             (declare (fixnum x) (timeout 10))
                             (* x x)))))

  (expect (equal '("doc" (declare (fixnum x) (timeout 10)) (* x x))
                 (remove-declarations
                  'unknown '("doc"
                             (declare (fixnum x) (timeout 10))
                             (* x x)))))

  (expect (equal '("doc" (declare (fixnum x)) (* x x))
                 (remove-declarations
                  'timeout '((declare (timeout 1))
                             "doc"
                             (declare (fixnum x) (timeout 10))
                             (* x x)))))

  (expect (equal '((declare (fixnum x)) "doc" (* x x))
                 (remove-declarations
                  'timeout '((declare (fixnum x) (timeout 10))
                             "doc"
                             (* x x)))))

  (expect (equal '((declare (fixnum x) (timeout 10)) "doc" (* x x))
                 (remove-declarations
                  'unknown '((declare (fixnum x) (timeout 10))
                             "doc"
                             (* x x)))))


  (expect (equal '((declare (fixnum x)) "doc" (* x x))
                 (remove-declarations
                  'timeout '((declare (timeout 1))
                             (declare (fixnum x) (timeout 10))
                             "doc"
                             (* x x)))))

  (expect (equal '((declare (timeout 1)) (declare (timeout 10)) "doc" (* x x))
                 (remove-declarations
                  'fixnum '((declare (timeout 1))
                            (declare (fixnum x) (timeout 10))
                            "doc"
                            (* x x))))))
(defstruct x a)


(deftest strcat-test ()
  (let ((foo :foo)
        (f∘∘ :f∘∘)
        (bar :bar)
        (baz :baz))
    (expect (equalp "FOO-BAR-BAZ" (strcat foo :- bar :- baz)))
    (expect (equalp "FOO-BAR-BAZ" (strcat foo :- "BAR" :- baz)))
    (expect (equalp "F∘∘-BAR-BAZ" (strcat f∘∘ :- "BAR" :- baz)))
    (expect (equalp "FOO∘BAR-BAZ∘" (strcat foo :∘ "BAR" :- baz :∘))))
  (expect (equalp "FOO-BAR-BAZ" (strcat :foo- :bar- :baz)))
  (expect (equalp "FOO-BAR-2" (strcat :foo- nil :bar- 2 nil)))
  (expect (equalp "FOO" (strcat :foo)))
  (expect (equalp "1" (strcat 1)))
  (expect (equal "" (strcat nil)))
  (expect (equal "" (strcat))))

(deftest strcat-test2 ()
  (declare (notinline strcat))
  (let ((foo :foo)
        (f∘∘ :f∘∘)
        (bar :bar)
        (baz :baz))
    (expect (equalp "FOO-BAR-BAZ" (strcat foo :- bar :- baz)))
    (expect (equalp "FOO-BAR-BAZ" (strcat foo :- "BAR" :- baz)))
    (expect (equalp "F∘∘-BAR-BAZ" (strcat f∘∘ :- "BAR" :- baz)))
    (expect (equalp "FOO∘BAR-BAZ∘" (strcat foo :∘ "BAR" :- baz :∘))))
  (expect (equalp "FOO-BAR-BAZ" (strcat :foo- :bar- :baz)))
  (expect (equalp "FOO-BAR-2" (strcat :foo- nil :bar- 2 nil)))
  (expect (equalp "FOO" (strcat :foo)))
  (expect (equalp "1" (strcat 1)))
  (expect (equal "" (strcat nil)))
  (expect (equal "" (strcat))))

(deftest keycat-test ()
  (let ((foo :foo)
        (f∘∘ :f∘∘)
        (bar :bar)
        (baz :baz))
    (expect (eq :foo-bar-baz (keycat foo :- bar :- baz)))
    (expect (eq :foo-bar-baz (keycat foo :- :bar :- baz)))
    (expect (eq :f∘∘-bar-baz (keycat f∘∘ :- :bar :- baz)))
    (expect (eq :foo∘bar-baz∘ (keycat foo :∘ :bar :- baz :∘))))
  (expect (eq :foo-bar-baz (keycat :foo- :bar- :baz)))
  (expect (eq :foo-bar-2 (keycat :foo- nil :bar- 2 nil)))
  (expect (eq :foo (keycat :foo)))
  (expect (eq :1 (keycat 1)))
  (expect (eq :|| (keycat nil)))
  (expect (eq :|| (keycat))))

(deftest keycat-test2 ()
  (declare (notinline keycat))
  (let ((foo :foo)
        (f∘∘ :f∘∘)
        (bar :bar)
        (baz :baz))
    (expect (eq :foo-bar-baz (keycat foo :- bar :- baz)))
    (expect (eq :foo-bar-baz (keycat foo :- :bar :- baz)))
    (expect (eq :f∘∘-bar-baz (keycat f∘∘ :- :bar :- baz)))
    (expect (eq :foo∘bar-baz∘ (keycat foo :∘ :bar :- baz :∘))))
  (expect (eq :foo-bar-baz (keycat :foo- :bar- :baz)))
  (expect (eq :foo-bar-2 (keycat :foo- nil :bar- 2 nil)))
  (expect (eq :foo (keycat :foo)))
  (expect (eq :1 (keycat 1)))
  (expect (eq :|| (keycat nil)))
  (expect (eq :|| (keycat))))

(deftest symcat-test ()
  (let ((*package* (find-package :keyword)))
    (let ((foo :foo)
          (f∘∘ :f∘∘)
          (bar :bar)
          (baz :baz))
      (expect (eq :foo-bar-baz (symcat foo :- bar :- baz)))
      (expect (eq :foo-bar-baz (symcat foo :- :bar :- baz)))
      (expect (eq :f∘∘-bar-baz (symcat f∘∘ :- :bar :- baz)))
      (expect (eq :foo∘bar-baz∘ (symcat foo :∘ :bar :- baz :∘))))
    (expect (eq :foo-bar-baz (symcat :foo- :bar- :baz)))
    (expect (eq :foo-bar-2 (symcat :foo- nil :bar- 2 nil)))
    (expect (eq :foo (symcat :foo)))
    (expect (eq :1 (symcat 1)))
    (expect (eq :|| (symcat nil)))
    (expect (eq nil (symcat)))))

(deftest symcat-test2 ()
  (declare (notinline symcat))
  (let ((*package* (find-package :keyword)))
    (let ((foo :foo)
          (f∘∘ :f∘∘)
          (bar :bar)
          (baz :baz))
      (expect (eq :foo-bar-baz (symcat foo :- bar :- baz)))
      (expect (eq :foo-bar-baz (symcat foo :- :bar :- baz)))
      (expect (eq :f∘∘-bar-baz (symcat f∘∘ :- :bar :- baz)))
      (expect (eq :foo∘bar-baz∘ (symcat foo :∘ :bar :- baz :∘))))
    (expect (eq :foo-bar-baz (symcat :foo- :bar- :baz)))
    (expect (eq :foo-bar-2 (symcat :foo- nil :bar- 2 nil)))
    (expect (eq :foo (symcat :foo)))
    (expect (eq :1 (symcat 1)))
    (expect (eq :|| (symcat nil)))
    (expect (eq nil (symcat)))))

(defalias f+ +)
(defalias fmod mod)

(deftest alias-test ()
  (expect (macroexpand* '(defalias f+ +)))
  (expect (= 10 (f+ 1 2 3 4)))
  (expect (= 10 (reduce #'f+ '(1 2 3 4))))

  (expect (equal '(1 2 3) (mapcar #'fmod '(1 2 3) '(2 3 4))))
  (expect (= 3 (fmod 7 4))))


;;; Macro keyword helpers.

(defun some-key-fun (&key (a 1) (b 2) (c 3))
  (+ a b c))

(defmacro call-some-key-fun (&key a b z)
  `(some-key-fun ,@(KEYS> a b (:c z))))

(deftest test-keys>-helper ()
  (expect (= 6 (call-some-key-fun)))
  (expect (= 8 (call-some-key-fun :b 4)))
  (expect (= 9 (call-some-key-fun :a 2 :b 4)))
  (expect (= 12 (call-some-key-fun :a 2 :b 4 :z 6))))

(defmacro loop-test-keywords ((from below &key index foo bar) &body body)
  ;; <- FOO bar
  (when (and bar (not FOO))
    (setf FOO (gensym* :FOO)))

  `(loop :for index fixnum :from ,from :below ,below
     ,@(FOR> foo `(fixnum = (ash 1 ,index)))
     ,@(FOR> bar `(fixnum = (1- ,foo)))
     :do (progn ,@body)
     ,@(FINALLY-RETURN> index)))

(deftest test-loop-keywords ()
  (let ((body-executed 0))
    (expect
     (= (loop-test-keywords (10 20 :index index :bar bar)
          (incf body-executed)
          (expect (= bar (1- (ash 1 index)))))
        20))
    (expect (= 10 body-executed))))

(deftest function-file-path-test ()
  (expect (ace.core.string:suffixp
           "/core/macro.lisp"
           (namestring (function-file-path 'inline-function-p))))
  (expect (null (function-file-path '×))))
