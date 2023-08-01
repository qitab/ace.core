;;; Copyright 2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

;;;;
;;;; Tests the defun* macro.
;;;;

(cl:defpackage #:ace.core.defun-test
  (:use #:common-lisp
        #:ace.core.macro
        #:ace.core.defun
        #:ace.core.type
        #:ace.test)
  (:import-from #:ace.core.defun
                #:parse-parameter-list
                #:parse-self-type-declaration))

(cl:in-package :ace.core.defun-test)


(defun %param (param)
  (when param
    (let ((name (parameter-name param))
          (type (parameter-type param))
          (default (parameter-default param))
          (supplied (parameter-supplied param))
          (key (parameter-key param)))
      (ecase (parameter-label param)
        ((:required :rest) `(,name ,type))
        ((:optional) `(,name ,type ,default ,supplied))
        ((:key) `(,name ,type ,default ,supplied ,key))
        ((:aux) `(,name ,type ,default))))))

(defun %params (params)
  (mapcar #'%param params))

(deftest test-parse-self-type-declaration ()
  (let* ((finfo
          (parse-self-type-declaration
           (parse-parameter-list '(a &optional a-short (a-long a a-long-sup)
                                   &rest keys
                                   &key (b 100) (b-long b)
                                   ((:c c-arg) nil c-sup)
                                   ((:c-long c-arg-long) (or c-arg 200) c-long-sup)
                                   &allow-other-keys
                                   &aux (z 20))
                                 :function-name 'foo)
           '(fixnum &optional fixnum (:a-long fixnum)
             &rest t
             &key fixnum (:b-long fixnum)
             (:c fixnum) (:c-long fixnum)
             &allow-other-keys
             &aux fixnum)))
         (required (finfo-required finfo))
         (optional (finfo-optional finfo))
         (rest (finfo-rest finfo))
         (keyword (finfo-keyword finfo))
         (aux (finfo-aux finfo)))
    (expect (equal (%params required) '((a fixnum))))
    (expect (equal (%params optional)
                   '((A-SHORT FIXNUM NIL NIL) (A-LONG FIXNUM A A-LONG-SUP))))
    (expect (equal (%param rest) '(keys t)))
    (expect (equal (%params keyword)
                   '((B FIXNUM 100 NIL :B) (B-LONG FIXNUM B NIL :B-LONG)
                     (C-ARG FIXNUM NIL C-SUP :C)
                     (C-ARG-LONG FIXNUM (OR C-ARG 200) C-LONG-SUP :C-LONG))))
    (expect (finfo-allow-other-keys finfo))
    (expect (equal (%params aux) '((z fixnum 20))))))

(deftest test-parse-self-type-declaration2 ()
  (let* ((finfo
          (parse-self-type-declaration
           (parse-parameter-list '(a &optional a-short (a-long a a-long-sup)
                                   &rest keys
                                   &key (b 100) (b-long b)
                                   ((:c c-arg) nil c-sup)
                                   ((:c-long c-arg-long) (or c-arg 200) c-long-sup)
                                   &aux (x 20) y)
                                 :function-name 'foo
                                 :body
                                 '((declare (t keys)
                                    (fixnum a-short a-long b b-long c-arg c-arg-long x y))))
           '(fixnum)))
         (required (finfo-required finfo))
         (optional (finfo-optional finfo))
         (rest (finfo-rest finfo))
         (keyword (finfo-keyword finfo))
         (aux (finfo-aux finfo)))

    (expect (equal (%params required) '((a fixnum))))
    (expect (equal (%params optional)
                   '((A-SHORT FIXNUM NIL NIL) (A-LONG FIXNUM A A-LONG-SUP))))
    (expect (equal (%param rest) '(keys t)))
    (expect (equal (%params keyword)
                   '((B FIXNUM 100 NIL :B) (B-LONG FIXNUM B NIL :B-LONG)
                     (C-ARG FIXNUM NIL C-SUP :C)
                     (C-ARG-LONG FIXNUM (OR C-ARG 200) C-LONG-SUP :C-LONG))))
    (expect (null (finfo-allow-other-keys finfo)))
    (expect (equal (%params aux) '((x fixnum 20) (y fixnum nil))))))


(defun* test-empty-params ()
  (declare (self () list))
  nil)

(defun* test-aux-keys (a &key b &aux (c (+ a b)))
  (list a b c))

(defun* test-function-optional (a &optional b)
  (declare (self (fixnum &optional fixnum) fixnum))
  "Function that tests the &optional keyword."

  (+ a (or b 23)))

(defun* test-function-keyword (a &key b)
  "Function that tests the &key lambda keyword."
  (declare (self (fixnum &key fixnum) fixnum))

  (+ a (or b 42)))

(defun* test-large-function-keyword (a &key b)
  "Function that tests the &key lambda keyword."
  (declare (self (list &key list) fixnum))
  (loop repeat 10
        for x in a
        for y in b
        while (< x y)
        do (format T "~A < ~A~%" x y)
           (format T "~A + ~A = ~A~%" x y (+ x y))
        sum (+ (loop for x in a sum x)
               (loop for x in b sum x))))


(defun* test-function-values (a)
  "Function that tests declaring multiple value return type."
  (declare (self (fixnum) fixnum boolean))

  (if (minusp a)
      (values (- a) nil)
      (values a t)))

(defun* test-function-provided (&key (a nil a-provided-p))
  "Function that tests the provided-p params."
  (declare (self (&key fixnum) fixnum boolean))
  (values (or a 0) a-provided-p))

(defun* test-function-with-nullable-args1  (a b &rest c)
  "Test that argument with nullable/complex types are supported."
  (declare (self ((or null fixnum) (or null fixnum) &rest fixnum) fixnum))
  (apply #'+ (or a 1) (or b 2) c))

(defun* test-function-with-nullable-args2  (a b &key c)
  "Test that argument with nullable/complex types are supported."
  (declare (self ((or null fixnum) (or null fixnum) &key (or null fixnum)) fixnum))
  (+ (or a 1) (or b 2) (or c 3)))

(defun* test-large-function-provided (&key (a nil a-provided-p))
  "Function that tests the provided-p params."
  (declare (self (&key fixnum) fixnum boolean))

  (loop repeat 10
        for x in a
        for y in a
        while (< x y)
        do (format T "~A < ~A~%" x y)
           (format T "~A + ~A = ~A~%" x y (+ x y))
        sum (+ (loop for x in a sum x)
               (loop for x in a sum x)))

  (values (or a 0) a-provided-p))

(defun* test-function-provided2 (&key (a 0 a-provided-p))
  (declare (self (&key fixnum) fixnum boolean))
  "Function that tests default argument values."
  (values a a-provided-p))

(defun* test-large-function-provided2 (&key (a 0 a-provided-p))
  "Function that tests the provided-p params."
  (declare (self (&key list) (or list fixnum) boolean))

  (loop repeat 10
        for x in a
        for y in a
        while (< x y)
        do (format T "~A < ~A~%" x y)
           (format T "~A + ~A = ~A~%" x y (+ x y))
        sum (+ (loop for x in a sum x)
               (loop for x in a sum x)))

  (values (or a 0) a-provided-p))

(defun* test-function-no-ftype (a b)
  "Function that has no ftype declaration."
  (+ a (or b 11)))

(defun* test-opt-function-no-ftype (a &optional b)
  "Function that has no ftype declaration."
  (+ a (or b 11)))

(defun* test-key-function-no-ftype (a &key b)
  "Function that has no ftype declaration."
  (+ a (or b 11)))

(defun* test-function-inline (a b)
  (declare (self inline (number fixnum) fixnum))
  "Inlined function without keywords."
  (+ a (or b 22)))

(defun* inline test-function-inline2 (a b)
  (declare (self (number fixnum) fixnum))
  "Inlined function without keywords."
  (+ a (or b 22)))

(defun* test-function-inline-keys (a &key b)
  (declare (self inline (number &key fixnum) fixnum))
  "Inlined function with keywords."
  (+ a (or b 22)))

(defun* test-function-notinline-keys (a &key b)
  (declare (self notinline (number &key fixnum) fixnum))
  "Not inlined function with keywords."
  (+ a (or b 22)))


(defun* test-large-function-notinline-keys (a &key b)
  "Function that tests the &key lambda keyword."
  (declare (self notinline (list &key list) fixnum))
  (loop repeat 10
        for x in a
        for y in b
        while (< x y)
        do (format T "~A < ~A~%" x y)
           (format T "~A + ~A = ~A~%" x y (+ x y))
        sum (+ (loop for x in a sum x)
               (loop for x in b sum x))))

(defun* test-just-inline (a &key b)
  "This tests that the (declare (self inline)) directive works without declaring an ftype."
  (declare (self inline) (fixnum a b))
  (+ a b))

(defun* fib (n &key (a 0) (b 1))
  "Tail-recursive Fibonacci number function."
  (declare (self (fixnum &key fixnum fixnum) fixnum))
  (if (= n 0)
      a
      (fib (- n 1) :a b :b (+ a b))))

(deftest test-function-calls ()
  (expect (= (test-function-optional 1) 24))
  (expect (= (test-function-optional 1 2) 3))

  (expect (= (test-function-keyword 1) 43))
  (expect (= (test-function-keyword 1 :b 2) 3))

  (mapc (lambda (expected i) (expect (= expected (fib i))))
        '(0 1 1 2 3 5 8 13)
        '(0 1 2 3 4 5 6 7))

  (multiple-value-bind (abs positive) (test-function-values 1)
    (expect (= abs 1))
    (expect positive))

  (multiple-value-bind (abs positive) (test-function-values -1)
    (expect (= abs 1))
    (expect (not positive)))

  (multiple-value-bind (a provided-p) (test-function-provided :a 1)
    (expect (= a 1))
    (expect provided-p))

  (multiple-value-bind (a provided-p) (test-function-provided)
    (expect (= a 0))
    (expect (not provided-p)))

  (multiple-value-bind (a provided-p) (test-function-provided2 :a 1)
    (expect (= a 1))
    (expect provided-p))

  (multiple-value-bind (a provided-p) (test-function-provided2)
    (expect (= a 0))
    (expect (not provided-p))))

(defparameter *dbg* (or #+dbg t))

(deftest test-simple-function-transforms ()

  ;; test-function-optional

  ;; In unoptimized version the function is not split nor inlined.
  (when *dbg*
    (expect (not (fboundp '%test-function-optional)))
    (expect (not (inline-function-p 'test-function-optional))))
  ;; In optimized build the function is not split nor inlined.
  (unless *dbg*
    (expect (not (fboundp '%test-function-optional)))
    (expect (not (inline-function-p 'test-function-optional)))))

(deftest test-inline-function-transforms ()

  ;; test-just-inline

  ;; In unoptimized version the function is not split nor inlined.
  (when *dbg*
    (expect (not (fboundp '%test-just-inline)))
    (expect (not (inline-function-p 'test-just-inline))))
  ;; In optimized build the function is not split but inlined.
  (unless *dbg*
    (expect (not (fboundp '%test-just-inline)))
    (expect (inline-function-p 'test-just-inline)))


  ;; test-function-inline

  ;; In unoptimized version the function is not split nor inlined.
  (when *dbg*
    (expect (not (fboundp '%test-function-inline)))
    (expect (not (inline-function-p 'test-function-inline))))
  ;; In optimized build the function is not split but inlined.
  (unless *dbg*
    (expect (not (fboundp '%test-function-inline)))
    (expect (inline-function-p 'test-function-inline)))

  ;; test-function-inline2

  ;; In unoptimized version the function is not split nor inlined.
  (when *dbg*
    (expect (not (fboundp '%test-function-inline2)))
    (expect (not (inline-function-p 'test-function-inline2))))
  ;; In optimized build the function is not split but inlined.
  (unless *dbg*
    (expect (not (fboundp '%test-function-inline2)))
    (expect (inline-function-p 'test-function-inline2))))

(deftest test-keyword-function-transforms ()
  ;; test-function-keyword

  ;; In unoptimized version the function is not split nor inlined.
  (when *dbg*
    (expect (not (fboundp '%test-function-keyword)))
    (expect (not (inline-function-p 'test-function-keyword))))
  ;; In optimized build the function is split, and the interface is inlined.
  (unless *dbg*
    (expect (fboundp '%test-function-keyword))
    (expect (inline-function-p 'test-function-keyword))
    (expect (not (inline-function-p '%test-function-keyword))))


  ;; test-function-inline-keys

  ;; In unoptimized version the function is not split nor inlined.
  (when *dbg*
    (expect (not (fboundp '%test-function-inline-keys)))
    (expect (not (inline-function-p 'test-function-inline-keys))))
  ;; In optimized build the function is not split but inlined.
  (unless *dbg*
    (expect (not (fboundp '%test-function-inline-keys)))
    (expect (inline-function-p 'test-function-inline-keys)))

  ;; test-function-notinline-keys

  ;; In unoptimized version the function is not split nor inlined.
  (when *dbg*
    (expect (not (fboundp '%test-function-notinline-keys)))
    (expect (not (inline-function-p 'test-function-notinline-keys))))
  ;; In optimized build the function is not split but inlined.
  (unless *dbg*
    (expect (not (fboundp '%test-function-notinline-keys)))
    (expect (not (inline-function-p 'test-function-notinline-keys)))))

(defstruct test-struct a b)

#+sbcl
(defun* (setf test-inline-a) (new-value struct)
  "A setter function."
  (declare (self inline))
  (setf (test-struct-a struct) new-value))

#+sbcl
(deftest test-setf-function-transforms ()
  ;; test-inline-a

  ;; In unoptimized version the function is not split nor inlined.
  (when *dbg*
    (expect (not (fboundp '(setf %test-inline-a))))
    (expect (not (inline-function-p '(setf test-inline-a)))))
  ;; In optimized build the function is not split but inlined.
  (unless *dbg*
    (expect (not (fboundp '(setf %test-inline-a))))
    (expect (inline-function-p '(setf test-inline-a)))))

(defun* function-with-rest-args (a &rest args &key b (c 1 c-p) &allow-other-keys &aux (d (+ a b c)))
  (declare (self (fixnum &rest t &key fixnum fixnum) list))
  (declare (optimize (speed 1) (debug 2) (safety 3)))
  (declare (fixnum d))
  (list a args b c c-p d))

(deftest test-function-with-rest-args ()
  (declare (optimize (speed 1) (debug 2) (safety 3)))
  (expect (equal '(1 (:b 2 :c 3 :d 4) 2 3 t 6)
                 (function-with-rest-args 1 :b 2 :c 3 :d 4)))
  (let ((x 0))
    (expect (equal '(1 (:c 2 :b 3 :d 4) 3 2 t 6)
                   (function-with-rest-args (incf x)
                                            :c (incf x)
                                            :b (incf x)
                                            :d (incf x))))))

(deftype short () '(unsigned-byte 16))

(defun* function-with-ignore
    (a b c &rest args &key (d 10 d-p) (e 20) f (g 30 g-p) &allow-other-keys &aux (h (+ a e f g)))
  (declare (self (short short short &rest t &key short short (:f short) (:g short)) list))
  (declare (ignore b c d)) (declare (ignore g-p))
  (declare (type short e h))
  (list a d-p e f g h args))

(deftest test-function-with-ignore ()
  (declare (optimize (speed 1) (debug 2) (safety 3)))
  (expect (equal '(1 t 4 5 6 16 (:e 4 :f 5 :g 6 :d 7))
                 (function-with-ignore 1 2 3 :e 4 :f 5 :g 6 :d 7)))

  (expect (equal '(1 nil 20 4 30 55 (:f 4))
                 (function-with-ignore 1 2 3 :f 4)))

  (let ((x 0))
    (expect (equal '(1 t 4 5 6 16 (:e 4 :f 5 :g 6 :d 7))
                   (function-with-ignore
                    (incf x) (incf x) (incf x) :e (incf x) :f (incf x) :g (incf x) :d (incf x))))))


(defun* function-with-alias-keywords (a &key (b 20) ((:c c-arg)) ((:d d-arg) 30)
                                        ((:e e-arg) 40 e-arg-sup))
  (list a b c-arg d-arg e-arg e-arg-sup))


(defun* function-with-alias-keywords2 (a &key (b 20) ((:c c-arg)) ((:d d-arg) 30)
                                         ((:e e-arg) 40 e-arg-sup))
  (declare (self (fixnum &key fixnum (:c fixnum) :d fixnum (:e fixnum)) list))
  (list a b c-arg d-arg e-arg e-arg-sup))


(defun* function-with-alias-keywords2b (a &key (b 20) ((:c c-arg)) ((:d d-arg) 30)
                                          ((:e e-arg) 40 e-arg-sup))
  (declare (self (fixnum &key fixnum (:c fixnum) fixnum :e fixnum) list))
  (list a b c-arg d-arg e-arg e-arg-sup))


(defun* function-with-alias-keywords2c (a &key (b 20) ((:c c-arg)) ((:d d-arg) 30)
                                          ((:e e-arg) 40 e-arg-sup))
  (declare (self (fixnum &key fixnum (:c fixnum) :e string :d fixnum) list))
  (list a b c-arg d-arg e-arg e-arg-sup))


(deftest test-function-with-alias-keywords ()
  (expect (equal '(1 2 3 4 5 t)
                 (function-with-alias-keywords 1 :b 2 :c 3 :d 4 :e 5)))

  (expect (equal '(10 20 nil 30 40 nil)
                 (function-with-alias-keywords 10))))

(deftest test-function-with-alias-keywords2 ()
  (expect (equal '(1 2 3 4 5 t)
                 (function-with-alias-keywords2 1 :b 2 :c 3 :d 4 :e 5)))

  (expect (equal '(10 20 nil 30 40 nil)
                 (function-with-alias-keywords2 10))))

(defun* function-with-alias-keywords3 (a &optional a-short (a-long a a-long-sup)
                                         &key (b 100) (b-long b)
                                         ((:c c-arg) nil c-sup)
                                         ((:c-long c-arg-long) (if c-sup c-arg 200) c-long-sup))
  (list a-short a-long a-long-sup b-long c-arg-long c-long-sup))

(defun* function-with-alias-keywords4 (a &optional a-short (a-long a a-long-sup)
                                         &key (b 100) (b-long b)
                                         ((:c c-arg) nil c-sup)
                                         ((:c-long c-arg-long) (or c-arg 200) c-long-sup))
  (declare (self (fixnum &optional fixnum (:a-long fixnum)
                         &key fixnum (:b-long fixnum)
                         (:c fixnum) (:c-long fixnum)) list))

  (list a-short a-long a-long-sup b-long c-sup c-arg-long c-long-sup))

(deftest test-function-with-alias-keywords3 ()
  (expect (equal '(2 3 t 5 7 t)
                 (function-with-alias-keywords3
                  1 2 3 :b 4 :b-long 5 :c 6 :c-long 7)))

  (expect (equal '(nil 1 nil 100 200 nil)
                 (function-with-alias-keywords3 1))))


(deftest test-function-with-alias-keywords4 ()
  (expect (equal '(2 3 t 5 t 7 t)
                 (function-with-alias-keywords4
                  1 2 3 :b 4 :b-long 5 :c 6 :c-long 7)))

  (expect (equal '(nil 1 nil 100 nil 200 nil)
                 (function-with-alias-keywords4 1))))

(defun* function-with-keyword+declare (a &key (b 20) (c (+ a b)))
  "Function contains arguments that are declared fixnum.
 A and B are not used in the body and also need not be ignored."
  (declare (fixnum a b c))
  c)

(deftest test-function-with-keyword+declare ()
  (expect (= 3 (function-with-keyword+declare 1 :b 2)))

  (expect (= 30 (function-with-keyword+declare 10)))

  (expect (= 4 (function-with-keyword+declare 10 :c 4)))

  (expect (= 4 (function-with-keyword+declare 1 :b 2 :c 4))))

(defun* function-with-keyword+declare2 (a &key (b 20) (c (+ a b)))
  "Function contains arguments that are declared fixnum.
 A and B are not used in the body and also need not be ignored."
  (declare (self (fixnum &key fixnum fixnum) fixnum))
  (declare (fixnum a b c))
  c)

(deftest test-function-with-keyword+declare2 ()
  (expect (= 3 (function-with-keyword+declare2 1 :b 2)))

  (expect (= 30 (function-with-keyword+declare2 10)))

  (expect (= 4 (function-with-keyword+declare2 10 :c 4)))

  (expect (= 4 (function-with-keyword+declare2 1 :b 2 :c 4))))


(defun* function-with-keyword+declare3 (a &key b (c (+ a b)))
  "Function contains arguments that are declared fixnum.
 A and B are not used in the body and also need not be ignored."
  (declare (self (fixnum &key fixnum fixnum) fixnum))
  c)

(deftest test-function-with-keyword+declare3 ()
  (expect (= 3 (function-with-keyword+declare3 1 :b 2)))

  (expect (= 4 (function-with-keyword+declare3 1 :c 4)))

  (expect (= 4 (function-with-keyword+declare3 1 :b 2 :c 4))))

(defun* simple-defun* (pos &key a (b :b) c (d "D" d-present-p))
  (declare (ignorable b))
  (list pos a b c d d-present-p))

(deftest test-simple-defun* ()
  (expect (equal (list "POS" "A" "B" 3 "D" nil) (simple-defun* "POS" :c 3 :b "B" :a "A")))
  (expect (equal (list "POS" "A" "B" 3 "D" t) (simple-defun* "POS" :c 3 :b "B" :a "A" :d "D")))
  (expect (equal (list "POS" "A" "B" 3 "E" t) (simple-defun* "POS" :c 3 :b "B" :a "A" :d "E")))
  (expect (equal (list "POS" "A" :b 3 "D" t) (simple-defun* "POS" :c 3 :a "A" :d "D"))))

(deftest test-simple-defun*-apply ()
  (mapc (lambda (expected args)
          (declare (notinline simple-defun*))
          (expect (equal expected (apply 'simple-defun* args))))
        '(("POS" "A" "B" 3 "D" nil)
          ("POS" "A" "B" 3 "D" t)
          ("POS" "A" "B" 3 "E" t)
          ("POS" "A" :b 3 "D" t))
        '(("POS" :c 3 :b "B" :a "A")
          ("POS" :c 3 :b "B" :a "A" :d "D")
          ("POS" :c 3 :b "B" :a "A" :d "E")
          ("POS" :c 3 :a "A" :d "D"))))

(defun* keywords-defun* (&key a b c)
  (list a b c))

(deftest test-defun*-evaluation-order ()
  (let ((x 0))
    (expect (equal '(2 nil 1) (keywords-defun* :c (incf x) :a (incf x))))
    (expect (equal '(4 nil 3) (funcall 'keywords-defun* :c (incf x) :a (incf x))))
    (expect (equal '(6 nil 5) (apply 'keywords-defun* (list :c (incf x) :a (incf x)))))))

(deftest test-defun*-runtime-keyword ()
  (let ((k :b))
    (expect (equal '(nil 1 nil) (keywords-defun* k 1)))
    (expect (equal '(nil 1 nil) (funcall 'keywords-defun* k 1)))
    (expect (equal '(nil 1 nil) (apply 'keywords-defun* (list k 1))))))


(defun calls-anything (function)
  (let ((string (with-output-to-string (stream)
                  (disassemble function :stream stream))))
    (or (search "JMP " string) ; if tail call
        (search "CALL " string))))

(defun* foldable- (a &key (b 0))
  (declare (self foldable (fixnum &key (:b fixnum)) fixnum))
  "A function that is compiler foldable."
  (the fixnum (- a b)))

(deftest test-foldable-transforms ()
  (let ((f1 (compile nil '(lambda (a b) (print (- a b)) 0)))
        (f2 (compile nil '(lambda (a b) (- a b)))))
    (expect (calls-anything f1))  ; confirm that CALLS-ANYTHING works for non-tail call
    (expect (calls-anything f2))) ; and works for tail call
  (let ((f (compile nil '(lambda () (foldable- 1)))))
    (expect (= 1 (funcall f)))
    (expect (not (calls-anything f))))
  (let ((f (compile nil '(lambda () (apply #'foldable- '(2))))))
    (expect (= 2 (funcall f)))
    (expect (not (calls-anything f)))))

(defun* my- (a b)
  "Not inline nor keyworded."
  (- a b))

(defun function-call-p (code)
  "True if the CAR of CODE comes up in the disassembly."
  (let* ((fun (compile nil `(lambda ()
                              (declare (optimize (speed 3) (safety 0) (debug 0)))
                              ,code)))
         (disassembly (with-output-to-string (out)
                         (disassemble fun :stream out))))
    (search (symbol-name (first code)) disassembly)))

#+sbcl
(deftest test-foldable2-transforms ()
  (expect (function-call-p '(my- 1 2)))
  (expect (not (function-call-p '(foldable- 1 :b 2)))))

(defun* inline-foldable- (a &key (b 0))
  (declare (self inline foldable (fixnum &key (:b fixnum)) fixnum))
  "A function that is compiler foldable."
  (- a b))

#+sbcl
(deftest test-inline-foldable-transforms ()
  (expect (not (fboundp '%inline-foldable-)))
  (expect (not (function-call-p '(inline-foldable- 1 :b 2))))

  (if *dbg*
      (expect (not (inline-function-p 'inline-foldable-)))
      (expect (inline-function-p 'inline-foldable-))))

(defun* foldable-inline- (a &key (b 0))
  (declare (self foldable inline (fixnum &key (:b fixnum)) fixnum))
  "A function that is compiler foldable."
  (- a b))

#+sbcl
(deftest test-foldable-inline-transforms ()
  (expect (not (fboundp '%foldable-inline-)))
  (expect (not (function-call-p '(foldable-inline- 1 :b 2))))

  (if *dbg*
      (expect (not (inline-function-p 'foldable-inline-)))
      (expect (inline-function-p 'foldable-inline-))))


(defstruct struct1 %a)

(defun* struct1-a (struct &key default)
  (or (struct1-%a struct) default))

(defun* (setf struct1-a) (value struct &key default)
  (setf (struct1-%a struct) value)
  (return-from struct1-a (or value default)))

(deftest test-setf-function ()
  (let ((s (make-struct1)))
    (expect (eql (struct1-a s :default 1) 1))
    (expect (eql (setf (struct1-a s :default 1) 2) 2))
    (expect (eql (setf (struct1-a s :default 1) nil) 1))
    (expect (eql (struct1-a s) nil))))

(defun* self-block-test-fun ()
  "Tests if we return expected value from this function."
  (declare (self () keyword))
  (return-from self :it-works)
  :all-things-great-must-come-to-an-end)

(defun* self-block-test-fun2 (a &key (b -1))
  "Tests if we return expected value from this function."
  (declare (self (fixnum &key fixnum) t))
  (return-from self (+ a b))
  :all-things-great-must-come-to-an-end)

(deftest test-self-block ()
  (expect (eq (self-block-test-fun) :it-works))
  (expect (eq (self-block-test-fun2 1) 0)))
