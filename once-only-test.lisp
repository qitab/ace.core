;;; Copyright 2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

;;; Tests the macro utilities.

(defpackage #:ace.core.once-only-test
  (:use #:ace.core.macro
        #:ace.core.once-only
        #:ace.test
        #:common-lisp)
  (:import-from #:ace.core.once-only
                ace.core.once-only::generate-once-only-content))

(in-package :ace.core.once-only-test)

;;;
;;; ONCE-ONLY tests.
;;;

(defun zero () 0)
(defun one () 1)
(defun two () 2)
(defun three () 3)
(defun four () 4)

(defmacro cons2 (x)
  (once-only (x)
    `(cons ,x ,x)))

(defmacro cons2+1 (&optional (x '(zero) xp))
  (once-only ((x (if xp `(1+ ,x) '(zero))))
    (declare (fixnum x) (ignorable x))
    `(cons ,x ,x)))

(defmacro cons2+2 (&optional x)
  (once-only ((x `(+ (or ,x 3) 2)))
    (declare (fixnum x) (ignorable x))
    `(cons ,x ,x)))

(deftest once-only-test ()
  (expect (equal '(1 . 1) (cons2 1)))
  (expect (equal '(cons 1 1) (macroexpand '(cons2 1))))
  (let ((cc 0))
    (flet ((cc () (incf cc)))
      (expect (equal '(1 . 1) (cons2 (cc))))
      (expect (= cc 1))))

  ;; Since x is not provided (zero) is used instead.
  (expect (equal '(0 . 0) (cons2+1)))
  (let ((cc 0))
    (flet ((zero () (incf cc)))
      ;; The form expansion captures the lexical environment.
      ;; Since the x argument was not provided,
      ;; it will not be bound to a temporary variable and
      ;; evaluated once.
      (expect (equal '(1 . 1) (cons2+1)))
      (expect (= cc 1))))

  ;; Since 1 is constant, this evaluates to (cons (1+ 1) (1+ 1)).
  (expect (equal '(2 . 2) (cons2+1 1)))
  (expect (equal '(cons 2 2) (macroexpand '(cons2+1 1))))
  (let ((cc 0))
    (flet ((cc () (incf cc)))
      ;; Since "(cc)" is not constant,
      ;; The init form `(1+ ,x) will be used to initialize the value.
      (expect (equal '(2 . 2) (cons2+1 (cc))))
      (expect (= cc 1)))))

(deftest once-only-test2 ()
  ;; Since x is not provided the init form (+ (or nil 3) 2) is used.
  (expect (equal '(5 . 5) (cons2+2)))

  ;; Since 1 is constant, this evaluates to (cons (+ 1 2) (+ 1 2)).
  (expect (equal '(3 . 3) (cons2+2 1)))
  (let ((cc 0))
    (flet ((cc () (incf cc)))
      ;; Since "(cc)" is not constant,
      ;; The init form `(+ (cc) 2) will be used to initialize the value.
      (expect (equal '(3 . 3) (cons2+2 (cc))))
      (expect (= cc 1)))))

(defmacro fx+ (x y)
  (once-only (x y)
    (declare (fixnum x y))
    `(+ ,x ,y)))

(deftest once-only-fixnum ()
  (expect (= 10 (fx+ 5 5)))
  (let ((x 5)
        (y 5))
    (expect (= 10 (fx+ x y)))))

(defmacro 2*x (x y)
  (once-only (x y)
    (declare (fixnum x y) (ignore y))
    `(+ ,x ,x)))

(deftest once-only-ignore ()
  (expect (= 10 (2*x 5 3)))
  (let ((x 5)
        (y 5))
    (expect (= 10 (2*x x y)))))

(defmacro func2 (f &optional x y)
  (once-only ((f `(coerce (or ,f #'+) 'function))
              (x `(or ,x 0))
              (y `(or ,y 0)))
    (declare (fixnym x y) (inline f))
    `(funcall ,f ,x ,y)))

(deftest func2-test ()
  (expect (= 0 (func2 #'-)))
  (expect (= -2 (func2 (lambda (x y) (- y x)) 5 3))))

(defmacro func3 (f &optional x y)
  (once-only ((f `(coerce (or ,f #'+) 'function))
              (x `(or ,x 0))
              (y `(or ,y 0)))
    (declare (fixnym x y) (inline f))
    `(1+ (block nil (let ((x (funcall ,f ,x ,y))) (when x (return x)) -100)))))

(defun my- (a b) (- a b))

(deftest func3-test ()
  (expect (= 1 (func3 #'-)))
  (expect (= 1 (func3 '-)))

  (expect (= 0 (func3 'my- 1 2)))
  (expect (= 0 (func3 #'my- 1 2)))
  (flet ((my- (a b) (+ a b)))
    (expect (= 0 (func3 '- 1 2)))
    (expect (= 0 (func3 'my- 1 2)))
    (expect (= 4 (func3 #'my- 1 2))))

  (expect (= -1 (func3 (lambda (x y) (- y x)) 5 3)))
  ;; Returning from the outside block - no +1
  (expect (= -2 (block nil (func3 (lambda (x y) (return (- y x))) 5 3)))))

(defmacro fynd-if (pred list &rest args &key start end from-end key)
  (once-only ((pred `(coerce ,pred 'function))
              list
              &rest args
              &key
              (start `(or ,start 0))
              end
              from-end
              (key `(coerce (or ,key #'identity) 'function)))
    (declare (inline pred)
             (list list) (fixnum start) (type (or null fixnum) end)
             (ignore args))
    `(find-if ,pred ,list
              :start ,start
              ,@(when end `(:end ,end))
              ,@(when from-end `(:from-end ,from-end))
              ,@(when key `(:key ,key)))))

(deftest fynd-if-test ()
  (expect (eql 1 (fynd-if #'evenp '(0 1 2 3 4) :from-end nil :key #'1+)))
  (expect (equal '(find-if #'evenp '(0 1 2 3 4) :start 0 :key #'1+)
                 (macroexpand '(fynd-if 'evenp '(0 1 2 3 4) :from-end nil :key '1+)))))

(deftest fynd-if-test1 ()
  (let ((calls nil))
    (flet ((e (x) (push x calls) (+ 2 (length calls))))
      (expect
       (eql 3 (fynd-if #'evenp '(3 0 1 2 3 4)
                       :from-end nil :key #'1+
                       :end (e :e1) :end (e :e2))))
      (expect (equal '(:e1 :e2) (reverse calls))))))

(deftest fynd-if-test2 ()
  (let ((calls nil))
    (flet ((s (x) (push x calls) 1)
           (e (x) (push x calls) 3))
      (expect
       (eql 1 (fynd-if #'evenp '(3 0 1 2 3 4)
                       :from-end nil :key #'1+
                       :end (e :e1) :start (s :s1)
                       :start (s :s2) :end (e :e2)
                       :start 10 :end 20)))
      (expect (equal '(:e1 :s1 :s2 :e2) (reverse calls))))))

(deftest fynd-if-test3 ()
  (let ((calls nil))
    (flet ((s (x) (push x calls) 1)
           (e (x) (push x calls) 3))
      (expect
       (eql 1 (fynd-if #'evenp '(3 0 1 2 3 4)
                       :from-end nil :key #'1+
                       :start (s :s1) :end (e :e2)
                       :end (e :e3) :start (s :s4)
                       :start 10 :end 20
                       :start (s :s5) :end (e :e6))))
      (expect (equal '(:s1 :e2 :e3 :s4 :s5 :e6) (reverse calls))))))


(defmacro macro-with-rest+key (x y &rest rest &key a b &allow-other-keys)
  (once-only ((x `(or ,x (one)))
              (y `(or ,y (two)))
              &rest rest
              &key
              (a `(or ,a (three)))
              (b `(or ,b (four)))
              &allow-other-keys)
    (declare (fixnum x y a b)
             (ignore a b)
             (dynamic-extent rest))
    ;(format t "~A~%" rest)
    `(list ,x ,y ,@rest ,@rest)))

(deftest once-with-rest+key ()
  (expect (equal '(1 2) (macro-with-rest+key 1 2)))
  (expect (equal '(1 2 :a 30 :a 30) (macro-with-rest+key 1 2 :a 30)))
  (expect (equal '(1 2 :a 4 :a 4) (macro-with-rest+key 1 2 :a (four))))
  (expect (equal '(1 2 :c 4 :c 4) (macro-with-rest+key 1 2 :c (four))))

  (let (values)
    ;; macro-with-rest+key captures those local definitions.
    (flet ((one () (push 1 values) (length values))
           (two () (push 2 values) (length values))
           (three () (push 3 values) (length values))
           (four () (push 4 values) (length values)))
      (declare (ignore #'two))
      (expect (equal '(1 2) (macro-with-rest+key 1 2)))
      (expect (equal '(4 3) values))
      (expect (equal '(1 2 :a 30 :a 30) (macro-with-rest+key 1 2 :a 30)))
      (expect (equal '(4 4 3) values))

      (expect (equal '(1 2 :b 4 :b 4) (macro-with-rest+key 1 2 :b (one))))
      (expect (equal '(3 1 4 4 3) values))

      (expect (equal '(1 2 :c 6 :c 6) (macro-with-rest+key 1 2 :c (one))))
      (expect (equal '(4 3 1 3 1 4 4 3) values)))))


(defmacro macro-with-rest (x y &rest rest &key a b &allow-other-keys)
  (declare (ignore a b))
  (once-only ((x `(or ,x (one)))
              (y `(or ,y (two)))
              &rest rest)
    (declare (fixnum x y))
    ;(format t "~A~%" rest)
    `(list ,x ,y ,@rest ,@rest)))

(deftest once-with-rest ()
  (expect (equal '(1 2) (macro-with-rest 1 2)))
  (expect (equal '(1 2 :a 30 :a 30) (macro-with-rest 1 2 :a 30)))
  (expect (equal '(1 2 :a 4 :a 4) (macro-with-rest 1 2 :a (four))))

  (let (values)
    ;; macro-with-rest captures those local definitions.
    (flet ((one () (push 1 values) (length values))
           (two () (push 2 values) (length values)))
      (declare (ignore #'two))
      (expect (equal '(1 2) (macro-with-rest 1 2)))
      (expect (equal '() values))
      (expect (equal '(1 2 :a 30 :a 30) (macro-with-rest 1 2 :a 30)))
      (expect (equal '() values))
      (expect (equal '(1 2 :b 1 :b 1) (macro-with-rest 1 2 :b (one))))
      (expect (equal '(1) values)))))

(defmacro body-macro ((a b) &body body)
  (once-only (a b &body (body `(lambda (,a ,b) ,@body)))
    `(funcall ,body ,a ,b)))

(defmacro body-macro2 ((a b) &body body)
  (once-only (a b &body (body (a b)))
    `(funcall ,body ,a ,b)))

(deftest body-test ()
  (flet ((%call (y x)
           (body-macro (x y)
             (/ x y))))
    (expect (= 4 (%call 3 12))))

  (flet ((%call (y x)
           (body-macro2 (x y)
             (/ x y))))
    (expect (= 4 (%call 3 12)))))

;;;
;;; Define compiler macro.
;;;

(defun q (&key a b c) (list a b c))

(define-compiler-macro* q (&whole whole &key a b c)
  (if (or a b c)
      `(list ,a ,b ,c)
      (return whole)))

(defun fn-with-macro (x y &key (a 1) (b 2))
  (* 1/2 x y a b))

(define-compiler-macro* fn-with-macro
    (&whole whole x y &key (a `(or ,a 1) ap) (b `(or ,b 2)))
  (declare (fixnum x y  a b))
  (if ap `(* ,y ,x ,a ,a ,b) (return whole)))

(deftest compiler-macro*-simple ()
  (let ((x 0))
    (expect (equal '(2 1 3) (q :b (incf x) :a (incf x) :c (incf x)))))

  ;; The two forms below expand to:
  ;; (* 3 4 1 1 2)
  (expect (= 24 (fn-with-macro 4 3 :b 2 :a 1)))
  (expect (= 24 (fn-with-macro 4 3 :a 1)))
  (expect (equal '(* 3 4 1 1 2) (macroexpand* '(fn-with-macro 4 3 :a 1))))
  (locally (declare (notinline fn-with-macro))
    (expect (= 12 (fn-with-macro 4 3 :b 2 :a 1))))
  (let ((x 0))
    ;; The form below expands to:
    ;; (LET ((#:X-VAR-2050 (SETQ X (+ 1 X)))
    ;;       (#:Y-VAR-2051 (SETQ X (+ 1 X)))
    ;;       (#:B-VAR-2053 (OR (SETQ X (+ 1 X)) 2))
    ;;       (#:A-VAR-2055 (OR (SETQ X (+ 1 X)) 1))
    ;;       (#:B-VAR-2056 (SETQ X (+ 1 X))))
    ;;   (DECLARE
    ;;    (IGNORABLE #:X-VAR-2050 #:Y-VAR-2051 #:B-VAR-2053 #:A-VAR-2055 #:B-VAR-2056)
    ;;    (TYPE FIXNUM #:X-VAR-2050)
    ;;    (TYPE FIXNUM #:Y-VAR-2051)
    ;;    (TYPE FIXNUM #:B-VAR-2053)
    ;;    (TYPE FIXNUM #:A-VAR-2055))
    ;;   (* #:Y-VAR-2051 #:X-VAR-2050 #:A-VAR-2055 #:A-VAR-2055 #:B-VAR-2053))
    (expect (= (* 6 16) (fn-with-macro (incf x) (incf x) :b (incf x) :a (incf x) :b (incf x))))
    (expect (= 5 x)))
  (let ((x 0))
    (symbol-macrolet ((foo (incf x)))
      ;; The form below expands to:
      ;; (LET ((#:X-VAR-2144 FOO)
      ;;       (#:Y-VAR-2145 FOO)
      ;;       (#:B-VAR-2147 (OR FOO 2))
      ;;       (#:A-VAR-2149 (OR FOO 1)))
      ;;   (DECLARE (IGNORABLE #:X-VAR-2144 #:Y-VAR-2145 #:B-VAR-2147 #:A-VAR-2149)
      ;;            (TYPE FIXNUM #:X-VAR-2144)
      ;;            (TYPE FIXNUM #:Y-VAR-2145)
      ;;            (TYPE FIXNUM #:B-VAR-2147)
      ;;            (TYPE FIXNUM #:A-VAR-2149))
      ;;   (* #:Y-VAR-2145 #:X-VAR-2144 #:A-VAR-2149 #:A-VAR-2149 #:B-VAR-2147))
      (expect (= (* 6 16) (fn-with-macro foo foo :b foo :a foo :b foo))))
    (expect (= 5 x)))
  (let ((x 0))
    ;; Expands to WHOLE.
    (expect (= 3 (fn-with-macro (incf x) (incf x) :b (incf x) :b (incf x))))
    (expect (= 4 x))))

(defun fn-with-macro2 (x y &key (a 1) (b 2)) (* 1/2 x y a b))
(define-compiler-macro* fn-with-macro2
    (&whole whole x y &key (a `(or ,a (one)) ap) (b `(or ,b (two))))
  (declare (fixnum x y a b))
  (if (not ap) `(* ,y ,x ,a ,a ,b) (return whole)))

(deftest compiler-macro*-simple2 ()
  (expect (= 30 (fn-with-macro2 3 5)))

  (expect (= 30 (fn-with-macro2 3 5 :b 2)))
  (expect (= 30 (fn-with-macro2 3 5 :b (two))))

  (let ((x 3) (y 5))
    (expect (= 30 (fn-with-macro2 x y)))))

(defun fn-with-macro3 (x y &optional (a 1) (b 2)) (* 1/2 x y a b))
(define-compiler-macro* fn-with-macro3
    (&whole whole x y &optional (a `(or ,a (one))) (b `(or ,b (two)) bp))
  (declare (fixnum x y  a b))
  (if (not bp)
      `(* ,y ,x ,a ,a ,b)
      (return whole)))

(deftest compiler-macro*-simple3 ()
  (expect (= 30 (fn-with-macro3 3 5)))
  (expect (= 30 (fn-with-macro3 3 5 1)))
  (let ((x 3) (y 5) (a 1))
    (expect (= 30 (fn-with-macro3 x y)))

    (expect (= 30 (fn-with-macro3 3 5 a)))
    (expect (= 30 (fn-with-macro3 x y a)))))

(defun fn-with-macro4 (x) (list x x x))
(define-compiler-macro* fn-with-macro4 (x)
  `(list ,x ,x ,x))

(deftest compiler-macro4 ()
  (let ((x 0))
    (symbol-macrolet ((foo (incf x)))
      ;; The form below SEEMS to expand to:
      ;; (LIST FOO FOO FOO)
      ;; Yet this is because SLIME does not provide the right LEXENV.
      (expect (equal '(1 1 1) (fn-with-macro4 foo))))
    (expect (= 1 x))))

(defun fjnd-if (predicate list &rest args)
  (apply #'find-if predicate list args))

(define-compiler-macro* fjnd-if
    ((p `(coerce ,p 'function))
     list
     &key start end
     (key `(coerce (or ,key #'identity) 'function)))
  (declare (list list) (function p key)
           (dynamic-extent p key)
           (inline p key))
  `(find-if ,p ,list
            ,@(when start `(:start ,start))
            ,@(when end `(:end ,end))
            ,@(when key `(:key ,key))))

(deftest compiler-macro*-fns ()
  ;; The two forms below expand to:
  ;; (FIND-IF #'ODDP '(1 2 3 4) :KEY #'1+)
  ;; Which the compiler optimizes down to the number 2.
  (expect (= 2 (fjnd-if #'oddp '(1 2 3 4) :key #'1+)))
  (expect (= 2 (fjnd-if 'oddp '(1 2 3 4) :key '1+)))
  (expect (equal '(FIND-IF #'ODDP '(1 2 3 4) :KEY #'1+)
                 (macroexpand* '(fjnd-if 'oddp '(1 2 3 4) :key '1+))))

  ;; The form below expands to:
  ;; (FLET ((G-P-FN-3483 (X)
  ;;          (= X 3))
  ;;        (#:KEY-FN-3484 (X)
  ;;          (IF (= X 2)
  ;;              3
  ;;              X)))
  ;;   (DECLARE (DYNAMIC-EXTENT (FUNCTION #:P-FN-3483) (FUNCTION #:KEY-FN-3484)))
  ;;   (DECLARE (INLINE #:KEY-FN-3484 #:P-FN-483))
  ;;   (FIND-IF #'#:P-FN-3483 '(1 2 3 4) :START 1 :END 4 :KEY #'#:KEY-FN-3484))
  ;;
  (expect (= 2 (fjnd-if (lambda (x) (= x 3)) '(1 2 3 4)
                        :key (lambda (x) (if (= x 2) 3 x))
                        :end 4 :start 1 :end 2 :start 3)))

  (let ((list '(1 2 3 4)))
    ;; The form below expands to:
    ;; (LET ((#:LIST-VAR-1921 LIST))
    ;;   (DECLARE (IGNORABLE #:LIST-VAR-1921)
    ;;            (TYPE LIST #:LIST-VAR-1921))
    ;;  (FIND-IF #'ODDP #:LIST-VAR-1921 :KEY #'1+))
    (expect (= 2 (fjnd-if #'oddp list :key #'1+)))

    ;; The form below expands to:
    ;; (FLET ((#:P-FN-1975 (X)
    ;;          (= X 3))
    ;;        (#:KEY-FN-1977 (X)
    ;;          (IF (= X 2)
    ;;              3
    ;;              X)))
    ;;   (DECLARE (IGNORABLE (FUNCTION #:KEY-FN-1977))
    ;;            (DYNAMIC-EXTENT (FUNCTION #:P-FN-1975) (FUNCTION #:KEY-FN-1977))
    ;;            (INLINE #:P-FN-1975 #:KEY-FN-1977))
    ;;   (LET ((#:LIST-VAR-1976 LIST))
    ;;     (DECLARE (IGNORABLE #:LIST-VAR-1976)
    ;;              (TYPE LIST #:LIST-VAR-1976))
    ;;     (FIND-IF #'#:P-FN-1975 #:LIST-VAR-1976 :START 1 :END 4 :KEY
    ;;              #'#:KEY-FN-1977)))
    (expect (= 2 (fjnd-if (lambda (x) (= x 3)) list
                          :key (lambda (x) (if (= x 2) 3 x))
                          :end 4 :start 1 :end 2 :start 3)))))


(defun r (x y &optional (z 0))
  (sqrt (+ (* x x) (* y y) (* z z))))

(define-compiler-macro* r (x y &optional (z `(or ,z 0) zp))
  (declare (fixnum x y z))
  `(sqrt (+ (* ,x ,x) (* ,y ,y) ,@(when zp `((* ,z ,z))))))

(deftest compiler-macro*-optional ()
  (expect (= 5 (r 3 4)))
  (let ((x 3) (y 4))
    ;; The form below expands to:
    ;; (LET ((#:X-VAR-1930 X) (#:Y-VAR-1931 Y))
    ;;   (DECLARE (IGNORABLE #:X-VAR-1930 #:Y-VAR-1931)
    ;;            (TYPE FIXNUM #:X-VAR-1930)
    ;;            (TYPE FIXNUM #:Y-VAR-1931))
    ;;   (SQRT (+ (* #:X-VAR-1930 #:X-VAR-1930) (* #:Y-VAR-1931 #:Y-VAR-1931))))
    (expect (= 5 (r x y))))

  (expect (= 13 (r 3 4 12)))
  (let ((x 3) (y 4) (z 12))
    ;; The form below expands to:
    ;; (LET ((#:X-VAR-1945 X) (#:Y-VAR-1946 Y) (#:Z-VAR-1948 (OR Z 0)))
    ;;   (DECLARE (IGNORABLE #:X-VAR-1945 #:Y-VAR-1946 #:Z-VAR-1948)
    ;;            (TYPE FIXNUM #:X-VAR-1945)
    ;;            (TYPE FIXNUM #:Y-VAR-1946)
    ;;            (TYPE FIXNUM #:Z-VAR-1948))
    ;;   (SQRT
    ;;    (+ (* #:X-VAR-1945 #:X-VAR-1945) (* #:Y-VAR-1946 #:Y-VAR-1946)
    ;;       (* #:Z-VAR-1948 #:Z-VAR-1948))))
    (expect (= 13 (r x y z)))))

(defun r2 (&key (x 0) (y 0))
  (sqrt (+ (* x x) (* y y))))

(define-compiler-macro* r2 (&rest rest &key
                                 (x `(or ,x 0))
                                 (y `(or ,y 0)))
  (declare (fixnum x y))
  `(sqrt (+ (* ,x ,x) (* ,y ,y) (length ',rest))))


(defun r3 (&key (x 0) (y 0) &allow-other-keys)
  (sqrt (+ (* x x) (* y y))))

(define-compiler-macro* r3 (&rest rest &key
                                 (x `(or ,x 0))
                                 (y `(or ,y 0))
                                 &allow-other-keys)
  (declare (fixnum x y))
  `(sqrt (+ (* ,x ,x) (* ,y ,y) (length ',rest))))

(deftest compiler-macro-allow-other-keys ()
  (expect (= 2 (r2 :allow-other-keys t :a 1)))
  (expect-macro-error
   (r2 nil :a 1))
  (expect-macro-error
   (r2 :allow-other-keys nil :a 1))
  (expect (= 2 (r3 :b 2 :a 1))))


(defun r4 (&rest nums)
  (* 2 (sqrt (apply #'+ (mapcar #'1+ nums)))))

(define-compiler-macro* r4 (&rest (nums (lmap (n nums) `(1+ ,n))))
  (declare (fixnum nums))
  `(+ (sqrt (+ ,@nums)) (sqrt (+ ,@nums))))

(deftest compiler-macro-r4 ()
  (expect (= 6 (r4 1 2 3)))
  (expect (equal '(+ (sqrt (+ 2 3 4)) (sqrt (+ 2 3 4))) (macroexpand* '(r4 1 2 3))))
  (let ((x 1) (y 2) (z 3))
    (expect (= 6 (r4 x y z))))

  (let ((x 0))
    (flet ((x () (incf x)))
      (expect (= 6 (r4 (x) (x) (x))))
      (expect (= 3 x)))))
