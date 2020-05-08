
(google.core.package:defpackage* ace.core.hook-test
  (:use #:cl #:google.test)
  (:use-alias #:google.core.hook)
  (:import-from #:ace.core.functional #:∘)
  (:import-from #:google.core.hook
                #:define-hook-function
                #:define-hook-method
                #:method-identifier
                #:method-number
                #:method-after
                #:method-before
                #:method-around
                #:node-methods
                #:parse-method-qualifiers
                #:sort-and-combine-methods))

(in-package #:ace.core.hook-test)

(defun make-hook-method (qualifiers)
  (let ((m (make-instance 'hook::hook-method
                          :function #'identity
                          :qualifiers qualifiers)))
    (parse-method-qualifiers m)
    m))

(deftest parse-method-qualifiers-fail-test ()
  (flet ((test (qualifiers)
           (expect-warning (make-hook-method qualifiers))))
    (test '())
    (test '(1))
    (test '(foo 1000 :before x))
    (test '(foo bar))
    (test '(foo :around 1 2))
    (test '(foo :before 1 2 3))
    (test '(foo :after 10))))

(deftest parse-method-qualifiers-around-test ()
  (let ((m (make-hook-method '(test :around foo bar baz))))
    (expect (eq (method-identifier m) 'test))
    (expect (equal '(foo bar baz) (method-around m)))
    (expect (zerop (method-number m)))
    (expect (not (method-before m)))
    (expect (not (method-after m))))

  (let ((m (make-hook-method '(test :around))))
    (expect (eq (method-identifier m) 'test))
    (expect (equal t (method-around m)))))

(deftest parse-method-qualifiers-before-after-test ()
  (let ((m (make-hook-method '(test :before foo :after bar baz))))
    (expect (eq (method-identifier m) 'test))
    (expect (not (method-around m)))
    (expect (zerop (method-number m)))
    (expect (equal '(bar baz) (method-after m)))
    (expect (equal '(foo) (method-before m))))
  (let ((m (make-hook-method '(test :after bar baz :before foo))))
    (expect (eq (method-identifier m) 'test))
    (expect (not (method-around m)))
    (expect (zerop (method-number m)))
    (expect (equal '(bar baz) (method-after m)))
    (expect (equal '(foo) (method-before m))))
  (let ((m (make-hook-method '(test :after bar baz :before))))
    (expect (eq (method-identifier m) 'test))
    (expect (not (method-around m)))
    (expect (zerop (method-number m)))
    (expect (equal '(bar baz) (method-after m)))
    (expect (equal '() (method-before m))))
  (let ((m (make-hook-method '(test :after :before foo))))
    (expect (eq (method-identifier m) 'test))
    (expect (zerop (method-number m)))
    (expect (not (method-around m)))
    (expect (equal '() (method-after m)))
    (expect (equal '(foo) (method-before m)))))

(deftest parse-method-qualifiers-number-test ()
  (let ((m (make-hook-method '(test 1000))))
    (expect (eq (method-identifier m) 'test))
    (expect (= 1000 (method-number m)))
    (expect (not (method-around m)))
    (expect (not (method-before m)))
    (expect (not (method-after m)))))

(deftest parse-method-qualifiers-default-test ()
  (let ((m (make-hook-method '(test))))
    (expect (eq (method-identifier m) 'test))
    (expect (= 0 (method-number m)))
    (expect (not (method-around m)))
    (expect (not (method-before m)))
    (expect (not (method-after m)))))

(deftest sort-and-combine-methods-test ()
  (let* ((methods (list (make-hook-method '(primary))
                        (make-hook-method '(before :before primary))
                        (make-hook-method '(after :after primary))
                        (make-hook-method '(around :around primary))))
         (nodes (sort-and-combine-methods methods)))
    (expect (equal '(before around after)
                   (mapcar (∘ #'method-identifier #'first #'node-methods) nodes)))
    (expect (eq 'primary (method-identifier (second (node-methods (second nodes)))))))

  (let* ((methods (list (make-hook-method '(primary))
                        (make-hook-method '(minus -1))
                        (make-hook-method '(plus 1))
                        (make-hook-method '(around :around primary))))
         (nodes (sort-and-combine-methods methods)))
    (expect (equal '(minus around plus)
                   (mapcar (∘ #'method-identifier #'first #'node-methods) nodes)))
    (expect (eq 'primary (method-identifier (second (node-methods (second nodes))))))))

(define-hook-function test-hooks1 (arg))

(declaim (list *test-hook-list*))
(defvar *test-hook-list*)

(defmethod test-hooks1 qualified1 ((arg (eql :test-qualified)))
  (push :qualified1 *test-hook-list*))

(defmethod test-hooks1 primary (arg)
  (expect (not (next-method-p)))
  (push :primary *test-hook-list*))

(defmethod test-hooks1 primary ((arg (eql :test-primary-call-next-method)))
  (push :qualified-primary-enter *test-hook-list*)
  (expect (next-method-p))
  (call-next-method) ; -> the unqualified primary
  (push :qualified-primary-exit *test-hook-list*))

(defmethod test-hooks1 qualified2 ((arg (eql :test-qualified)))
  (push :qualified2 *test-hook-list*))

(defmethod test-hooks1 before-primary :before primary (arg)
  (push :before-primary *test-hook-list*))

(defmethod test-hooks1 after-primary :after primary (arg)
  (push :after-primary *test-hook-list*))

(defmethod test-hooks1 around-primary :around primary ((arg (eql :test-around)))
  (push :around-primary-enter *test-hook-list*)
  (expect (next-method-p))
  (call-next-method)
  (push :around-primary-exit *test-hook-list*))

(defmethod test-hooks1 minus -1 ((arg (eql :test-numeric)))
  (push -1 *test-hook-list*))

(defmethod test-hooks1 plus 1 ((arg (eql :test-numeric)))
  (push 1 *test-hook-list*))

(defmethod test-hooks1 qualified3 ((arg (eql :test-qualified)))
  (push :qualified3 *test-hook-list*))

(defmethod test-hooks1 around-qualified :around qualified3 (arg)
  (push :around-qualified3-enter *test-hook-list*)
  (expect (next-method-p))
  (call-next-method)
  (push :around-qualified3-exit *test-hook-list*))

(deftest test-hook-around ()
  (let ((*test-hook-list* nil))
    (test-hooks1 :test-around)
    (expect (equal '(:before-primary
                     :around-primary-enter
                     :primary
                     :around-primary-exit
                     :after-primary)
                   (reverse *test-hook-list*)))))

(deftest test-hook-numeric ()
  (let ((*test-hook-list* nil))
    (test-hooks1 :test-numeric)
    (expect (equal '(-1
                     :before-primary
                     :primary
                     :after-primary
                     1)
                   (reverse *test-hook-list*)))))

(deftest test-hook-qualified-primary ()
  (let ((*test-hook-list* nil))
    (test-hooks1 :test-qualified)
    ;; The last defined methods come before earlier defined ones.
    (expect (equal '(:around-qualified3-enter
                     :qualified3
                     :around-qualified3-exit
                     :qualified2
                     :qualified1
                     :before-primary
                     :primary
                     :after-primary)
                   (reverse *test-hook-list*)))))

(deftest test-hook-primary-call-next-method ()
  (let ((*test-hook-list* nil))
    (test-hooks1 :test-primary-call-next-method)
    (expect (equal '(:before-primary
                     :qualified-primary-enter
                     :primary
                     :qualified-primary-exit
                     :after-primary)
                   (reverse *test-hook-list*)))))

(define-hook-function before-after-hooks ())

(defmethod before-after-hooks seventh :after fifth sixth second first ()
  (push 7 *test-hook-list*))

(defmethod before-after-hooks eigth :before :after seventh ()
  (push 8 *test-hook-list*))

(defmethod before-after-hooks first ()
  (push 1 *test-hook-list*))

(defmethod before-after-hooks second :before third fourth :after first ()
  (push 2 *test-hook-list*))

(defmethod before-after-hooks third :before fourth fifth :after first ()
  (push 3 *test-hook-list*))

(defmethod before-after-hooks fifth :after :before sixth seventh ()
  (push 5 *test-hook-list*))

(deftest before-after-hooks-test ()
  (let ((*test-hook-list* nil))
    (before-after-hooks)
    (expect (equal '(1 2 3 5 7 8)
                   (reverse *test-hook-list*)))))

(define-hook-function test-hooks2 ()
  :operator list)

(define-hook-method test-hooks2 primary ()
  :primary)

(define-hook-method test-hooks2 around-primary :around primary ()
  (list :enter (call-next-method) :exit))

(define-hook-method test-hooks2 after :after primary ()
  :after)

(define-hook-method test-hooks2 before :before primary ()
  :before)

(deftest define-hook-method-point2-test ()
  (expect (equal '(:before (:enter :primary :exit) :after)
                 (test-hooks2))))

(defun applicable-methods (gf)
  (mapcar #'method-identifier (compute-applicable-methods gf '())))

(deftest clear-test ()
  (let ((m (make-instance 'hook::hook-method
                          :function (constantly 2)
                          :qualifiers '(foo))))
    (add-method #'test-hooks2 m)
    (expect (equal '(foo before after around-primary primary) (applicable-methods #'test-hooks2)))
    (hook:clear #'test-hooks2 'foo)
    (expect (equal '(before after around-primary primary) (applicable-methods #'test-hooks2)))))

(define-hook-function test-around (x y))

(defmethod test-around t :around ((x float) y) :top-around)

(defmethod test-around t :around ((x integer) y) (1+ (call-next-method)))

(defmethod test-around internal (x y) (+ x y))

(deftest test-around-test ()
  (expect (eq :top-around (test-around 1.0 2.0)))
  (expect (= 4 (test-around 1 2))))
