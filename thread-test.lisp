;;; Copyright 2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

;;;;
;;;; Tests for the ace.core.thread package.
;;;;

(defpackage #:ace.core.thread-test
  (:use #:common-lisp
        #:ace.core.thread
        #:ace.test)
  (:import-from #:ace.core.thread
                ace.core.thread::holding-mutex-p)
  (:import-from #:sb-ext
                sb-ext:atomic-incf))

(in-package #:ace.core.thread-test)

;; TODO(czak): Investigate if we can use LPARALLEL for this.

(defmacro pprog1 (form1 &rest forms)
  "Take each form and execute in a separate simple thread. Joins threads at the end.
 Returns the result of FORM1 which is executed in the current thread."
  `(let* ((functions (list ,@(loop for form in forms collect `(lambda () ,form))))
          (threads (mapcar #'make-thread functions)))
     ;; TODO(b/72035110): Cannot stack-allocate in opt.
     ;; (declare (dynamic-extent functions))
     (prog1 ,form1
       (mapc #'join-thread threads))))

(defvar *thread-var* 0)

(deftest test-pprog1 ()
  (expect (= 1 (pprog1 1)))
  (loop repeat 100 do
    (let ((test (list 0)))
      (expect (<= 0 (pprog1
                     (atomic-incf (car test))
                     (atomic-incf (car test) 2)
                     (atomic-incf (car test) 4)
                     (atomic-incf (car test) 8))
                 (car test)))
      (expect (= (car test) 15)))))

(deftest test-current-thread ()
  (loop repeat 100 do
    (pprog1
     (expect (eq (current-thread) (current-thread)))
     (expect (eq (current-thread) (current-thread)))
     (expect (eq (current-thread) (current-thread)))
     (expect (eq (current-thread) (current-thread))))))

(deftest test-recursive-mutex ()
  (let ((mutex (make-mutex "TEST"))
        (count 0))
    (declare (fixnum count))
    (flet ((increment () (with-mutex (mutex :reenter nil)
                           (with-mutex (mutex :protect nil)
                             (incf count)))))
      (loop repeat 100 do
        (pprog1 (increment)
                (increment)
                (increment)
                (increment)
                (increment))))
    (expect (= count 500))))

(deftest test-frmutex ()
  (let ((mutex (make-frmutex "FR"))
        (count1 0)
        (count2 0))
    (declare (fixnum count1 count2))
    (flet ((increment1 () (with-frmutex-write (mutex)
                           (incf count1)
                           (sleep 0.001)
                           (incf count2)))
           (increment2 () (with-frmutex-write (mutex :reenter nil)
                           (incf count1)
                           (sleep 0.001)
                           (incf count2)))
           (test ()
             (expect (with-frmutex-read (mutex)
                       (= count1 count2)))))

      (loop repeat 100 do
        (pprog1 (test)
                (test)
                (increment1)
                (increment2)
                (test)
                (test))))
    (expect (= count1 count2 200))))

(deftest test-unnamed-frmutex ()
  (let ((mutex (make-frmutex)))
    (expect mutex)))

(deftest test-unprotected-mutex-safe ()
  (declare (optimize (safety 3)))
  (let ((mutex (make-mutex "TEST")))
    (expect-error
      (catch :foo
        (with-mutex (mutex :protect nil)
          (throw :foo :no-error))))))

(deftest test-unprotected-mutex-unsafe ()
  (declare (optimize (safety 0)))
  (let ((mutex (make-mutex "TEST")))
    (expect (not (holding-mutex-p mutex)))
    (expect
     (eq (catch :foo
           (with-mutex (mutex :protect nil)
             (expect (holding-mutex-p mutex))
             (throw :foo :no-error)))
         :no-error))
    (expect (holding-mutex-p mutex))))


(deftest test-with-value-computed-once ()
  (let ((promise (make-promise "once"))
        (count0 (list 0))
        (count1 0)
        (count2 0)
        (count3 (list 0)))
    (declare (fixnum count1 count2))
    (flet ((increment ()
             (atomic-incf (car count0))
             (when (eql 42 (with-value-computed-once (promise)
                             (incf count1)
                             (sleep 0.001)
                             (incf count2)
                             42))
               (atomic-incf (car count3)))))
      (mapc #'join-thread
            (loop :repeat 100
                  :collect (make-thread #'increment))))
    (expect (promise-fulfilled-p promise))
    (expect (= (promise-value promise) 42))
    (expect (= (car count0) 100))
    (expect (= (car count3) 100))
    (expect (= count1 count2 1))))


(deftest stack-allocated-test ()
  (expect (not (stack-allocated-p 1)))
  (let ((c (cons 1 2)))
    (expect (not (stack-allocated-p c))))
  (let ((c (cons 1 2)))
    (declare (dynamic-extent c))
    (expect (stack-allocated-p c))))

(defvar *always-heap-p-stream* t)
;;; Prior to putting WITHOUT-ARENA into (BACKTRACES) it could have crashed due to
;;; cross-arena pointers depending on whether each interrupted thread and the
;;; interruptor were using arenas.
#+sbcl
(deftest backtrace-string-not-to-arena ()
  ;; It doesn't matter when this ENCAPSULATE happens (if tests execute
  ;; concurrently) because it's a safe change to make to the function.
  (sb-int:encapsulate 'ace.core.thread::print-backtrace 'arena-sanitizer
    (lambda (realfun &key (stream *debug-io*))
      ;; A failing ASSERT inside backtrace is pretty bad. Don't do that-
      ;; just set a flag saying the test failed
      (unless (sb-ext:heap-allocated-p stream)
        (setf *always-heap-p-stream* nil))
      (apply realfun :stream stream)))
  (let ((runflag t))
    (labels ((get-deeply-in-stack (levels)
               ;; Recurse so that there are a bunch of frames to print, the idea
               ;; being to force the string-stream buffer to grow.
               (cond ((plusp levels)
                      (get-deeply-in-stack (1- levels))
                      (sleep (random .00001))) ; so not a tail call
                     (t
                      (print 'foo (make-broadcast-stream)))))
             (use-arena-and-start (&rest args)
               ;; Can't switch from arena to arena, so get out of the main arena
               ;; that was inherited from the creating thread
               (sb-vm:unuse-arena)
               (let ((arena (sb-vm:new-arena (* 1024 1024 10)))) ; 10 MiB
                 (loop while runflag
                       do (sb-vm:with-arena (arena)
                            (apply #'get-deeply-in-stack args))
                          (sb-vm:rewind-arena arena)))))
    (let ((main-arena (sb-vm:new-arena (* 1024 1024 10)))) ; 10 MiB
      (sb-vm:with-arena (main-arena)
        (let* ((nthreads 5)
               (threads (loop repeat nthreads for id from 0
                              collect (sb-thread:make-thread
                                       #'use-arena-and-start
                                       :arguments (list 40)
                                       :name (format nil "worker ~d" id)))))
          (dotimes (i 10)
            (ace.core.thread:backtraces))
          (setq runflag nil)
          (mapc 'sb-thread:join-thread threads))))))
  (expect *always-heap-p-stream*))
