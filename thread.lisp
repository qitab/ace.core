;;; Copyright 2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

;;;; Utilities related to threads and synchronization.
;;;; Uses the implementation detail of bordeaux-threads for make-thread
;;;; This package is designed to be used in other packages.
;;;;

;; TODO(czak): Change this package to be used as alias prefixed package.
(defpackage #:ace.core.thread
  (:use #:common-lisp
        #:ace.core.defun
        #:ace.core
        #:ace.core.macro
        #:ace.core.hook
        #:ace.core.etc)
  (:import-from #:bordeaux-threads
                ;; make-thread is overridden.
                #:thread
                #:join-thread
                #:interrupt-thread
                #:destroy-thread
                #:current-thread
                #:thread-yield
                #:threadp
                #:thread-name
                #:thread-alive-p
                #:all-threads
                #:with-recursive-lock-held
                #:with-timeout
                #:timeout
                #:acquire-lock
                #:release-lock)
  #+sbcl
  (:import-from #:sb-ext
                sb-ext:atomic-incf
                sb-ext:atomic-decf
                sb-ext:stack-allocated-p
                sb-ext:word)
  #+sbcl
  (:import-from #:sb-thread
                sb-thread:barrier
                sb-thread:mutex-name)
  #+sbcl
  (:import-from #:sb-sys
                sb-sys:without-interrupts
                sb-sys:allow-with-interrupts
                sb-sys:with-local-interrupts)
  (:export
   ;; Also exported from ACE.CORE.
   #:thread #:threadp
   #:make-thread #:make
   #:thread-name #:name
   #:current-thread #:current
   #:join-thread #:join
   #:interrupt-thread #:interrupt
   #:destroy-thread #:destroy
   #:all-threads
   #:thread-alive-p #:alivep
   #:yield
   #:run
   #:with-timeout
   #:timeout

   #:stack-allocated-p

   #:make-mutex #:mutex
   #:mutex-name
   #:mutex-error
   #:with-mutex
   #:with-mutex-once
   #:with-unprotected-mutex
   #:with-unprotected-mutex-once
   #:acquire-lock
   #:release-lock
   #:barrier

   #:promise
   #:make-promise
   #:promise-fulfilled-p
   #:promise-value
   #:fulfill-promise
   #:with-value-computed-once

   #:frmutex
   #:make-frmutex
   #:with-frmutex-read
   #:with-frmutex-write

   #:backtraces
   #:print-backtraces))

(in-package #:ace.core.thread)

#-(or armedbear clisp ccl ecl sbcl lispworks allegro scl)
(error "unsupported Lisp implementation")
;; corman, mcl - have no thread-join.
;; cmucl       - has no recursive (balanced) locks.
;; lispworks, allegro, scl - no test access to commercial Lisps.

;;;
;;; Stuff missing from the bordeaux-threads library.
;;;

(deftype mutex ()
  "A mutual exclusion variable type for which multiple locks can be acquired in the same thread.
 This allows reentrant critical sections to be coded.
 Related:
  thread:make-recursive-mutex
  thread:with-recursive-lock-held"
  #+armedbear 'bordeaux-threads::mutex-recursive
  #+allegro   'mp:process-lock
  #+clisp     'mt:mutex
  #+ccl       'ccl:lock
  #+cmu       'mp:lock
  #+ecl       'mp:lock
  #+lispworks 'mp:lock
  #+sbcl      'sb-thread:mutex
  #+scl       'thread:lock)

(define-hook-function run (function &rest arguments)
  :documentation
  "This runs the FUNCTION with ARGUMENTS for each thread.")

(defmethod run default (function &rest arguments)
  ;; Default method calls the function on arguments.
  (declare (function function) (list arguments))
  (if arguments (apply function arguments) (funcall function)))

(defun make-thread (function &key (name "anonymous") arguments)
  "Creates a new thread that executes the FUNCTION.
Optionally, one can specify the NAME of the thread and the ARGUMENTS.
If VARS is specified, or there have been thread special variables declared
in the context of the current thread, those variables will be rebound in
the new thread created.

Parameters:
  FUNCTION  - the function executed in the thread context.
  NAME      - the name for the thread (default: anonymous).
  ARGUMENTS - a list of arguments for the function.

Example:
  (make-thread (lambda (arg) (format t \"FOO:~A~%\" arg))
       :name \"FOO\" :arguments '(42))

Related:
  thread:make-thread*
  bordeaux-threads:make-thread
"
  (declare (function function) (string name))
  #+sbcl
  (sb-thread:make-thread
   #'run :name name :arguments (list* function arguments))
  #-sbcl
  (bordeaux-threads::make-thread
   (lambda () (apply #'run function arguments))
   :name name :initial-bindings nil))

;;; Aliases
;;; TODO(czak): Replace the use of thread functions
;;;  with the shorter aliases.
(defalias make make-thread)
(defalias name thread-name)
(defalias current current-thread)
(defalias join join-thread)
(defalias interrupt interrupt-thread)
(defalias destroy destroy-thread)
(defalias alivep thread-alive-p)
(defalias yield thread-yield)

;;; Mutual exclusion locks.

(define-condition mutex-error (simple-error error) ()
  (:documentation "A type of SIMPLE-ERROR used to signal mutex errors."))

(defun* make-mutex (&optional name)
  "Makes a recursive mutex with optional NAME."
  (declare (self inline (&optional (or null string)) mutex))
  (bordeaux-threads:make-recursive-lock name))

(defun* holding-mutex-p (mutex)
  "True if the current thread owns the MUTEX."
  #+sbcl (sb-thread:holding-mutex-p mutex)
  #+ccl  (eq (ccl::%lock-owner mutex) (current-thread))
  #-(or ccl sbcl)
  (assert nil nil "Unimplemented: HOLDING-MUTEX-P"))

(defmacro with-mutex ((mutex &key (lock t) (reenter t) (protect t) (inline :default))
                      &environment env &body body)
  "Defines BODY as a critical section that is guarded by MUTEX.
If MUTEX is NIL at runtime, the BODY is executed without locking.
The mutex is locked if the LOCK form evaluates to non-nil at runtime.

The following options are evaluated at compile-time only:
If INLINE is T, the BODY is inlined instead of being encapsulated in a local function.
If REENTER is NIL and the MUTEX is owned by the current thread, a MUTEX-ERROR is signaled.
If PROTECT is NIL, the mutex is released without the safety of UNWIND-PROTECT.
Note that PROTECT equal NIL, does not even protect from interrupts,
so even if the code surrounded by the WITH-MUTEX form never exits non-locally,
there is still possibility that the MUTEX remains locked resulting in an undefined state."
  (check-type reenter boolean)
  (check-type protect boolean)
  (check-type inline (member t nil :default))
  (let* ((lock-always-false
          (or (and (constantp lock env) (not (eval lock)))     ; NOLINT
              (and (constantp mutex env) (not (eval mutex))))) ; NOLINT
         (lock-form
          (if protect
              (if reenter 'with-recursive-lock-held 'with-mutex-once)
              (if reenter 'with-unprotected-mutex 'with-unprotected-mutex-once)))
         (inline (if (eq inline :default)
                     (and (zerop (space-level env)) (= 3 (speed-level env)))
                     inline)))
    (cond (lock-always-false
           `(locally ,@body))
          (t
           (once-only (mutex lock)
             `(flet ((&body () ,@body))
                (declare (dynamic-extent #'&body))
                ,@(when inline `((declare (inline &body))))
                (if (and ,lock ,mutex)
                    (,lock-form (,mutex) (&body))
                    (&body))))))))

(defmacro with-mutex-once ((mutex) &environment env &body body)
  "Executes BODY while holding the MUTEX.
Checks that the current thread is NOT the owner of the MUTEX."
  (once-only (mutex)
    `(progn
       ,@(unless (zerop (safety-level env))
           `((when (holding-mutex-p ,mutex)
               (error 'mutex-error "Cannot lock ~A again." ,mutex))))
       (with-recursive-lock-held (,mutex)
         (locally ,@body)))))

(defmacro with-unprotected-mutex ((mutex &key (lock t) (reenter t))
                                  &environment env &body body)
  "Like WITH-MUTEX but without an (expensive) UNWIND-PROTECT.
In safe mode, this actually does an UNWIND-PROTECT and CHECKS for a nonlocal exit.
The MUTEX is locked if the LOCK form evaluates to non-nil at runtime.
Also, if MUTEX is NIL at runtime, the BODY is executed without locking.
Note, this is inherently dangerous macro as it does not even handle interrupts properly.

The following options are evaluated at compile-time only:
If REENTER is NIL and the MUTEX is owned by the current thread, a MUTEX-ERROR is signaled."
(check-type reenter boolean)
(once-only (mutex lock)
  (flet ((form (&rest rest)
           (with-gensyms (%got-it)
             `(let ((,%got-it
                     (and (and ,lock ,mutex)
                          ,@(cond (reenter
                                   `((not (holding-mutex-p ,mutex))))
                                  ((plusp (safety-level env))
                                   `((or (not (holding-mutex-p ,mutex))
                                         (error 'mutex-error "Cannot lock ~A again." ,mutex)))))
                          (acquire-lock ,mutex))))
                (multiple-value-prog1
                    (locally ,@body)
                  (when ,%got-it (release-lock ,mutex))
                  ,@rest)))))
    (if (plusp (safety-level env))
        (with-gensyms (%unwind)
          `(let ((,%unwind t))
             (unwind-protect ,(form `(setf ,%unwind nil))
               (when ,%unwind
                 (error 'mutex-error "Unwind with an unprotected mutex ~A!" ,mutex)))))
        (form)))))

(defmacro with-unprotected-mutex-once ((mutex &key (lock t)) &body body)
  "Like WITH-MUTEX-ONCE but without an (expensive) UNWIND-PROTECT.
The mutex is locked if the LOCK form evaluates to non-nil at runtime.
In safe mode, this actually does an UNWIND-PROTECT and CHECKS for a nonlocal exit."
  (once-only (mutex lock)
    `(with-unprotected-mutex (,mutex :lock ,lock :reenter nil)
       (locally ,@body))))

;;;
;;; Extended mutex concepts
;;;

(deftype promise () `(cons (or mutex null) t))

(declaim (inline make-promise))
(defun make-promise (&optional (name "promise"))
  "Return a PROMISE with optional NAME."
  (cons (make-mutex name) nil))

(declaim (inline promise-fulfilled-p))
(defun promise-fulfilled-p (promise)
  "True if PROMISE has been fulfilled and the value is ready."
  (declare (promise promise))
  (null (car promise)))

(declaim (inline fulfill-promise))
(defun fulfill-promise (promise value)
  "Assigns a VALUE to PROMISE.
The caller must hold to promise MUTEX.
The promise must not be fulfilled, yet."
  (declare (promise promise))
  (dcheck (not (promise-fulfilled-p promise)))
  (dcheck (holding-mutex-p (car promise)))
  (setf (cdr promise) value
        (car promise) nil)
  value)

(declaim (inline promise-value))
(defun promise-value (promise)
  "Return the value of the PROMISE that was assigned by FULFILL-PROMISE."
  (dcheck (promise-fulfilled-p promise))
  (cdr promise))

(defun %with-value-computed-once (promise body)
  (declare (promise promise) (function body))
  ;; Check if promise is not fulfilled, yet?
  ;; If promise is fulfilled, (car promise) will be NIL.
  (clet ((mutex (car promise)))
    (with-unprotected-mutex (mutex)
      ;; Check once more for computation finished,
      ;; as multiple threads would pass the first test,
      ;; and only the first thread needs to do the computation.
      (unless (promise-fulfilled-p promise)
        (fulfill-promise promise (funcall body)))))
  (dcheck (promise-fulfilled-p promise))
  ;; Return results.
  (promise-value promise))

(defmacro with-value-computed-once ((promise) &body body)
  "Compute the value of the BODY only once and store it in the PROMISE.

With multiple threads only one of the threads will execute BODY and
the its value is stored in the PROMISE. Other threads, either read
the promise value or wait until it is available.

After the BODY has been executed the resulting value is used to
fulfill the PROMISE. It is an error to fulfill the PROMISE within
the BODY.

The synchronization mechanism uses WITH-UNPROTECTED-MUTEX.

Example:
  (defvar *promise* (thread:make-promise))
  (defun the-answer ()
     (with-value-computed-once (*promise*)
        (init-deep-thought)
        (loop :repeat 42000 :sum 0.001)))
  ;;
  ;; If THE-ANSWER function is called from multiple threads,
  ;; only one thread will calculate the value,
  ;; while other threads will wait for the first one.
  ;; (promise-value *promise*) will hold the value afterwards.
"
  (once-only (promise (body `(lambda () ,@body)))
    (declare (dynamic-extent body) (inline body))
    `(%with-value-computed-once ,promise ,body)))


;;;
;;; Writer/reader transaction.
;;;

;;
;; This implements a fast read mutex - a mutex that does not lock on reads.
;; This is done by keeping counters on the FRMUTEX structure.
;; The counters are changed by WITH-FRMUTEX-WRITE atomically.
;; They are only equal when no write takes place.
;;
;; The macro WITH-FRMUTEX-READ will loop forever until those counters are equal.
;;
;; Note, that the fast read mutex does not assure safety.
;; It only assures consistency. Should a data structure (e.g. a hash-table) get into
;; an invalid state, while the readers are trying to read it,
;; undefined behavior will be the result.
;;

(declaim (inline %make-frmutex make-frmutex))
(defstruct (frmutex (:constructor %make-frmutex))
  "A fast reader mutex. Allows the readers to read without locking."
  (mutex (make-mutex) :type mutex :read-only t)
  (pre-count 0 :type word)
  (post-count 0 :type word))

(defun make-frmutex (&optional name)
  "Make a FAST-READ-MUTEX with the optional NAME."
  (%make-frmutex :mutex (make-mutex name)))

(defmacro with-frmutex-read ((fast-read-mutex) &body body)
  "Execute BODY forms until the FAST-READ-MUTEX indicates that no write took place.
This version will only return the first value of BODY."
  (once-only (fast-read-mutex)
    (with-gensyms (%count %result)
      `(loop for ,%count of-type word
               = (prog1 (frmutex-post-count ,fast-read-mutex) (barrier (:read)))
             for ,%result = (locally ,@body)
             until (= ,%count (progn (barrier (:read)) (frmutex-pre-count ,fast-read-mutex)))
             finally (return ,%result)))))

(defmacro with-frmutex-write ((fast-read-mutex
                               &key (lock t) (reenter t) (protect t))
                              &environment env &body body)
  "Executes BODY forms locking the FAST-READ-MUTEX for writing.
The mutex is locked if the LOCK form evaluates to non-nil at runtime.
The locking is done only for the other writers,
while the readers do busy waiting with reevaluation.

The following options are evaluated at compile-time only:
If REENTER is NIL and the MUTEX is owned by the current thread, a MUTEX-ERROR is signaled.
If PROTECT is NIL, the mutex is released without the safety of UNWIND-PROTECT.
Note that PROTECT equal NIL, does not even protect from interrupts,
so even if the code surrounded by the WITH-MUTEX form never exits non-locally,
there is still possibility that the MUTEX remains locked resulting in an undefined state."
  (check-type reenter boolean)
  (check-type protect boolean)
  (once-only (fast-read-mutex lock)
    (if protect
        (with-gensyms (%mutex %got-it)
          `(without-interrupts
             (let ((,%got-it nil)
                   (,%mutex (frmutex-mutex ,fast-read-mutex)))
               (unwind-protect
                 (progn
                   (setf
                    ,%got-it
                    (and ,lock
                         ,@(cond (reenter
                                  `((not (holding-mutex-p ,%mutex))))
                                 ((plusp (safety-level env))
                                  `((or (not (holding-mutex-p ,%mutex))
                                        (error 'mutex-error "Cannot lock ~A again." ,%mutex)))))
                         (allow-with-interrupts (acquire-lock ,%mutex))))
                   (atomic-incf (frmutex-pre-count ,fast-read-mutex))
                   (barrier (:write))
                   (with-local-interrupts ,@body))
                 (barrier (:write))
                 (atomic-incf (frmutex-post-count ,fast-read-mutex))
                 (when ,%got-it (release-lock ,%mutex))))))
        ;; Unprotected
        `(multiple-value-prog1
             (with-unprotected-mutex ((frmutex-mutex ,fast-read-mutex)
                                      :lock ,lock :reenter ,reenter)
               (atomic-incf (frmutex-pre-count ,fast-read-mutex))
               (barrier (:write))
               (locally ,@body))
           (barrier (:write))
           (atomic-incf (frmutex-post-count ,fast-read-mutex))))))
;;;
;;; Backtraces
;;;

(defun* print-backtrace (&key (stream *debug-io*))
  "Prints the backtrace to STREAM."
  (declare (self (&key stream)))
  (ignore-errors
   #+sbcl (sb-debug:print-backtrace :stream stream :print-thread t)
   #-sbcl (trivial-backtrace:print-backtrace-to-stream stream))
  (values))

(defun backtraces (&key (wait 0.1) (interrupt-thread #'interrupt-thread))
  "Returns a list of backtraces for all threads - if possible.
 WAIT - is the amount of time to wait for the threads to return backtraces.
 INTERRUPT-THREAD - is the function used to interrupt threads."
  (let* ((threads (all-threads))
         (current-thread (current-thread))
         (streams (#+(and sbcl arena-allocator) sb-vm:without-arena
                   #-(and sbcl arena-allocator) progn
                    (loop for p in threads collect (cons (make-string-output-stream) 1)))))
    (loop for thread in threads
          for cell in streams
          do
       (let ((cell cell)) ;; limit the closure
         (flet ((store-backtrace ()
                  ;; Values for CDR cell.
                  ;; 1 = do print; 0 = too late.
                  (when (= (atomic-incf (cdr cell) 1) 1)
                    ;; (cdr cell) = 2 = printing.
                    (print-backtrace :stream (car cell))
                    (atomic-decf (cdr cell) 2))))
           (if (eq thread current-thread)
               (store-backtrace)
               (funcall interrupt-thread thread #'store-backtrace)))))

    ;; Wait for the list to fill up with traces.
    (loop repeat 10 while (some (lambda (s) (plusp (cdr s))) streams)
          do (sleep (/ wait 10)))

    (loop for stream in streams
          for thread in threads
          ;; Values for CDR cell.
          ;; 1 = not interrupted; 2 = printing; 0 = got backtrace.
          for trace = (and (zerop (atomic-decf (cdr stream)))
                           (get-output-stream-string (car stream)))
          when (plusp (length trace))
            collect trace
          else
            collect (format nil "No backtrace for: ~A~%" thread))))

(defun print-backtraces (&key (stream *debug-io*))
  "Prints the backtraces for all threads to the STREAM."
  (format stream "~{~&~80~~%~A~&~}" (ace.core.thread:backtraces)))
