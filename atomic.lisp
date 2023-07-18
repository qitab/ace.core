;;; Copyright 2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

;;; Package providing atomic operations.
;;;
;;; In current implementation it just provides aliases for:
;;;  sb-ext:atomic-incf
;;;  sb-ext:atomic-decf
;;;  sb-ext:atomic-pop
;;;  sb-ext:atomic-push

;;; TODO(czak): Replace stuff in //lisp/parallel:atomic

(defpackage #:ace.core.atomic
  (:use #:ace.core
        #:ace.core.defun
        #:ace.core.macro
        #:cl)
  #+sbcl
  (:import-from #:sb-thread
                sb-thread:barrier)
  #+sbcl
  (:import-from #:sb-ext
                sb-ext:word
                sb-ext:atomic-incf
                sb-ext:atomic-decf
                sb-ext:atomic-pop
                sb-ext:atomic-push
                sb-ext:atomic-update
                sb-ext:compare-and-swap
                sb-ext:get-cas-expansion)
  #+sbcl
  (:import-from #:sb-kernel
                sb-kernel:check-bound)
  (:shadow cl:decf
           cl:incf
           cl:pop
           cl:push)
  (:export #:incf
           #:decf
           #:orf
           #:pop
           #:push
           #:word
           #:cas
           #:cas2
           #:peek
           #:poke
           #:update
           #:update*
           #:barrier
           #:array-cas/word))

(in-package #:ace.core.atomic)

(defmacro incf (place &optional (n 1) &environment env)
  "Increment PLACE atomically by N. Return old value."
  (let ((place (macroexpand* place env)))
    `(atomic-incf ,place ,n)))

(defmacro decf (place &optional (n 1) &environment env)
  "Decrement PLACE atomically by N. Return old value."
  (let ((place (macroexpand* place env)))
    `(atomic-decf ,place ,n)))

(defmacro push (object list)
  "PUSH OBJECT onto the LIST atomically. Return new list."
  `(atomic-push ,object ,list))

(defmacro pop (list)
  "POP from the LIST atomically. Return the top value or NIL."
  `(atomic-pop ,list))

(defmacro cas (place old new &environment env)
  "Compare the PLACE to the OLD value and swap with the NEW the value.
 Returns the actual value found in the PLACE.
 If the actual value found is equal to OLD, the swap succeeded."
  (let ((place (macroexpand* place env)))
    `(compare-and-swap ,place ,old ,new)))

(defmacro cas2 (place old1 old2 new1 new2 &environment env)
  "With PLACE = (SVREF ARRAY INDEX), replace OLD1 and OLD2 values
with NEW1 and NEW2 values. Returns (values old1* old2*).
ARRAY must be full word (64b) or T element-type vector.
INDEX must be multiple of 2 - i.e. aligned at 16 bytes."
  (destructuring-bind (svref array index) (macroexpand place env)
    (expect (eq 'svref svref))
    (once-only (array index old1 old2 new1 new2)
      `(locally (declare (optimize (safety 0)))
         (sb-vm::%vector-cas-pair ,array ,index
                                  ,old1 ,old2
                                  ,new1 ,new2)))))

(defmacro update (place function &rest args &environment env)
  "Update the PLACE using FUNCTION.
The FUNCTION needs to accept the ARGS and the old value of the PLACE.
It needs to return an updated value.
The UPDATE returns the NEW value after the update."
  (let ((place (macroexpand* place env)))
    `(atomic-update ,place ,function ,@args)))

(defmacro update* (place function &rest args &environment env)
  "Update the PLACE using FUNCTION.
The FUNCTION needs to accept ARGS and the old value of the PLACE
and needs to return the updated value.
The UPDATE* returns the OLD value just before the update."
  (let ((place (macroexpand* place env)))
    (multiple-value-bind (vars vals old new cas-form read-form)
        (get-cas-expansion place env)
      `(let* (,@(mapcar 'list vars vals)
              (,old ,read-form))
         (loop :until
            (let ((,new (funcall ,function ,@args ,old)))
              (eq ,old (setf ,old ,cas-form))))
         ,old))))

(defmacro peek (place)
  "Returns the value of PLACE. Includes a READ BARRIER."
  `(progn
     (barrier (:read)) ; before
     ,place))

(defmacro poke (place value)
  "Sets the PLACE to VALUE.
 This operation will synchronize the cache lines for the PLACE.
 COMPARE-AND-SWAP needs to work on that place.
 Returns the old value before setting the new."
  (with-gensyms (ignore)
    (once-only (value)
      `(update* ,place (lambda (,ignore) ,ignore ,value)))))

(defmacro orf (place &rest clauses &environment env)
  "Atomically, check PLACE for a non-nil value and if NIL
set it to an OR value made of CLAUSES."
  (let ((place (macroexpand* place env)))
    (multiple-value-bind (vars vals old new cas-form read-form)
        (get-cas-expansion place env)
      `(let* (,@(mapcar 'list vars vals)
              (,old ,read-form))
         (or ,old
             (clet ((,new (or ,@clauses)))
               (or ,cas-form ,new)))))))

;;; TODO(czak): Implement array access in SBCL.

(deftype index () '(and unsigned-byte fixnum))

#+sbcl
(sb-c:defknown %array-cas/word (t index word word) word
   (sb-c:always-translatable)
  :overwrite-fndb-silently t)

#+sbcl sb-vm::
(define-vop (ace.core.atomic::%array-cas/word)
  (:translate ace.core.atomic::%array-cas/word)
  (:policy :fast-safe)
  (:args (array :scs (descriptor-reg) :to :eval)
         (index :scs (any-reg) :to :result)
         (old :scs (unsigned-reg) :target rax)
         (new :scs (unsigned-reg)))
  (:arg-types * positive-fixnum unsigned-num unsigned-num)
  (:temporary
   (:sc descriptor-reg :offset rax-offset
    :from (:argument 2)
    :to :result :target result)
   rax)
  (:results (result :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 4
    (move rax old)
    (inst #:cmpxchg :lock
          (ea (- (* vector-data-offset n-word-bytes) other-pointer-lowtag)
              array index (ash 1 (- word-shift n-fixnum-tag-bits)))
          new)
    (move result rax)))

(defun %array-cas/word (a i o n) (%array-cas/word a i o n))

(defun* array-cas/word (array index old new)
  "Compare and swap a cell in a simple ARRAY with word (64 bit) elements.
The array entry at INDEX is compared with the OLD value and if it matches,
it is replaced with the NEW value. The operation always returns the value
found previously at the INDEX. Compare it with OLD to test for success."
  (declare (self inline ((simple-array word 1) index word word) word))
  (%array-cas/word
   array (check-bound array (length array) index) old new))
