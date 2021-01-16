;;; Copyright 2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

;;; Utilities related to vectors.
;;; The symbols in this package are designed to be used with the package prefix.
;;; Use the ACE.CORE namespace for simple syntax.
;;;

(ace.core:defpackage* #:ace.core.vector
  (:use #:cl #:ace.core)
  #+sbcl
  (:import-from #:sb-ext sb-ext:word)
  (:export
   #:index #:size
   #:compare
   #:with-vector
   #:make-empty
   #:some #:some1 #:some2
   #:every #:every1 #:every2
   #:prefetch))

(in-package #:ace.core.vector)

(deftype index () "Vector subscript type." `(mod ,array-dimension-limit))
(deftype size () "Vector size type." `(integer 0 ,array-dimension-limit))

(defun* compare (v1 v2 &key (test #'eq))
  "Iterate over vector V1 and V2 comparing elements with TEST (EQ by default)."
  (declare (self inline (vector vector &key function) boolean))
  (let ((len1 (length v1))
        (len2 (length v2)))
    (declare (size len1 len2))
    (when (= len1 len2)
      (loop :for i :of-type index :from 0 :below len1
            :always (funcall test (aref v1 i) (aref v2 i))))))

#+(or sbcl ccl)
(defvar *empty-vectors* (apply #'make-hash-table :test #'eq
                               #+sbcl '(:synchronized t)
                               #+ccl  '(:shared :lock-free)))

(defun* make-empty (element-type)
  "Creates a shared instance of a static empty vector for the given ELEMENT-TYPE.
 The instances are shared between the upgraded-array-element-types wrt. the ELEMENT-TYPE."
  (declare (self ((or symbol cons)) vector))
  #+(or sbcl ccl)
  (let ((upgraded-type (upgraded-array-element-type element-type)))
    (or (gethash upgraded-type *empty-vectors*)
        (setf (gethash upgraded-type *empty-vectors*)
              (make-array 0 :element-type upgraded-type))))
  #-(or sbcl ccl)
  (make-array 0 :element-type element-type))

#+sbcl
(map nil (lambda (x) (make-empty (sb-vm:saetp-specifier x)))
     sb-vm:*specialized-array-element-type-properties*)

(defmacro with-vector (((vector-var vector) (start-var start) (end-var end)
                        &key (check-fill-pointer t) force-inline) &body body)
  "This de-references a VECTOR and binds VECTOR-VAR to the underlying vector representation.
START and END are adjusted to fall into the range of the simple vector. The bounds are checked.
Some implementations allow to transfrom the vector into the underlying SIMPLE-ARRAY.
If FORCE-INLINE is true, the WITH-VECTOR forms will be inlined."
  #+sbcl
  `(sb-kernel:with-array-data ((,vector-var ,vector) (,start-var ,start) (,end-var ,end)
                               :check-fill-pointer ,check-fill-pointer
                               :force-inline ,force-inline)
     ,@body)
  #+cmu
  `(lisp::with-array-data ((,vector-var ,vector) (,start-var ,start) (,end-var ,end))
     (assert (<= 0 ,start-var ,end-var
                 (if ,check-fill-pointer (length ,vector-var) (array-dimension ,vector-var 0))))
     ,@body)
  #+openmcl
  (let ((offset (gensym "OFFSET-")))
    `(multiple-value-bind (,vector-var ,offset) (ccl::array-data-and-offset ,vector)
       (let ((,start-var (+ ,start ,offset))
             (,end-var   (+ ,end ,offset)))
         (assert (<= 0 ,start-var ,end-var
                     (if ,check-fill-pointer (length ,vector-var) (array-dimension ,vector-var 0))))
         ,@body)))
  ;; This may not produce a simple vector.
  #-(or sbcl cmu openmcl)
  (let ((%vector (gensym "VECTOR-")))
    `(let ((,%vector ,vector))
       (let ((,vector-var ,%vector)
             (,start-var ,start)
             (,end-var (or ,end (length ,%array))))
         (assert (<= 0 ,start-var ,end-var
                 (if ,check-fill-pointer (length ,vector-var) (array-dimension ,vector-var 0))))
         ,@body))))

;;;
;;; Prefetch
;;;
#+sbcl
(progn
  (sb-c:defknown %array-prefetch-t0/word (t index) (values)
    (sb-c::dx-safe sb-c:always-translatable)
    :overwrite-fndb-silently t)

  sb-vm::
  (define-vop (ace.core.vector::%array-prefetch-t0/word)
      ;; VOP for PREFETCH TO on word vectors, with a tagged register index.
      (:translate ace.core.vector::%array-prefetch-t0/word)
    (:policy :fast-safe)
    (:args (array :scs (descriptor-reg) :to :eval)
           (index :scs (any-reg) :to :result))
    (:arg-types * positive-fixnum)
    (:results)
    (:generator 4
                (inst #:PREFETCH :T0
                      ;; TODO(czak): Allow immediate and signed-reg index,
                      ;;   precompute best EA based on the input node.
                      (ea (- (* sb-vm:vector-data-offset sb-vm:n-word-bytes)
                             sb-vm:other-pointer-lowtag)
                          array index (ash 1 (- word-shift n-fixnum-tag-bits))))))

  (defun %array-prefetch-t0/word (a i)
    ;; Function stub for the VOP.
    ;; Define in terms of the VOP translation.
    (%array-prefetch-t0/word a i))

  (defun* prefetch (array index)
    "Prefetch memory for a word ARRAY at INDEX to all caches.

Prefetch memory into all caches. This kind of software prefetch is useful in
case of a non-liner or unpredictable memory access where the memory address is
known several instructions in advance. The hardware prefetchers are usually very
good at optimizing any form of linear memory access.

An example where prefetch may help is binary search.

Prefetch can help if profiling has determined that the memory throughput
has not been maxed out. It may cause a slowdown otherwise.

Note that any prefetch offsets are usually CPU and memory hardware dependent."
    (declare (self inline (array index)))
    (etypecase array
      ((or simple-vector (simple-array word 1))
       (%array-prefetch-t0/word array index)))))
