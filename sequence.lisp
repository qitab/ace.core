;;; Utilities related to general sequences.
;;;

(ace.core:defpackage* #:ace.core.sequence
  (:use #:common-lisp)
  (:export
   ;; TODO(czak): Those may be interesting when defined for generic sequences.
   ;; #:length=
   ;; #:partition
   ;; #:npartition
   ;; #:appendf
   ;; #:nconcf
   ;; #:nreversef
   ;; #:unionf
   ;; #:intersectionf
   #:removef
   #:removef-if
   #:removef-if-not
   #:deletef
   #:deletef-if
   #:deletef-if-not))

(in-package #:ace.core.sequence)

(defmacro removef (item sequence &rest rest
                   &key from-end test test-not start end count key
                   &environment env)
  "Remove all elements from the SEQUENCE that match the ITEM.
SEQUENCE needs to be a place and is assigned the new returned SEQUENCE.

The REST parameters are:
 FROM-END - if true, will start deleting from the end,
 TEST - the equality test used to compare ITEM with the elements in the sequence,
 TEST-NOT - the complement of the TEST,
 START - the START index (default 0),
 END - the END index (default is NIL, the end of sequence),
 COUNT - the maximum count of elements to be deleted,
 KEY - a function that derives values to be compared with the ITEM."
  (declare (ignore from-end test test-not start end count key))
  (multiple-value-bind (vars vals places setter getter) (get-setf-expansion sequence env)
    `(let* (,@(mapcar #'list vars vals)
            (,(first places) (remove ,item ,getter ,@rest))
            ,@(rest places))
       ,setter)))

(defmacro removef-if (predicate sequence &rest rest
                      &key from-end start end count key
                      &environment env)
  "Remove all elements from the SEQUENCE that match the PREDICATE.
SEQUENCE needs to be a place and is assigned the new returned SEQUENCE.

The REST parameters are:
 FROM-END - if true, will start deleting from the end,
 START - the START index (default 0),
 END - the END index (default is NIL, the end of sequence),
 COUNT - the maximum count of elements to be deleted,
 KEY - a function that derives values to be tested by the predicate."
  (declare (ignore from-end start end count key))
  (multiple-value-bind (vars vals places setter getter) (get-setf-expansion sequence env)
    `(let* (,@(mapcar #'list vars vals)
            (,(first places) (remove-if ,predicate ,getter ,@rest))
            ,@(rest places))
       ,setter)))

(defmacro removef-if-not (predicate sequence &rest rest
                          &key from-end start end count key
                          &environment env)
  "Remove all elements from the SEQUENCE that does NOT match the PREDICATE.
SEQUENCE needs to be a place and is assigned the new returned SEQUENCE.

The REST parameters are:
 FROM-END - if true, will start deleting from the end,
 START - the START index (default 0),
 END - the END index (default is NIL, the end of sequence),
 COUNT - the maximum count of elements to be deleted,
 KEY - a function that derives values to be tested by the predicate."
  (declare (ignore from-end start end count key))
  (multiple-value-bind (vars vals places setter getter) (get-setf-expansion sequence env)
    `(let* (,@(mapcar #'list vars vals)
            (,(first places) (remove-if-not ,predicate ,getter ,@rest))
            ,@(rest places))
       ,setter)))

(defmacro deletef (item sequence &rest rest
                   &key from-end test test-not start end count key
                   &environment env)
  "Destructively delete all elements from the SEQUENCE that match the ITEM.
SEQUENCE needs to be a place and is assigned the returned modified SEQUENCE.

The REST parameters are:
 FROM-END - if true, will start deleting from the end,
 TEST - the equality test used to compare ITEM with the elements in the sequence,
 TEST-NOT - the complement of the TEST,
 START - the START index (default 0),
 END - the END index (default is NIL, the end of sequence),
 COUNT - the maximum count of elements to be deleted,
 KEY - a function that derives values to be compared with the ITEM."
  (declare (ignore from-end test test-not start end count key))
  (multiple-value-bind (vars vals places setter getter) (get-setf-expansion sequence env)
    `(let* (,@(mapcar #'list vars vals)
            (,(first places) (delete ,item ,getter ,@rest))
            ,@(rest places))
       ,setter)))

(defmacro deletef-if (predicate sequence &rest rest
                      &key from-end start end count key
                      &environment env)
  "Destructively delete all elements from the SEQUENCE that match the PREDICATE.
SEQUENCE needs to be a place and is assigned the returned modified SEQUENCE.

The REST parameters are:
 FROM-END - if true, will start deleting from the end,
 START - the START index (default 0),
 END - the END index (default is NIL, the end of sequence),
 COUNT - the maximum count of elements to be deleted,
 KEY - a function that derives values to be tested by the predicate."
  (declare (ignore from-end start end count key))
  (multiple-value-bind (vars vals places setter getter) (get-setf-expansion sequence env)
    `(let* (,@(mapcar #'list vars vals)
            (,(first places) (delete-if ,predicate ,getter ,@rest))
            ,@(rest places))
       ,setter)))

(defmacro deletef-if-not (predicate sequence &rest rest
                          &key from-end start end count key
                          &environment env)
  "Destructively delete all elements from the SEQUENCE that does NOT match the PREDICATE.
SEQUENCE needs to be a place and is assigned the returned modified SEQUENCE.

The REST parameters are:
 FROM-END - if true, will start deleting from the end,
 START - the START index (default 0),
 END - the END index (default is NIL, the end of sequence),
 COUNT - the maximum count of elements to be deleted,
 KEY - a function that derives values to be tested by the predicate."
  (declare (ignore from-end start end count key))
  (multiple-value-bind (vars vals places setter getter) (get-setf-expansion sequence env)
    `(let* (,@(mapcar #'list vars vals)
            (,(first places) (delete-if-not ,predicate ,getter ,@rest))
            ,@(rest places))
       ,setter)))
