;;; Copyright 2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

;;; Arithmetic and boolean operations on fixnums.
;;;
;;; Using the functions may help to optimize code.
;;; On the other side, it prevents the compiler to do some optimizations in case the
;;; the operation could be performed on un-boxed machine words (e.g. 64 bit words).
;;; In this case the compiler may generate boxing code.
;;; There is no assumption that the typed arithmetic will be modular on all platforms nor
;;; that it will be modular when interpreted. Please use the modular arithmetic for this.
;;; The operations are optimized unless the code is compiled with the DBG feature.
;;;

(cl:defpackage #:ace.core.fx
  (:use #:ace.core.defun
        #:ace.core.fast-ops)
  (:import-from
   :cl
   cl:fixnum cl:integer
   cl:inline
   cl:&optional cl:t cl:boolean
   cl:integer-length
   cl:defconstant
   cl:declare cl:declaim cl:optimize cl:speed cl:safety cl:debug
   cl:most-positive-fixnum
   cl:most-negative-fixnum)
  (:export
   #:bits #:positive-bits

   ;; Fixnum math (with FX prefix). Please, read notes before using this for code optimization.
   #:mod #:rem #:truncate #:floor #:ceiling

   #:+ #:- #:* #:/ #:incf #:decf #:1+ #:1-
   #:max #:min #:maxf #:minf
   #:minusp #:zerop #:plusp #:oddp #:evenp
   #:< #:> #:<= #:>= #:= #:/=

   #:logior #:logxor #:logand #:logandc1 #:logandc2 #:logbitp
   #:logiorf #:logxorf #:logandf #:logandc1f #:logandc2f

   #:ash #:ashf #:ash*))

(cl:in-package #:ace.core.fx)

;;; Compile the operations without safety unless in the debug build.
#-dbg (declaim (optimize (speed 3) (safety 0) (debug 0)))

(define-typed-integer-math-functions fixnum "")

(defconstant bits ;; NOLINT
  (integer-length (cl:- cl:most-positive-fixnum cl:most-negative-fixnum))
  "The width of FIXNUM in bits.")

(defconstant positive-bits ;; NOLINT
  (integer-length cl:most-positive-fixnum)
  "The width of FIXNUM in bits.")

(defun* ash* (integer amount)
  "Move INTERGER bits to the left by the AMOUNT of places modulo the width of a FIXNUM."
  (declare (self inline foldable (integer fixnum) fixnum))
  #+sbcl
  (sb-vm::ash-left-modfx integer amount)
  #-sbcl
  (etypecase integer
    (fixnum (mask-signed-field +fixnum-width+ (ash integer amount)))
    (integer (mask-signed-field +fixnum-width+
                                (ash (mask-signed-field +fixnum-width+ integer) amount)))))
