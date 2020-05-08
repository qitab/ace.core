;;; Arithmetic and boolean operations on the vector index type.
;;;

(defpackage #:ace.core.ix
  ;; TODO(czak): Remove nicknames.
  (:nicknames #:google.core.ix)
  (:import-from #:ace.core.vector #:index)
  (:export
   #:mod #:rem #:truncate #:floor #:ceiling

   #:+ #:- #:* #:/ #:incf #:decf #:1+ #:1-
   #:max #:min #:maxf #:minf
   #:minusp #:zerop #:plusp #:oddp #:evenp
   #:< #:> #:<= #:>= #:= #:/=

   #:ash #:ashf
   #:logior #:logxor #:logand #:logandc1 #:logandc2 #:logbitp
   #:logiorf #:logxorf #:logandf #:logandc1f #:logandc2f))

(in-package #:ace.core.ix)

;;; Compile the operations without safety unless in the debug build.
#-dbg (cl:declaim (cl:optimize (cl:speed 3) (cl:safety 0) (cl:debug 0)))

(google.core.fast-ops:define-typed-integer-math-functions index "")
