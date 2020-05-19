;;; Copyright 2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

;;;; Utilities for dealing with types.
;;;;


;;;; TODO(czak): Unify the libraries across google3/lisp and travel/qpx.

(defpackage #:ace.core.type
  (:use #:cl #:ace.core)
  #+sbcl (:import-from #:sb-cltl2 #:variable-information #:function-information)
  #+ccl (:import-from #:ccl #:variable-information #:function-information)
  #+allegro (:import-from #:sys #:variable-information #:function-information)
  #+lispworks (:import-from #:hcl #:variable-information #:function-information)
  (:export
   #:add-null
   #:remove-null
   #:unknownp
   #:contains-unknown-p
   #:declaimed
   #:element-of-array
   #:expand
   ;; INFO - describes a type and the operations on it
   #:info #:make-info #:make-info-form
   #:info-name
   #:info-constructor-form
   #:info-clear
   #:info-reset
   #:info-compare
   #:info-copy
   #:info-clone
   #:info-combine
   #:info-hash
   #:info-format
   #:info-cyclicp #:cyclicp
   ;; PARAMETER - describes a function parameter
   #:parameter
   #:parameter-name
   #:parameter-key
   #:parameter-type
   #:parameter-default
   #:parameter-defaultp
   #:parameter-supplied
   #:parameter-label
   #:make-parameter
   ;; FINFO - describes a function
   ;; TODO(czak): This needs its own package.
   #:finfo
   #:finfo-name
   #:finfo-inline
   #:finfo-foldable
   #:finfo-known
   #:finfo-required
   #:finfo-optional
   #:finfo-rest
   #:finfo-keyword
   #:finfo-allow-other-keys
   #:finfo-aux
   #:finfo-values
   #:make-finfo
   #:function-form-argument-types
   #:function-lambda-list
   ;;
   #:form-typep
   #:function-information
   #:variable-information
   #:limits
   #:bit-length
   #:list-of #:deftype-list-of
   #:upgraded-type-of
   #:specialp
   #:globalp))

(in-package #:ace.core.type)

(defstruct info
  "TYPE:INFO describes attributes of a type."
  name             ; The name of the type.
  ;; Default functions dealing with the type.
  constructor-form ; A constructor form used to create instances of this type.
  clear            ; Clears the instance. Takes a :PURGE option.
  reset            ; Returns the instance to the defult state. Takes constructor arguments.
  copy             ; Copies elements from the source into another, possibly new, instance.
  clone            ; Deep copy elements from the source into another, possibly new, instance.
  combine          ; Copies and adds elements from the source into the destination merging fields.
  merge            ; Copies and adds elements from the source into the destination merging fields.
  compare          ; Default type ordering and equality function. Returns -1, 0, 1.
  hash             ; Returns a fixnum hash number that respects the compare function.
  format           ; Symbol for the format function that prints the type.
  cyclicp)         ; True if the type may be cyclic.

(defun make-info-form (info)
  "Given the type INFO, return a from that can construct the INFO."
  (with-slots (name constructor-form clear reset copy clone combine compare hash cyclicp)
      (the info info)
    `(make-info
      :name ',name
      :constructor-form ',constructor-form
      :clear ',clear
      :reset ',reset
      :copy ',copy
      :clone ',clone
      :combine ',combine
      :compare ',compare
      :hash ',hash
      :cyclicp ',cyclicp)))

;;;
;;; TODO(czak): Move to a package called ace.core.function.
;;;

(deftype parameter-label ()
  "State in which the function argument list parse is at the moment."
  '(member :required :optional :rest :key :aux))

(defstruct parameter
  "Provides information about a function parameter."
  (name nil :type symbol)
  (key nil :type keyword)
  (type nil :type (or cons symbol))
  (default nil :type t)
  (defaultp nil :type boolean)
  (supplied nil :type symbol)
  (label nil :type parameter-label))

(defmethod make-load-form ((p parameter) &optional env)
  (declare (ignore env))
  (make-load-form-saving-slots p))

(defstruct finfo
  "TYPE:FINFO stores information about a function."
  (name nil :type (or (and symbol (not null)) (cons symbol list)))
  (inline nil :type boolean) ; function is inlineable, see MACRO:INLINE-FUNCTION-P.
  (foldable nil :type boolean)
  (known nil :type boolean)
  (required nil :type list) ; required parameters
  (optional nil :type list) ; optional parameters
  (rest nil :type (or null parameter))     ; rest parameter
  (keyword nil :type list)  ; keyword parameter
  (aux nil :type list) ; auxiliary variables
  (allow-other-keys nil :type boolean)
  (values nil))

(defmethod make-load-form ((i finfo) &optional env)
  (declare (ignore env))
  (make-load-form-saving-slots i))

(defmethod print-object ((o finfo) stream)
  "Print the finfo O to STREAM."
  (with-slots (name inline foldable required optional rest keyword allow-other-keys values) o
    (print-unreadable-object (o stream)
      (format stream "FINFO ~A~:[~; inline~]~:[~; foldable~]~_ (" name inline foldable)
      (format stream "~{~A~^ ~}" (mapcar #'parameter-name required))
      (let ((sep (if required " " "")))
        (when optional
          (format stream "~A&optional ~{~A~^ ~}" sep (mapcar #'parameter-name optional))
          (setf sep " "))
        (when rest
          (format stream "~A&rest ~A" sep (parameter-name rest))
          (setf sep " "))
        (when keyword
          (format stream "~A&key ~{~A~^ ~}" sep (mapcar #'parameter-name keyword))
          (setf sep " "))
        (when allow-other-keys
          (format stream "~A&allow-other-keys" sep)))
      (format stream ")~_ ~A" values))))

(defvar *function-infos* (make-hash-table :test #'equal) "Stores function infos.")

(defun finfo (name)
  "Returns the FINFO for the function NAME."
  (gethash name *function-infos*))

(defun (setf finfo) (finfo name)
  "Sets the function information FINFO for the function with NAME."
  (setf (gethash name *function-infos*) finfo))

(defun function-lambda-list (name)
  "Return the lambda-list for the global function with NAME."
  (let ((finfo (finfo name))
        (ll))
    (unless finfo (return-from function-lambda-list (values nil nil)))

    (dolist (r (finfo-required finfo))
      (push (parameter-name r) ll))
    (when (finfo-optional finfo)
      (push '&optional ll)
      (dolist (o (finfo-optional finfo))
        (cond ((parameter-supplied o)
               (push `(,(parameter-name o)
                       ,(parameter-default o)
                       ,(parameter-supplied o))
                     ll))
              ((parameter-default o)
               (push `(,(parameter-name o) ,(parameter-default o)) ll))
              (t
               (push (parameter-name o) ll)))))
    (when (finfo-rest finfo)
      (push '&rest ll)
      (push (parameter-name (finfo-rest finfo)) ll))
    (when (finfo-keyword finfo)
      (push '&key ll)
      (dolist (k (finfo-keyword finfo))
        (cond ((parameter-supplied k)
               (push `(,(if (string= (parameter-key k) (parameter-name k))
                            (parameter-name k)
                            `(,(parameter-key k) ,(parameter-name k)))
                       ,(parameter-default k)
                       ,(parameter-supplied k))
                     ll))
              ((parameter-default k)
               (push `(,(if (string= (parameter-key k) (parameter-name k))
                            (parameter-name k)
                            `(,(parameter-key k) ,(parameter-name k)))
                       ,(parameter-default k)) ll))
              (t
               (push (if (string= (parameter-key k) (parameter-name k))
                         (parameter-name k)
                         `((,(parameter-key k) ,(parameter-name k))))
                     ll))))
      (when (finfo-allow-other-keys finfo)
        (push '&allow-other-keys ll)))

    (values (nreverse ll) t)))

(defun function-form-argument-types (function-form)
  "Returns (values types T) for the types of arguments for a functional FUNCTION-FORM.
 Returns (values nil nil) if the function information was not found."
  (unless (typep function-form '(cons symbol list))
    (error "Expected ~S to be a function form." function-form))
  (let* ((name (first function-form))
         (finfo (finfo name))
         (args (rest function-form))
         (rest-type nil)
         (types))
    (cond (finfo
           (with-accessors ((required finfo-required)
                            (optional finfo-optional)
                            (rest     finfo-rest)
                            (keyword  finfo-keyword)
                            (allow-other-keys finfo-allow-other-keys))
               (the finfo finfo)
             (dolist (spec required)
               (push (parameter-type spec) types)
               (pop args))
             (dolist (spec optional)
               (unless args (return))
               (push (parameter-type spec) types)
               (pop args))
             (when (and rest args)
               (setf rest-type (parameter-type rest))
               (unless keyword
                 (dolist (arg args)
                   (declare (ignore arg))
                   (push rest-type types))))
             (when (and keyword args)
               (loop for (key value) on args by #'cddr do
                 (unless (keywordp key)
                   (error "Expected a keyword in ~S got ~S" args key))
                 (let ((spec (find key keyword :key #'parameter-key)))
                   (unless (or allow-other-keys spec)
                     (error "Unexpected ~S keyword in ~S." key finfo))
                   (push 'keyword types)
                   (push (or (and spec (parameter-type spec)) rest-type) types)))))
           (values (nreverse types) t))
          (t
           (values nil nil)))))

(defun form-typep (form type &optional env)
  "Returns (values T T) if FORM is of TYPE in the lexical environment ENV.
 Returns (values NIL T) if the FORM is not of TYPE.
 Returns (valuse NIL NIL) if the information about the type of the form cannot be derived.
 Examples:
   (form-typep 10 'fixnum) => T T
   (form-typep '(+ 10 x) 'number) => T T
   (form-typep '(string x) 'number) => NIL T
   (form-typep '(undefined x) 'number) => NIL NIL"
  (multiple-value-bind (result resultp)
      (cond ((eq type 't)
             (values t t))
            ;; Evaluate the constants and check the type of result.
            ((constantp form)
             (values (typep (eval form) type env) t))
            ;; A symbol maybe a name of a variable.
            ((symbolp form)
             ;; The variables can be special, lexical, symbol macros, (constants,) globals, ...
             ;; The type returned by variable information maybe a super type of the declared type.
             (let ((symbol-type (cdr (assoc 'type (nth-value 2 (variable-information form env))))))
               (when symbol-type
                 (subtypep symbol-type type env))))
            ;; Use the type specified in (THE TYPE ...) from.
            ((typep form '(cons (eql the) (cons (or symbol list) list)))
             (subtypep (second form) type env))
            ;; Check if this is a function form.
            ;; Use FTYPE definition to find the function value type.
            ((typep form '(cons symbol list))
             (multiple-value-bind (dtype local declarations) (function-information (car form) env)
               (declare (ignore local))
               (when (eq dtype :function)
                 (let* ((ftype (cdr (assoc 'ftype declarations)))
                        (value-type (and (typep ftype '(cons (eql function) list)) (third ftype))))
                   (when (typep value-type '(cons (eql values) list))
                     (subtypep (second value-type) type env)))))))
    (values result resultp)))

(defun specialp (symbol &optional environment)
  "True if SYMBOL denotes a special variable in the ENVIRONMENT."
  (declare (ignorable environment))
  (when (and (symbolp symbol) (not (constantp symbol)))
    #+abcl         (ext:variable-special-p symbol)
    #+cmu          (walker:variable-globally-special-p symbol)
    #+(or ecl gcl) (si:specialp symbol)
    #+clisp        (ext:special-variable-p symbol evironment)
    #+(or sbcl ccl allegro lispworks)
    (eq (variable-information symbol environment) :special)))

(defun globalp (symbol)
  "True if SYMBOL denotes a global variable."
  #+(or sbcl ccl allegro lispworks)
  (eq (variable-information symbol) :global))

(defun declaimed (symbol &key (class :variable))
  "Return the declaimed toplevel type specifier for a given SYMBOL.
 CLASS can be :VARIABLE or :FUNCTION."
  (when (symbolp symbol)
    #-sbcl
    (assert nil nil "TYPE:DECLAIMED unimplemented.")
    #+sbcl
    (let ((type (sb-int:info class :type symbol)))
      (and type (sb-kernel:type-specifier type)))))

(defun unknownp (type-specifier)
  "True if the TYPE-SPECIFIER specifies an unknown type."
  #+sbcl
  (sb-kernel:unknown-type-p (sb-kernel:specifier-type type-specifier))
  #-sbcl
  (assert nil nil "TYPE:UNKNOWNP unimplemented."))

(defun contains-unknown-p (type-specifier)
  "True if the TYPE-SPECIFIER contains an unknown type."
  (or (unknownp type-specifier)
      (and (consp type-specifier)
           (member (car type-specifier) '(and or not) :test #'eq)
           (some #'contains-unknown-p (rest type-specifier)))))

(defun expand (type-specifier &optional env)
  "Expands the TYPE-SPECIFIER to a canonical representation in the ENV."
  ;; TODO(czak): Fix for other then SBCL to accept the environment.
  (declare (ignorable env))
  #+abcl          (system::expand-deftype type-specifier)
  #+xcl           (system::expand-deftype type-specifier)
  #+allegro       (excl:normalize-type type :default type-specifier)
  #+ccl           (ccl::type-expand type-specifier)
  #+clisp         (ext:type-expand type-specifier)
  #+cmu           (kernel:type-expand type-specifier)
  #+ecl           (si::expand-deftype type-specifier)
  #+mkcl          (si::expand-deftype type-specifier)
  #+lispworks     (type:expand-user-type type-specifier)
  #+sbcl          (sb-ext:typexpand type-specifier env)
  #-(or abcl allegro ccl clisp cmu ecl lispworks mkcl sbcl xcl)
  (assert nil nil "TYPE:EXPAND unimplemented."))

(defun %limits (type &optional env)
  "Returns (values min max) - the minimum and maximum value for an integer or float TYPE.
 ENV - the lexical environment used to expand the TYPE.
 Returns (values NIL NIL) for any of the values if it cannot be determined."
  (let ((expanded (expand type env)))
    (if (consp expanded)
        (case (first expanded)
          ((integer float long-float double-float single-float short-float)
           (let* ((%start  (second expanded))
                  (start   (if (consp %start) (1+ (first %start)) %start))
                  (%end    (third expanded))
                  (end     (if (consp %end) (1- (first %end)) %end)))
             (when (or (eq start '*) (eq end '*))
               (multiple-value-bind (min max) (%limits (first expanded) env)
                 (when (eq start '*) (setf start min))
                 (when (eq end   '*) (setf end max))))
             (values start end)))
          (or
           (loop
              with min = nil
              with max = nil
              for subtype in (rest expanded) do
                (multiple-value-bind (smin smax) (%limits subtype env)
                  (unless (and smin smax) (return (values nil nil)))
                  (when (<= smin smax)
                    (setf min (if min (min min smin) smin)
                          max (if max (max max smax) smax))))
              finally (return (values min max))))
          (and
           ;; This heuristic here is somehow relaxed.
           (let (min max)
             ;; Compute the wide inclusive range.
             (loop for subtype in (rest expanded) do
                  (multiple-value-bind (smin smax) (%limits subtype env)
                    (unless (and smin smax (> smin smax))
                      (when smin (setf min (if min (max min smin) smin)))
                      (when smax (setf max (if max (min max smax) smax))))))
             ;; Subtract the exclusive inverted ranges.
             (loop for subtype in (rest expanded) do
                  (multiple-value-bind (smin smax) (%limits subtype env)
                    (when (and smin smax (> smin smax))
                      (if (and min max (> min max))
                          (setf min (max min smin)
                                max (min max smax))
                          (progn
                            (when (or (null min) (<= smax min smin)) (setf min smin))
                            (when (or (null max) (<= smax max smin)) (setf max smax)))))))
             (values min max)))
          (mod
           (values 0 (1- (second expanded))))
          (eql
           (let ((value (second expanded)))
             (if (integerp value)
                 (values value value)
                 (values nil nil))))
          (member
           (loop
              with min = nil
              with max = nil
              for element in (rest expanded) do
                (unless (numberp element) (return (values nil nil)))
                (setf min (if min (min min element) element)
                      max (if max (max max element) element))
              finally (return (values min max))))

          (not
           (multiple-value-bind (min max) (%limits (second expanded) env)
             (values (and max (1+ max)) (and min (1- min)))))

          ((satisfies values)
           ;; type too complex.
           (values nil nil)))

        ;; else - atom.
        (case expanded
          ((float long-float)  (values most-negative-long-float most-positive-long-float))
          (double-float        (values most-negative-double-float most-positive-double-float))
          (single-float        (values most-negative-single-float most-positive-single-float))
          (short-float         (values most-negative-short-float most-positive-short-float))
          (fixnum              (values most-negative-fixnum most-positive-fixnum))
          (unsigned-byte       (values 0 nil))
          (bit                 (values 0 1))
          (t                   (values nil nil))))))

(defun limits (type &optional env)
  "Returns (values min max) - the minimum and maximum value for an integer or float type.
 ENV - the lexical environment used to expand the TYPE.
 Returns NIL for any of the values if it cannot be determined."
  (if (subtypep type '(or integer float) env)
      (%limits type env)
      (values nil nil)))

(defun members (type &optional env)
  "Returns (values members flag) for an enumeration TYPE (in environment ENV).
MEMBERS is the list of member elements of the TYPE.
FLAG is nil, when the TYPE was not recognized as a member type."
  (let ((expanded
         (if (eq type 'boolean)
             ;; Ensure that nil comes before T in the expansion.
             '(member nil t)
             (expand type env))))
    (typecase expanded
      ((cons (eql eql))
       (values (list (second expanded)) t))
      ((cons (eql member))
       (values (remove-duplicates (rest expanded) :from-end t) t))
      ((cons (eql or))
       (let (members)
         (dolist (clause (rest expanded))
           (multiple-value-bind (sub-members memberp) (members clause env)
             (unless memberp (return-from members (values nil nil)))
             (dolist (e sub-members)
               (pushnew e members))))
         (values (nreverse members) t)))
      ((cons (eql and))
       (let ((first t) members type-lists)
         (declare (list members type-lists))
         (dolist (clause (rest expanded))
           (multiple-value-bind (sub-members memberp) (members clause env)
             (declare (list sub-members))
             (cond ((not memberp)
                    (push clause type-lists))
                   (first
                    (setf members (copy-list sub-members)
                          first nil))
                   (t
                    ;; Retain the ordering of the list.
                    (setf members
                          (delete-if-not
                           (lambda (e) (member e sub-members))
                           members))))))
         (if (and type-lists first)
             (values nil nil)
             (values
              (if type-lists
                  (loop :with filter = `(and ,@(nreverse type-lists))
                        :for e :in members
                        :when (typep e filter)
                          :collect e)
                  members)
              t))))
      ((eql nil)
       (values '() t))
      ((eql bit)
       (values '(0 1) t))
      ((eql null)
       (values '(nil) t))
      (t
       (values nil nil)))))

(defun bit-length (type &key (representation :absolute) env)
"Returns (values BIT-LENGTH BYTE-TYPE MIN MAX):
  BIT-LENGTH is the number of bits inclusive the sign bit required for the type.
  BYTE-TYPE is SIGNED-BYTE or UNSIGNED-BYTE depending from the type accepting negative values.
  MIN and MAX are the minimum and maximum values accepted by the type.

Returns (values BIT-LENGTH 'MEMBER members) for MEMBER like types.

 Arguments:
  REPRESENTATION can be:
    :ABSOLUTE - an unjustified integer representation with zero represented by all bits unset.
         The bit-length is given by the necessary integer-length of the minimum and maximum values
         plus the sign bit for signed-byte integers.
    :RELATIVE - an integer representation shifted at by the minimum value.
         The integer-length of the range between minimum and maximum values.

  ENV is the environment used to resolve the TYPE.

  Returns (values NIL NIL NIL NIL) if the bit-length cannot be found."
  ;; TODO(czak): Support mixed representation (or NULL (integer 0 127)).
    (cond ((subtypep type 'integer env)
           (multiple-value-bind (min max) (limits type env)
             (if (and (integerp min) (integerp max))
                 (values
                  (ecase representation
                    (:absolute
                     (if (minusp min)
                         (1+ (max (integer-length min) (integer-length max)))
                         (integer-length max)))
                    (:relative
                     (integer-length (- max min))))
                  (if (minusp min) 'signed-byte 'unsigned-byte)
                  min max)
                 (values nil nil nil nil))))
          (t
           (multiple-value-bind (members memberp) (members type)
             (if (and memberp members)
                 (values (integer-length (1- (length members)))
                         'member members nil)
                 (values nil nil nil nil))))))

(defun element-of-array (array-type &optional env)
  "Returns the element type for an ARRAY-TYPE. Returns NIL if ARRAY-TYPE is not an array-type.
 ENV is the lexical environment used to expand the type."
  (and (subtypep array-type 'array)
       (let ((expanded (expand array-type env)))
         (if (consp expanded)
             (second expanded)
             (case expanded
               ((base-string simple-base-string)    'base-character)
               ((string      simple-string)         'character)
               ((bit-vector  simple-bit-vector)     'bit)
               (t t))))))

(defun add-null (type-spec)
  "Extends the TYPE-SPEC by adding the NULL type.
 If the type is already nullable returns the TYPE-SPEC as is."
  ;; Note: this is done only for aesthetic reasons.
  (cond ((ignore-errors (typep nil type-spec))      type-spec)
        ((contains-unknown-p type-spec)             `(or null ,type-spec))
        ((subtypep type-spec 'function)             `(or null ,type-spec))
        ((atom type-spec)                           `(or null ,type-spec))
        ((eq (first type-spec) 'or)                 `(or null . ,(cdr type-spec)))
        ((eq (first type-spec) 'member)             `(member nil . ,(cdr type-spec)))
        (t                                          `(or null ,type-spec))))

(defun remove-null (type-spec)
  "Modified the TYPE-SPEC by removing the NULL type.
 Removes NULL from OR type-spec and NIL from the MEMBER type-spec.
 Returns NIL if the type defaults to NULL."
  (cond ((eq type-spec 'null) nil)
        ((atom type-spec) type-spec)
        ((eq (first type-spec) 'or)
         (let ((types (remove 'null (rest type-spec))))
           (cond ((cdr types) (cons 'or (mapcar #'remove-null types)))
                 (types       (remove-null (first types))))))
        ((eq (first type-spec) 'member)
         `(members ,@(remove nil (rest type-spec))))
        (t
         type-spec)))

(defun upgraded-type-of (value)
  "Given a VALUE, return its type with the following upgrades:
    BOOLEAN       -> BOOLEAN
    BIT           -> INTEGER
    FIXNUM        -> INTEGER
    CONS          -> LIST
    any STRING    -> STRING
    any CHARACTER -> CHARACTER
    otherwise     -> (type-of value)."
  (typecase value
    (boolean   'boolean)
    (string    'string)
    (character 'character)
    (integer   'integer)
    (list      'list)
    (t         (type-of value))))

(deftype list-of (&optional (type '*))
  "A type for a list of TYPE elements."
  ;; Declaring a satisfies predicate on the elements is not useful.
  ;; Such a '(list-of) type would cause the type to be linear in the length of the list.
  ;; Having an N factor to algorithms dealing with the lists is not very sensible to performance.
  ;; See define-list-of macro below, for more rope.
  (if (eq type nil) 'null 'list))

(defmacro deftype-list-of (type &key (suffix type))
  "Returns a LIST-OF<-suffix> type for the element TYPE. Defines a LIST-OF<-suffix>-P predicate.
 SUFFIX is used to build the names of the predicate and the list-of type.
 WARNING: Use the list-of-* types with caution as they add linear complexity at every occurrence.
 E.g. If found in function FTYPE, local variable declaration, or the type of a struct attribute,
 every call to the function every assignment to the variable and every access to the attribute will
 be of O(n) with respect to the length of the list."
  (let ((predicate (intern (format nil "LIST-OF-~A-P" suffix)))
        (list-of-type (intern (format nil "LIST-OF-~A" suffix))))
    `(progn
       (defun ,predicate (list) ;; NOLINT
         (and (listp list) (loop :for e :in list :always (typep e ',type))))
       (deftype ,list-of-type () '(and list (satisfies ,predicate))))))
