;;; Copyright 2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

;;; Provides common macros and functions that do not have a dedicated package.
;;;

(defpackage #:ace.core.etc
  (:use #:common-lisp
        #:ace.core
        #:ace.core.macro
        #:ace.core.once-only)
  (:import-from #:ace.core.type #:variable-information)
  (:export
   #:one-of
   #:orf #:andf
   #:define-constant
   #:define-numerals
   #:defglobal* #:defglobal!
   #:clet
   #:reader-value-bind
   #:with-readers))

(in-package #:ace.core.etc)

(defmacro define-constant (name value &key (test '#'eql) documentation)
  "Defines a constant NAME with the VALUE, using TEST to compare
 VALUE for changes. DEFCONSTANT compares using EQL and signals an
 error when the values are different, so this macro may be useful
 for creating bindings to lists or other structures that aren't EQL.
 DOCUMENTATION sets the documentation for NAME.
 Example:
   (define-constant +fields+ '(a b c)
    :test #'equalp :documentation 'Fields used in...')
 Related:
  cl:defconstant
  alexandria:define-constant
  qpx:defconstant-*
  sb-int:defconstant-eqx"
  ;; This one does not warn twice on a changed value.
  ;; Also, it sets the constant value to the NEW value when TEST passes.
  `(defconstant ,name (%set-constant-value ',name ,value ,test) ; NOLINT
     ,@(when documentation (list documentation))))

(defvar *constant-name* nil "Used to pass constant name to functions.")

(defun %set-constant-value (name value &optional (test #'eql) (env (lexenv)))
  ;; This functions tries to circumvent some compiler checks
  ;; in order to extend the definition of a constant wrt. TEST predicate.
  ;; TODO(czak): Add support for more than SBCL, CLISP, or ECL.
  (let ((*constant-name* name))
    (cond
      ((not (boundp name)))
      ((not (constantp name env))
       ;; Maybe warn about changing a non-constant symbol
       ;; for the implementations that would not.
       )
      ((or (null test) (funcall test (symbol-value name) value))
       ;; Assure that if it passes the test, the constant has the value.
       ;; This reduces the number of complaints from the compiler.
       #-(or clisp)
       (ignore-errors
        (handler-bind ((warning #'muffle-warning)
                       (error #'continue))
          ;; The CL standard defines this as undefined behavior
          ;; if the symbol is a constant. E.g. SBCL will signal an error.
          (set name value)))
       ;; CLISP needs to call an internal function.
       #+clisp (sys::%proclaim-constant name value))
      (t
       ;; ECL would happily assign any value.
       ;; So, warn the user that the constant value changed.
       #+(or ecl)
       (warn "New value ~A for constant ~S unequal to the old ~A" ; NOLINT
             value name (symbol-value name)))))
  value)

#+sbcl
(defmacro defglobal* (name init-form &optional doc)
  "Defines a global variable that is always bound and cannot be proclaimed special.
 The INIT-FORM is evaluated at load/execute time.
 Arguments:
  NAME      - the symbol for the global variable.
  INIT-FORM - used to initialize the variable.
  DOC       - documentation.
 Related:
  sb-ext:defglobal"
  `(#+sbcl sb-ext:define-load-time-global
    #-sbcl defvar
    ,name ,init-form ,@(when doc `(,doc))))

(defmacro defglobal! (name init-form &optional doc)
  "Defines a global variable that is always bound and cannot be proclaimed special.
 The INIT-FORM is evaluated at compile and load/execute time.
 Arguments:
  NAME      - the symbol for the global variable.
  INIT-FORM - used to initialize the variable.
  DOC       - documentation.
 Related:
  sb-ext:defglobal"
  #+sbcl
  `(sb-ext:defglobal ,name ,init-form ,@(when doc `(,doc)))
  #-sbcl
  `(eval-always (defvar ,name ,init-form ,@(when doc `(,doc)))))


(defun %numeral-eq (old new)
  "Returns always true even if OLD and NEW values differ."
  (or (eql old new)
      (warn "Numeral~@[ '~S'~] redefined (~D -> ~D)." *constant-name* old new) ; NOLINT
      t))

(defmacro define-numerals (&rest names)
  "Defines constants using NAMES assigning them numbers starting at 0."
  `(progn
     ,@(loop :for (n . rest) :on names
             :for o :from 0
             :do (when (find n rest :test #'eq)
                   (warn "Numeral '~S' found twice in the list of names." n)) ; NOLINT
             :collect
             `(defconstant ,n (%set-constant-value ',n ,o #'%numeral-eq)))))

;;;
;;; one-of shortcut

(defmacro one-of (e &rest members)
  "True if element E compares EQL with at least one of the MEMBERS."
  (once-only (e)
    `(or ,@(lmap (m members) `(eql ,e ,m)))))

;;;
;;; SETF forms for OR and AND.
;;; TODO(czak): Move to an own module.
;;;

(defmacro orf (place &rest rest &environment env)
  "The ORF modifying macro has a similar short-cut semantics as OR.
It will return the first set of values that starts with non-NIL one
from the list taken from the PLACE and the REST of arguments.
E.g.

 (let ((a 2))
   (orf a 3 4))
=>
2
;; 'a' retains its value.

 (let (a)
   (orf a 3 4))
=>
3
;; 'a' is set to 3.

 (let ((a 1) (b 2) (c nil))
   (orf (values a b c) (values 4 5 6)))
=>
1
2
NIL
;; a, b, and c retain their values.

The behavior of ORF for setting a multiple-value PLACE is similar to
that of the SETF operator. It always returns the set of values for the PLACE.
E.g.:

 (let ((a nil) (b 2) (c 3))
    (orf (values a b c) (values 4 5 6 7)))
=>
4
5
6
;; a, b, and c are set to 4, 5, and 6 respectively.

 (let ((a nil) (b 2) (c 3))
    (orf (values a b c) (values 4 5)))
=>
4
5
NIL
;; a, b, and c are set to 4, 5, and NIL respectively.

This behavior is different from OR, where only the multiple-values
for the last sub-form are returned, and the first value only otherwise.

In addition if the first value of PLACE is NIL,
it will be set to the values returned by the first sub-form
for which the first value is non-NIL.
If none of the sub-forms return non-NIL as the first value
and the PLACE consists of multiple-values,
then the PLACE will be set to the values returned by the last sub-form.

This is different from a potential DEFINE-MODIFY-MACRO operator which
would always set the place even in the case where its first value is non-NIL."
  (multiple-value-bind (vars vals places setter getter)
      (get-setf-expansion place env)
    `(let* (,@(mapcar #'list vars vals)
            ,@places)
       ,(if (cdr places)
            ;; multiple value places
            (let ((store-vars `(values ,@places)))
              `(cond ((setf ,store-vars ,getter)
                      ,store-vars)
                     (t
                      (or ,@(lmap (form rest) `(setf ,store-vars ,form)))
                      ,setter)))
            ;; single value place
            `(or ,getter
                 (progn
                   (setf ,(car places) (or ,@rest))
                   ,setter))))))

(defmacro andf (place &rest rest &environment env)
  "The ANDF modifying macro has a similar short-cut semantics as AND.
It will return the first set of values that starts with NIL from the list
generated by the PLACE and the REST of arguments.
E.g.

 (let ((a 1))
   (andf a 3 4))
=>
4
;; 'a' retains its value.

 (let ((a 1))
   (andf a 3 4 nil))
=>
nil
;; 'a' is set to  NIL.

 (let ((a nil) (b 2) (c 3))
   (andf (values a b c) (values 4 5 6)))
=>
NIL
2
3
;; a, b, and c retain their values.

The behavior of ANDF for setting a multiple-value PLACE is similar to
that of the SETF operator. It always returns the set of values for the PLACE.
E.g.:

 (let ((a 1) (b 2) (c nil))
    (andf (values a b c) (values nil 5 6 7)))
=>
NIL
5
6
;; a, b, and c are set to NIL, 5, and 6 respectively.

 (let ((a 1) (b 2) (c nil))
    (andf (values a b c) (values nil 5)))
=>
NIL
5
NIL
;; a, b, and c are set to NIL, 5, and NIL respectively.

This behavior is different from AND, where only the multiple-values
for the last sub-form are returned, and the first value only otherwise.

In addition, if the first value of PLACE is non-NIL,
it will be set to the values returned by the first sub-form
for which the first value is NIL. If none of the sub-forms return NIL
as the first value, then the PLACE will be set to the values
returned by the last sub-form.

This is different from a potential DEFINE-MODIFY-MACRO operator which
would always set the place even in the case where its first value is NIL."
  (multiple-value-bind (vars vals places setter getter)
      (get-setf-expansion place env)
    (let* ((place (if (cdr places) `(values ,@places) (car places)))
           (setfs (lmap (form rest) `(setf ,place ,form))))
      `(let* (,@(mapcar #'list vars vals)
              ,@places)
         (cond ((setf ,place ,getter)
                (and ,@setfs)
                ,setter)
               (t
                ,place))))))

;;;
;;; Variation on LET.
;;;

(defmacro clet ((&rest clauses) &body body)
  "Binds variables defined in CLAUSES and when one is NIL, it stops.

CLAUSES have the form (VARS . FORMS).
The last form of the FORMS is the INIT-FORM providing values for the bindings.
Each clause creates a binding in succession similar to LET*.
The FORMS of later clauses can thus refer to previous bindings.

Example:
 (CLET ((sum (and a b (+ a b)))
        (c (format t \"SUM: ~A~%\" sum)
           (unless (zerop sum)
             (/ (- a b) sum))))
    ;; Evaluates the body only if A and B are non-NIL
    ;; and when the SUM is non-zero.
    ;; Second clause will output the SUM unless A or B is NIL.
    ...)

VARS can be a symbol. In this case the first value is assigned and checked.
If this value is NIL, the clause fails and the whole CLET form returns NIL.

VARS can be a list of symbols. In this case the values returned by
the INIT-FORM are used to bind to the VARS. If all the bound values are NIL,
the clause fails and CLET returns NIL.

Example:
 (CLET ((key (make-key x y z))
        ((value have) (gethash key table)))
   ;; Evaluates the body when KEY is non-NIL
   ;; and when either the VALUE or HAVE binding is non-NIL.
   ...)

Thus CLET has an implicit AND-OR schema,
with clauses in an AND-relation, and with the values of
multiple-value binding clauses in an OR-relation.

If a clause is just a variable name - i.e. not a CONS -
then the variable is bound to NIL and the evaluation continues.
"
  (unless clauses
    (return-from clet `(locally ,@body)))

  (let ((block (gensym* :block))
        bindings ignored)
    (dolist (clause clauses)
      (etypecase clause
        (cons
         (destructuring-bind (vars &rest init) clause
           (setf init (if (cdr init) `(progn ,@init) (car init)))
           (etypecase vars
             (atom
              (push `(,vars (or ,init (return-from ,block))) bindings))
             ((cons t null)
              (push `(,(car vars) (or ,init (return-from ,block))) bindings))
             ((cons t cons)
              (let (syms)
                (dolist (v vars)
                  (let ((g (gensym* v)))
                    (push g syms)
                    (push g bindings)))
                (setf syms (nreverse syms))
                (push `(,(car vars) (multiple-value-setq ,syms ,init))
                      bindings)
                (push `(,(car syms) (or ,@syms (return-from ,block))) bindings)
                (push (car syms) ignored)
                (loop :for s :in (rest syms)
                      :for v :in (rest vars)
                      :do (push `(,v ,s) bindings)))))))
        (atom
         (push clause bindings))))

    `(block ,block
       (let* ,(nreverse bindings)
         ,@(and ignored `((declare (ignore ,@ignored))))
         ,@body))))

;;;
;;; Accessor macros for structured objects.
;;;

(defun! parse-object-spec (object-spec &optional environment)
  "Parses the OBJECT-SPEC for the type of object given the lexical ENVIRONMENT.
 Returns TYPE and OBJECT or signals an error when the type is not found or is not a symbol."
  (cond ((and (consp object-spec)
              (eq (first object-spec) 'the)
              (second object-spec)
              (third object-spec))
         (values (second object-spec) (third object-spec)))
        ((symbolp object-spec)
         (multiple-value-bind (binding local info) (variable-information object-spec environment)
           (declare (ignore local))
           (unless (member binding '(:special :lexical :constant))
             (error "Cannot derive the type for: ~S." object-spec))
           (let ((type (cdr (assoc 'type info))))
             (when (and (typep type '(cons symbol list)) (eq (first type) 'or))
               (let ((%types (remove 'null (rest type))))
                 (when (= 1 (length %types))
                   (setf type (first %types)))))
             (unless (and type (symbolp type) (symbol-package type))
               (error "Cannot derive the type for: ~S." object-spec))
             (values type object-spec))))
        (t
         (error "Cannot derive the type for: ~S." object-spec))))

(defun! parse-slot-spec (slot-spec object-type)
  "Parses a SLOT-SPEC for a slot in OBJECT-TYPE.
 Returns a VARIABLE name and ACCESSOR symbol or signals an error if the symbol is not accessible."
  (let* ((variable (if (consp slot-spec) (first slot-spec) slot-spec))
         (slot (if (consp slot-spec) (second slot-spec) slot-spec))
         (package (symbol-package object-type))
         (accessor-name (format nil "~A-~A" object-type slot)))
    (multiple-value-bind (accessor status) (find-symbol accessor-name package)
      (ecase status
        ((nil)
         (error "Could not find symbol ~S in: ~S." accessor-name (package-name package)))
        ((:internal :inherited)
         (unless (eq slot (find-symbol (symbol-name slot) package))
           (error "The symbol ~S is not external in: ~S." accessor-name (package-name package))))
        (:external))
      (values variable accessor))))

(defmacro reader-value-bind (&environment env (&rest slots) object-spec &body body)
  "Executes the BODY in a lexical environment where the names of SLOTS have been
 bound to values read by readers from an OBJECT of a TYPE specified in OBJECT-SPEC.
 The OBJECT-SPEC can have the form (the TYPE OBJECT) to explicitly specify the type of object.
 If OBJECT-SPEC has no (the TYPE OBJECT) form, the OBJECT for equals the OBJECT-SPEC form and
 the lexical environment is consulted to derive the type of the OBJECT.

 The readers are found using the TYPE-SLOT name.
 It is an error if the reader is not an accessible symbol in the current package.
 Compared to WITH-READERS this macro will bind all the variables to the values returned
 by the readers. Those bindings can than be modified like normal lexical bindings.
 All the readers are called exactly once when using this form.
 See also: WITH-READERS, WITH-ACCESSORS."
  (if slots
      (multiple-value-bind (type object) (parse-object-spec object-spec env)
        (assert (and type (symbolp type) (symbol-package type)))
        (let (bindings)
          (once-only (object)
            (dolist (slot-spec slots)
              (multiple-value-bind (variable reader) (parse-slot-spec slot-spec type)
                (push `(,variable (,reader ,object)) bindings)))
            `(let (,@(nreverse bindings))
               ,@body))))
      body))

(defmacro with-readers (&environment env (&rest slots) object-spec &body body)
  "Executes the BODY in a lexical environment where the names of SLOTS are a symbol-macrolet
 to local functions that call the corresponding readers on the OBJECT of a TYPE that
 specified by the OBJECT-SPEC. The OBJECT-SPEC can have the form (the TYPE OBJECT)
 to explicitly specify the type of object. If OBJECT-SPEC has no (the TYPE OBJECT) form,
 the OBJECT form equals the OBJECT-SPEC form and the lexical environment is consulted
 to derive the type of the OBJECT.

 The reader names are created using <TYPE>-<SLOT> pattern.
 The SLOTS may have two patterns: SLOT and (VARIABLE SLOT).
 It is an error if the reader is not an accessible symbol in the current package.
 Compared to READER-VALUE-BIND using this macro the readers are only called at the
 place when the corresponding symbols are accessed. The readers can be called multiple times
 or not at all when using this form.
 See also: READER-VALUE-BIND, WITH-ACCESSORS"
  (if slots
      (multiple-value-bind (type object) (parse-object-spec object-spec env)
        (assert (and type (symbolp type) (symbol-package type)))
        (let* ((bindings '())
               (macros '()))
          (once-only (object)
            (dolist (slot-spec slots)
              (multiple-value-bind (variable reader) (parse-slot-spec slot-spec type)
                #-opt (push `(,reader (*) (,reader *)) bindings)
                (push `(,variable (,reader ,object)) macros)))
            (setf bindings (nreverse bindings))
            `(flet (,@bindings)
               ,@(when bindings `((declare (inline ,@(mapcar #'first bindings)))))
               (symbol-macrolet ,(nreverse macros)
                 ,@body)))))
      body))
