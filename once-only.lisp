;;; Copyright 2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

;;; Defines the ONCE-ONLY, and DEFINE-COMPILER-MACRO* helpers.
;;;

(defpackage #:ace.core.once-only
  (:use #:cl #:ace.core.macro)
  (:export
   #:define-compiler-macro*
   #:once-only))

(in-package #:ace.core.once-only)

;;;
;;; Crutches.
;;;

(defun llkp (sym)
  "True if SYM is a LAMBDA-LIST-KEYWORD."
  (and (symbolp sym) (find sym lambda-list-keywords :test #'eq)))

;;;
;;; ONCE-ONLY
;;;

(defun parse-once-only-lambda-list (lambda-list)
  "Parse the LAMBDA-LIST for a once-only form and return a set of values.

Values:
  VARS   - the variables that need to be bound for the body of the macro.
  PARAMS - parameter list for the macro LAMBDA.
  PVARS  - the positional variables.
  PINIT  - positional argument's init-forms.
  KSYMS  - the keyword variables with keywords.
  KINIT  - keyword arguments keywords and init-forms.
  REST-VAR - the rest variable for the &REST parameter from the LAMBDA-LIST.
  REST-INIT - the init form for the &REST parameter from the LAMBDA-LIST.
  BODY-VAR - the body variable for the &BODY parameter from the LAMBDA-LIST.
"
  (let (vars params pvars pinit ksyms kinit key rest-var rest-init body-var)
    (do ((ll lambda-list (cdr ll)))
        ((not ll))
      (destructuring-bind (var &optional (init nil initp) varp)
          (if (atom (car ll))
              (list (car ll))
              (car ll))
        (when varp
          (warn ; NOLINT
           "ONCE-ONLY does not accept the ~S indicator." varp))
        (case var
          ((&optional &allow-other-keys)
           (push var params))
          (&rest
           (when rest-var
             (warn ; NOLINT
              "Multiple &REST arguments in the lambda-list for ONCE-ONLY."))
           (pop ll)
           (setf rest-var (1st-or-atom (car ll))
                 rest-init (2nd-or-atom (car ll)))
           (check-type rest-var symbol)
           (push '&rest params)
           (push rest-var params))
          (&body
           (pop ll)
           (destructuring-bind (var &optional (init nil initp))
               (if (atom (car ll)) (list (car ll)) (car ll))
             (when body-var
               (warn ; NOLINT
                "Multiple &BODY arguments in the lambda-list for ONCE-ONLY."))
             (setf body-var var)
             (push var vars)
             (push var params)
             (push var pvars)
             (push
              (cond
                ((and initp (typep init '(cons t (cons (cons (eql lambda))))))
                 ;; Matches `(lambda args ...)
                 init)
                ((and initp (listp init))
                 ``(lambda (,,@init) ,@,var))
                (t ``(lambda () ,@,var)))
              pinit)))
          (&key
           (unless rest-var
             (warn ; NOLINT
              "ONCE-ONLY needs a &REST argument for keywords."))
           (setf key t)
           (push '&key params))
          (t
           (when (llkp var)
             (warn ; NOLINT
              "ONCE-ONLY does not accept ~S in the lambda-list." var))
           (let ((sym (gensym* (2nd-or-atom var))))
             (push var vars)
             (cond
               (key
                (let ((key (if (consp var)
                               (first var)
                               (make-keyword (the symbol var))))
                      (var (2nd-or-atom var)))
                  (push `((,key ,var) ,sym) params)
                  (push key ksyms)
                  (push sym ksyms)
                  (push `(quote ,key) kinit)
                  (push (if initp init var) kinit)))
               (t
                (push var params)
                (push var pvars)
                (push (if initp init var) pinit))))))))
    (when rest-var
      (push rest-var vars))
    (values
     (nreverse vars)
     (nreverse params)
     (nreverse pvars)
     (nreverse pinit)
     (nreverse ksyms)
     (nreverse kinit)
     rest-var rest-init
     body-var)))

(defun parse-once-only-declarations (vars declarations env)
  "Parses DECLARATIONS for VARS from the ONCE-ONLY body.

Parameters:
 VARS - the names of the variables.
 DECLARATIONS - the declarations as passed to the macro in the &BODY.
 ENV - the lexical environment of the macro.

Values:
 TYPES - the declared type of each variable or NIL.
 INLINE - a list of 0, 1, or NIL indicating a notinline or inline variable.
 DXS - the dynamic-extent status of each variable.
 IGNORE - variables ignored in the body of the macro.
 IGNORABLE - variables declared as ignorable in the body of the macro.
"
  (let ((dx-vars        (find-declared 'dynamic-extent declarations))
        (inline-vars    (find-declared 'inline declarations))
        (notinline-vars (find-declared 'notinline declarations))
        (ignored-vars   (find-declared 'ignore declarations))
        (ignorable-vars (find-declared 'ignorable declarations)))
    (values
     (lmap (v vars)
       (find-type-declaration v declarations nil env))
     (lmap (v vars)
       (cond ((find v notinline-vars :test 'eq) '0)
             ((find v inline-vars :test 'eq) '1)))
     (lmap (v vars) (and (find v dx-vars :test 'eq) t))
     (lconc (v vars) (and (find v ignored-vars :test 'eq) `(,v)))
     (lconc (v vars) (and (find v ignorable-vars :test 'eq) `(,v))))))

#+sbcl
(defun form-typep (symbol type &optional lexenv)
  "True if the SYMBOL is declared of TYPE in LEXENV."
  (multiple-value-bind (declared known) (variable-type symbol lexenv)
    (and known declared
         (subtypep declared type lexenv))))

(defun verbatimp (form lexenv &optional type)
  "True if the FORM is passed verbatim.

Parameters:
 FORM - the form in question.
 LEXENV - lexical environment.
 TYPE - requested type for the form value.
"
  (or (constantp form lexenv)
      ;; Variables for which there is no type specified.
      #+sbcl
      (and (symbolp form)
           (or (not type) (form-typep form type lexenv))
           (member (sb-cltl2:variable-information form lexenv)
                   '(nil :lexical :constant :global)))
      ;; Function names.
      (typep form '(cons (eql function)))))

(defun generate-once-only-content (generator
                                   &key types dynamic-extent inline
                                   pvars pinit ksyms kinit
                                   (rest-init nil rest-init-p)
                                   (lexenv (lexenv))
                                   lexvar)
  "Using the GENERATOR return the content for a ONCE-ONLY or compiler macro*.

Parameters:
 GENERATOR - the body function from the macro that called this function.
 TYPES     - the declared types for the PVARS and KSYMS.
 DYNAMIC-EXTENT - flags indicating which syms can be declared dynamic-extent.
 INLINE    - flags indicating which syms should be declared inline or not.
 PVARS     - the positional argument variables names.
 PINIT     - the init-forms for the positional arguments.
 KSYMS     - the keyword argument variables (with the keyword)
 KINIT     - the init-forms for the keyword arguments (with the keywords).
 REST-INIT - the rest arguments to the lambda - usually the rest keyword list.
 LEXENV    - the lexical environment at the macro expansion.
 LEXVAR    - a variable to bind an augmented lexical environment to.
"
  (declare (function generator)
           (list types dynamic-extent inline pvars pinit ksyms kinit rest-init))
  (let (flets ftypes lets let-types dxs inlines (newenv lexenv))
    (labels ((simplify (form)
               (values (ace.core.simplify:simplify form lexenv)))
             (add-ftype (fn args body)
               (flet ((argtype (arg)
                        (or (car (member arg lambda-list-keywords :test #'eq))
                            (find-type-declaration arg body t lexenv))))
                 (let ((ftype `(ftype (function ,(mapcar #'argtype args)) ,fn)))
                   (push ftype ftypes)
                   (when lexvar
                     (setf
                      newenv
                      (handler-bind ((warning #'muffle-warning))
                        #+sbcl
                        (sb-cltl2:augment-environment
                         newenv :function `(,fn) :declare `(,ftype))
                        #+ccl
                        (ccl:augment-environment
                         newenv :function `(,fn) :declare `(,ftype))))))))
             (maybe-bind (form sym type dx inline)
               ;; Generates (optionally) a binding for FORM and return the
               ;; gensym variable or function name.
               (let ((f (simplify form)))
                 (cond ((verbatimp f lexenv type) f)
                       ((typep f '(cons (eql lambda)))
                        (let* ((fn (gensym* sym :-fn-))
                               (fun `(function ,fn)))
                          (push `(,fn ,@(rest f)) flets)
                          (add-ftype fn (second f) (cddr f))
                          (when (eql inline 1) (push fn inlines))
                          (when dx (push fun dxs))
                          fun))
                       (t
                        (let ((var (gensym* sym :-var-)))
                          (push `(,var ,f) lets)
                          (when type (push `(type ,type ,var) let-types))
                          var))))))
      ;; The lambda list can have 3 forms here:
      ;;  - only positional arguments that are satisfied using PINIT
      ;;  - positional plus &REST
      ;;  - positional plus &REST and a keyword section.
      ;;
      ;; PINIT + REST constitute the form arguments passed to the macro.
      ;; They both need to be simplified.
      ;; When there are keywords, we need to replace the first occurrence
      ;; of the keyword in the REST list by the corresponding init forms
      ;; to the keyword.
      (let* ((pcount (length pinit))
             (empty (make-list pcount))
             (types  (or types empty))
             (dx     (or dynamic-extent empty))
             (inline (or inline empty))
             ;; positional parameters.
             (pinit
              (lmap (i pinit) (s pvars) (type types) (dx dx) (inline inline)
                (maybe-bind i s type dx inline)))
             ;; keyword parameters.
             (kinit (mapcar #'simplify kinit))
             (keywords (loop :for (k v) :on kinit :by #'cddr :collect k))
             (ktypes*  (nthcdr pcount types))
             (kdx*     (nthcdr pcount dx))
             (kinline* (nthcdr pcount inline))
             (rest-type   (and rest-init-p (car (last types))))
             (rest-dx     (and rest-init-p (car (last dynamic-extent))))
             (rest-inline (and rest-init-p (car (last inline))))
             (rest-init
              (cond (keywords ; keyword version
                     (loop :for (k v) :on rest-init :by #'cddr
                           :for init = (getf kinit k v)
                           :for value
                             = (if (remf kinit k)
                                   (let ((ki (position k keywords :test 'eq)))
                                     (maybe-bind
                                      init k
                                      (or (nth ki ktypes*) rest-type)
                                      (or (nth ki kdx*) rest-dx)
                                      (or (nth ki kinline*) rest-inline)))
                                   (maybe-bind
                                    init k rest-type rest-dx rest-inline))
                           :nconc `(,k ,value)))
                    ;; Single rest parameter.
                    (rest-init-p
                     (lmap (r rest-init)
                       (maybe-bind r :r rest-type rest-dx rest-inline)))))
             (vars (and lexvar `(,lexvar)))
             (vals
              ;; Remaining keyword parameters.
              (loop :for (k v) :on kinit :by #'cddr
                    :for ki = (position k keywords :test #'eq)
                    :for type = (nth ki ktypes*)
                    :for dx = (nth ki kdx*)
                    :for inline = (nth ki kinline*)
                    :do (push (getf ksyms k) vars)
                    :collect (maybe-bind v k type dx inline)))
             (content
              (let ((vals (if lexvar (list* newenv vals) vals)))
                (progv (nreverse vars) vals
                  (apply generator (append pinit rest-init)))))
             ;; TODO(czak): Add the notinline declarations.
             (notinlines))
        (declare (list keywords flets lets let-types dxs inlines))

        (when lets
          ;; Wrap in LET.
          (setf lets (nreverse lets)
                content
                `(let ,lets
                   (declare (ignorable ,@(mapcar #'car lets))
                            ,@(nreverse let-types)
                            ,@(and notinlines `((notinline ,@notinlines))))
                   ,content)
                notinlines nil))

        (when flets
          ;; WRAP in FLET.
          (setf content
                `(flet ,(nreverse flets)
                   (declare
                    (ignorable ,@(lmap (f flets) `(function ,(car f))))
                    ,@(nreverse ftypes)
                    ,@(and dxs `((dynamic-extent ,@(nreverse dxs))))
                    ,@(and inlines `((inline ,@(nreverse inlines))))
                    ,@(and notinlines `((notinline ,@notinlines))))
                   ,content)
                notinlines nil))

        (when notinlines
          ;; Wrap in locally if there are notinline declarations.
          (setf content
                `(locally (declare (notinline ,@notinlines))
                   ,content)))

        ;; Return the (possibly wrapped) content.
        content))))

(defmacro once-only ((&rest lambda-list) &body body &environment env)
    "Assures that the arguments as specified in the LAMBDA-LIST are evaluated
only once and in the right order in the BODY of the macro.

ONCE-ONLY is useful when writing compiler macros for functions
that have keyword arguments in their lambda-list.

Parameters:
 LAMBDA-LIST - an macro lambda-list with the macro/function parameters.

The LAMBDA-LIST is very similar to a default lambda-list.
The difference is in the interpretation of the 'default' value for all
of the parameters in the LAMBDA-LIST. For ONCE-ONLY the
place of the default value is taken by the INIT-FORM which should
reference the corresponding variable in the LAMBDA-LIST.
In ONCE-ONLY all the parameters in the LAMBDA-LIST can have an
INIT-FORM to initialize them, even the required positional parameters.
This allows to perform transformations on the arguments.
E.g. one can write:

  (once-only ((p `(coerce ,p 'function)) list
               &rest args
               &key (key `(coerce (or ,key #'identity) 'function)))
   ...)

to convert the P and KEY arguments to functions.

Avoid capturing variables or function names in the INIT-FORM as it is
expanded in the lexical environment of the final macro expansion.

Also, the &OPTIONAL keyword is ignored in the lambda list and supplied
variable indicators are not accepted in ONCE-ONLY. Thus the syntax of
the LAMBDA-LIST has the following form.

 ([var | (var init-form) | &OPTIONAL]*
  [&REST rest-from
   &KEY [var | (var [init-from]) | ((:keyword var) [init-form])]*])

The BODY of the macro is wrapped into a LET form in the final expansion
which assures that the parameters are evaluated in
the right order inclusive out-of order keyword arguments.
It also handles properly multiple arguments with the same keyword.
When &KEY is specified in the ONCE-ONLY lambda-list it must be preceded
with a &REST input argument to properly derive the order of arguments passed.

To return a different non-wrapped form like a &WHOLE form
the user needs to call RETURN or a RETURN-FROM to an equivalent target.

ONCE-ONLY will pass constant values and function names (with the #' prefix)
verbatim to the BODY of the macro as those expressions
do not need to evaluated only once as they have no side-effects.
This allows the user to test for a non-constant expression using GENSYMP
and use this information to optimize the generated code or remove
unreachable parts of the generated expressions.

LAMBDA expressions passed to ONCE-ONLY will be used to generate
an FLET wrapper expression, where each LAMBDA becomes an FLET function.

Users of ONCE-ONLY can specify various declarations in the BODY
for the variables in the LAMBDA-LIST. Those expressions include type
declarations as well as DYNAMIC-EXTENT, INLINE or NOTINLINE declarations.
The generated compiler-macro will then use those declarations in the
final expansion to describe the type of the resulting values or to declare
properties of the passed in functions and lambdas such as the
INLINE, NOTINLINE or DYNAMIC-EXTENT properties of the generated FLET functions.
Those declarations will be inserted into the final compiler-macro expansions.
They are not used to describe the parameters to the compiler macro itself.

Declarations:
 types  - forms that are not constant will be assigned to temporary variables
          so they are not evaluated more than once. Those variables can have
          a type declared.
 IGNORE, IGNORABLE - declarations are used for the macro variables
          within the macro and are not used in the final expansion code.
 INLINE - for any FLET function created from a passed in LAMBDA form.
          This should be declared only for functions that will be used only
          once in the code generated by the macro.
 NOTINLINE - Declare a function passed with the variable to be notinline.
             This is useful if the expansion of the macro uses the function
             in many places and it would result in large compiled code size.
 DYNAMIC-EXTENT - for any FLET function created from a passed in LAMBDA form.
                  This is a non-safe declarations and can only be used if
                  it is absolutely sure that the lambda closure will not
                  escape from the dynamic scope of generated final code.

Example 1:

 (defun r (x y &optional (z 0))
   (sqrt (+ (* x x) (* y y) (* z z))))

 (define-compiler-macro r (x y &optional zp)
   (once-only (x y (z `(or ,z 0)))
     (declare (fixnum x y z))
     `(sqrt (+ (* ,x ,x) (* ,y ,y) ,@(when zp `((* ,z ,z)))))))

Example 2:

 (defun q (&key a b c) (list a b c))

 (define-compiler-macro q (&whole whole &rest rest &key a b c)
   (once-only (&rest rest &key a b c)
     (if (or a b c)
         `(list ,a ,b ,c)
         (return whole))))

 (let ((x 0))
   (q :b (incf x) :a (incf x) :c (incf x)))
  => (2 1 3)

See also:
 DEFINE-COMPILER-MACRO* - which can abbreviate the usage in compiler macros."
  (%once-only 'once-only lambda-list body env))

(defun %once-only (macro lambda-list body env &optional env-var)
  "Implementation for ONCE-ONLY macro.

Parameters:
 MACRO - the name of the macro calling this function.
 LAMBDA-LIST - the arguments passed to the macro.
 BODY - the body forms withing the macro invocation.
 ENV - the lexical environment at macro expansion time.
 ENV-VAR - the environment variable for the environment.
"
  (multiple-value-bind (vars params pvars pinit ksyms kinit
                        rest-var rest-init body-var)
      (parse-once-only-lambda-list lambda-list)
    (with-split-body (body declarations nil)
      (when body-var
        (push `(declare (dynamic-extent ,body-var)) declarations))
      (multiple-value-bind (types inline dxs ignore ignorable)
          (parse-once-only-declarations vars declarations env)
        (declare (list types inline dxs ignore ignorable))
        (cond ((not rest-var))
              ((find rest-var ignore :test #'eq)
               ;; Remove the &rest parameter.
               (let ((&rest. (member '&rest params)))
                 (assert &rest.) ; NOLINT
                 (setf ignore (delete rest-var ignore :test #'eq)
                       (car &rest.) (caddr &rest.)
                       (cdr &rest.) (cdddr &rest.))))
              ((and (eq macro 'once-only) kinit)
               ;; In ONCE-ONLY the &rest parameter is mostly input.
               (pushnew rest-var ignorable :test #'eq)))
        (let* ((syms (loop :for (k s) :on ksyms :by #'cddr :collect s))
               (specials (and syms `((special ,@syms))))
               (&body (gensym* :body))
               (declarations
                `(,@specials
                  ,@(and ignore `((ignore ,@ignore)))
                  ,@(and ignorable `((ignorable ,@ignorable)))))
               (result
                `(block NIL
                   (flet ((,&body ,params
                            ,@(when declarations
                                `((declare ,@declarations)))
                            ,@body))
                     (declare (dynamic-extent #',&body))
                     (generate-once-only-content
                      #',&body
                      ,@(unless (every #'null types)  `(:types ',types))
                      ,@(unless (every #'null dxs)    `(:dynamic-extent ',dxs))
                      ,@(unless (every #'null inline) `(:inline ',inline))
                      ,@(when pvars     `(:pvars ',pvars))
                      ,@(when pinit     `(:pinit (list ,@pinit)))
                      ,@(when ksyms     `(:ksyms ',ksyms))
                      ,@(when kinit     `(:kinit (list ,@kinit)))
                      ,@(when rest-init `(:rest-init ,rest-init))
                      ,@(when env-var   `(:lexvar ',env-var)))))))
          (if syms
              `(with-gensyms ,syms
                 (declare ,@specials)
                 ,result)
              result))))))
;;;
;;; DEFINE-COMPILER-MACRO*
;;;

(defun parse-macro*-lambda-list (macro lambda-list)
  "Parse the LAMBDA-LIST for the MACRO form and return a set of values.

Values:
  NEW-LAMBDA-LIST - a replacement lambda-list with additional info.
  ONCE-ONLY-LAMBDA-LIST - the arguments for ONCE-ONLY
  REST-VAR, RESTP - the rest variable and indicator.
"
  (let (new-lambda-list once-only-args rest-var restp env-var)
    (do ((ll lambda-list (cdr ll)))
        ((not ll))
      (destructuring-bind (var &optional (init nil initp) varp)
          (if (atom (car ll))
              (list (car ll))
              (car ll))
        (case var
          ((&whole &environment)
           (pop ll)
           (let ((arg (first ll)))
             (check-type arg symbol)
             (push var new-lambda-list)
             (when (eq var '&environment)
               (setf env-var arg))
             (push arg new-lambda-list)))
          ((&optional &allow-other-keys)
           (push var new-lambda-list)
           (push var once-only-args))
          (&rest
           (when rest-var
             (warn ; NOLINT
              "Multiple &REST arguments in the lambda-list for ~S." macro))
           (pop ll)
           (setf rest-var (1st-or-atom (car ll))
                 restp t)
           (check-type rest-var symbol)
           (push '&rest new-lambda-list)
           (push rest-var new-lambda-list)
           (push '&rest once-only-args)
           (push (car ll) once-only-args))
          (&key
           (unless rest-var
             (setf rest-var (gensym* :rest))
             (push '&rest new-lambda-list)
             (push rest-var new-lambda-list)
             (push '&rest once-only-args)
             (push rest-var once-only-args))
           (push '&key new-lambda-list)
           (push '&key once-only-args))
          (t
           (when (llkp var)
             (warn ; NOLINT
              "~S does not accept ~S in the lambda-list." macro var))
           (push (if varp `(,var nil ,varp) var) new-lambda-list)
           (push (if initp `(,var ,init) var) once-only-args)))))
    (values
     (nreverse new-lambda-list)
     (nreverse once-only-args)
     rest-var restp
     env-var)))

(defmacro define-compiler-macro* (name (&rest lambda-list)
                                  &body body &environment env)
  "Defines a compiler-macro with the NAME and assures that the arguments
as specified in the LAMBDA-LIST are evaluated only once and in the right order.

DEFINE-COMPILER-MACRO* is useful when writing compiler macros for functions
that have keyword arguments in their lambda-list. It also replaces the use
of ONCE-ONLY in such compiler-macros as it generates the necessary wrappers.

Parameters:
 NAME - the NAME of the compiler-macro and the corresponding function.
 LAMBDA-LIST - an macro lambda-list with the macro/function parameters.

The LAMBDA-LIST is very similar to a default macro lambda-list.
The difference is in the interpretation of the 'default' value for all
of the parameters in the LAMBDA-LIST. For DEFINE-COMPILER-MACRO* the
place of the default value is taken by the INIT-FORM which should
reference the corresponding variable in the LAMBDA-LIST.
In DEFINE-COMPILER-MACRO* all the parameters in the LAMBDA-LIST can have an
INIT-FORM to initialize them, even the required positional parameters.
This allows to perform transformations on the arguments.
E.g. one can write:

  (define-compiler-macro* fynd
     ((p `(coerce ,p 'function)) list
      &key (key `(coerce (or ,key #'identity) 'function)))
   ...)

to convert the P and KEY arguments to functions.

Avoid capturing variables or function names in the INIT-FORM as it is
expanded in the lexical environment of the final macro expansion.

The BODY of the macro is wrapped into a LET form in the final expansion
which assures that the parameters are evaluated in
the right order inclusive out-of order keyword arguments as well as
properly evaluating multiple arguments with the same keyword.

In order to return a different non-wrapped form like the &WHOLE form
the user needs to use the RETURN statement or an equivalent RETURN-FROM.

DEFINE-COMPILER-MACRO* will pass constant values and function names (with
#' prefix) verbatim to the BODY of the macro as those expressions
do not need to evaluated only once as they have no side-effects.
This allows the user to test for a non-constant expression using GENSYMP
and use this information to optimize the generated code or remove
unreachable parts of the generated expressions.

LAMBDA expressions passed to the compiler-macro will be used to generate
an FLET wrapper expression, where each LAMBDA becomes an FLET function.

Users of DEFINE-COMPILER-MACRO* can specify various declarations in the BODY
for the variables in the LAMBDA-LIST. Those expressions include type
declarations as well as DYNAMIC-EXTENT, INLINE or NOTINLINE declarations.
The generated compiler-macro will then use those declarations in the
final expansion to describe the type of the resulting values or to declare
properties of the passed in functions and lambdas such as the
INLINE, NOTINLINE or DYNAMIC-EXTENT properties of the generated FLET functions.
Those declarations will be inserted into the final compiler-macro expansions.
They are not used to describe the parameters to the compiler macro itself.

Declarations:
 types  - forms that are not constant will be assigned to temporary variables
          so they are not evaluated more than once. Those variables can have
          a type declared.
 IGNORE, IGNORABLE - declarations are used for the macro variables
          within the macro and are not used in the final expansion code.
 INLINE - for any FLET function created from a passed in LAMBDA form.
          This should be declared only for functions that will be used only
          once in the code generated by the macro.
 NOTINLINE - Declare a function passed with the variable to be notinline.
             This is useful if the expansion of the macro uses the function
             in many places and it would result in large compiled code size.
 DYNAMIC-EXTENT - for any FLET function created from a passed in LAMBDA form.
                  This is a non-safe declarations and can only be used if
                  it is absolutely sure that the lambda closure will not
                  escape from the dynamic scope of generated final code.

Example 1:

 (defun r (x y &optional (z 0))
   (sqrt (+ (* x x) (* y y) (* z z))))

 (define-compiler-macro* r (x y &optional (z `(or ,z 0) zp))
   (declare (fixnum x y z))
   `(sqrt (+ (* ,x ,x) (* ,y ,y) ,@(when zp `((* ,z ,z))))))

Example 2:

 (defun q (&key a b c) (list a b c))

 (define-compiler-macro* q (&whole whole &key a b c)
   (if (or a b c)
       `(list ,a ,b ,c)
       (return whole)))

 (let ((x 0))
   (q :b (incf x) :a (incf x) :c (incf x)))
  => (2 1 3)

See also:
 ONCE-ONLY - a similar macro that can be used inside other macros.
"
  (multiple-value-bind (new-lambda-list once-only-lambda-list rest restp envar)
      (parse-macro*-lambda-list 'define-compiler-macro* lambda-list)
    (with-split-body (body declarations docs)
      `(define-compiler-macro ,name ,new-lambda-list ,@docs
         ,@(when envar `((declare (special ,envar))))  ;; NOLINT
         ,(%once-only 'define-compiler-macro*
                      once-only-lambda-list
                      `(,@declarations
                        ,@(unless restp `((declare (ignore ,rest))))
                        ,@body)
                      env envar)))))
