;;; Defines the defun* macro which provides functions with easy ftype declarations.
;;;

(defpackage #:ace.core.defun
  ;; TODO(czak): Remove nickname. Rename file.
  (:nicknames #:google.core.defun*)
  (:use #:cl
        #:ace.core
        #:ace.core.macro
        #:ace.core.type)
  (:export #:defun* #:self #:foldable #:known))

(in-package #:ace.core.defun)

;; The functionality here is used only at compile time.
;; One would like that it is compiled the same way in all types of builds.
;; So setting the SPEED and SAFETY here to some arbitrary values.
(declaim (optimize (speed 3) (safety 1)))

;;;
;;; defun* - A defun like macro that makes defining functions types easier and
;;;          passing keyword arguments more efficient by inlining the keyword lookup.
;;;

(deftype lambda-list-state ()
  "State in which the function argument list parse is at the moment."
  '(or parameter-label (member :rest-consumed :allow-other-keys)))

(defun parse-parameter-list (parameters &key body function-name)
  "Parses the type parameter types SELF-TYPES from a SELF declaration.
 Joins the DEFUN* PARAMETERS list with the types from the SELF type declaration.
 If the SELF type declaration is missing the BODY is used to search for TYPE declarations.
 Parameters:
  PARAMETERS - the lambda list of the parameters for the function.
  BODY - the of the function with other declarations.
  FUNCTION-NAME - the name of the function for the SELF declaration.
 Returns a FINFO."
  (flet ((fail (message &rest args)
           (warn "~@[~S: ~]~?" function-name message args))) ; NOLINT
    (let (required optional rest keyword allow-other-keys aux)
      (loop with state of-type lambda-list-state = :required
            with args list = parameters
            for arg-form in args
            for lambda-keyword-p = (find arg-form lambda-list-keywords :test #'eq)
            for compound-arg-p = (consp arg-form)
            for %arg = (if compound-arg-p (first arg-form) arg-form)
            for arg-name = (cond (lambda-keyword-p nil)
                                 ((and (eq state :key)
                                       (consp %arg)
                                       (keywordp (first %arg))
                                       (symbolp (second %arg)))
                                  (second %arg))
                                 ((symbolp %arg)
                                  %arg)
                                 (t
                                  (fail "Cannot parse parameter name ~A in ~A" %arg args)))
            for arg-keyword = (cond (lambda-keyword-p nil)
                                    ((and (eq state :key) (consp %arg))
                                     (assert (keywordp (first %arg)))
                                     (first %arg))
                                    ((symbolp %arg)
                                     (make-keyword %arg)))
            for default = (and compound-arg-p (second arg-form))
            for defaultp = (and compound-arg-p (cdr arg-form) t)
            for supplied = (and compound-arg-p (third arg-form))
            for arg-type = (and arg-name body (find-type-declaration arg-name body))
            do
         (if lambda-keyword-p
             (case %arg
               (&optional
                (when (member state '(:optional :rest :key :aux))
                  (fail "Misplaced '&optional' in ~S" args))
                (setf state :optional))
               (&rest
                (when (member state '(:rest :key :aux))
                  (fail "Misplaced '&rest' in ~S" args))
                (setf state :rest))
               (&key
                (when (member state '(:key :allow-other-keys :aux))
                  (fail "Misplaced '&key' in ~S" args))
                (setf state :key))
               (&allow-other-keys
                (unless (eq state :key)
                  (fail "Misplaced '&allow-other-keys' in ~S" args))
                (setf state :allow-other-keys
                      allow-other-keys %arg))
               (&aux
                (when (eq state :aux)
                  (fail "Too many '&aux' in ~S" args))
                (setf state :aux))
               (t
                (fail "Unsupported lambda keyword '~S' in ~S" %arg args)))
             ;; Probably a parameter.
             (flet ((arg ()
                      (make-parameter :name arg-name :key arg-keyword :type arg-type :label state
                                      :default default :defaultp defaultp :supplied supplied)))
               (ecase state
                 (:required
                  (unless (symbolp arg-form)
                    (fail "Expected a symbol for the required parameter ~A in ~S" arg-form args))
                  (push (arg) required))
                 (:optional
                  (push (arg) optional))
                 (:rest
                  (when rest
                    (fail "Expected a lambda keyword after the &rest parameter in ~S" args))
                  (unless (symbolp arg-form)
                    (fail "Expected a symbol for the &rest parameter ~A in ~S" arg-form args))
                  (setf rest (arg)))
                 (:key
                  (push (arg) keyword))
                 (:allow-other-keys
                  (fail "Expected only '&aux' after '&allow-other-keys' in ~S" args))
                 (:aux
                  (push (arg) aux))))))

      (make-finfo :name function-name
                  :required (nreverse required)
                  :optional (nreverse optional)
                  :rest rest
                  :keyword (nreverse keyword)
                  :allow-other-keys (and allow-other-keys t)
                  :aux (nreverse aux)))))

(defun finfo-all-parameters (finfo &key (restp t) auxp)
  "Returns a list of all parameters from FINFO.
 If RESTP is true (as the default), the rest parameter is attached before keywords.
 If AUXP is true, also adds &AUX variables."
  (append (finfo-required finfo)
          (finfo-optional finfo)
          (and restp (finfo-rest finfo) (list (finfo-rest finfo)))
          (finfo-keyword finfo)
          (and auxp (finfo-aux finfo))))

(defun parse-self-type-declaration (finfo self-types)
  "Parses the SELF-TYPES from the SELF declaration.
 Joins the DEFUN* PARAMETERS list with the types from the SELF type declaration.
 If the SELF type declaration is missing the BODY is used to search for TYPE declarations.
 Parameters:
  FINFO - function info structure containing the parameter information.
  SELF-TYPES - the parameters type specification from the SELF declaration.
 Returns a parsed representation for each type of parameter:
  (required optional rest keyword allow-other-keys aux))"
  ;; Set the parameter types using the declarations in self-types.
  (loop with all-params of-type list = (finfo-all-parameters finfo :auxp t)
        ;; Condense :keyword type pairs to (:keyword type).
        ;; The self type declarations can have 3 forms:
        ;; - type - positional
        ;; - (:keyword type) - associated
        ;; - :keyword type  - associated
        with types = (loop with %types = self-types
                           while %types
                           for type = (pop %types)
                           collect
                           (if (keywordp type) `(,type ,(pop %types)) type))

        with state of-type lambda-list-state = :required
        with param-list = all-params
        with already-typed-params = nil
        for type-spec in types
        for previous-keyworded = nil then keyworded
        for keyworded = (and (consp type-spec) (keywordp (first type-spec)) (first type-spec))
        for type = (if keyworded (second type-spec) type-spec)
        for lambda-keyword-p = (find type-spec lambda-list-keywords :test #'eq)
        do
     (labels ((fail (message &rest args)
                (warn "~@[~S: ~]~?" (finfo-name finfo) message args) ; NOLINT
                (return))
              (set-type (param)
                (cond ((not param)
                       (fail "Parameters ~A~%and types ~S~%mismatch~@[ in ~A part~] at ~A."
                             finfo self-types
                             (unless (eq state :required) state)
                             type-spec))
                      ((find (parameter-name param) already-typed-params :test #'eq)
                       (fail "The type of parameter ~A has already been specified: ~A."
                             (parameter-name param) (parameter-type param)))
                      (t
                       (setf (parameter-type param) type)
                       (push (parameter-name param) already-typed-params)))))
       (cond (lambda-keyword-p
              (case type
                (&optional
                 (when (member state '(:optional :rest :rest-consumed :key :aux))
                   (fail "Misplaced '&optional' in ~S." self-types))
                 (setf state :optional
                       param-list (finfo-optional finfo)))
                (&rest
                 (when (member state '(:rest :rest-consumed :key :aux))
                   (fail "Misplaced '&rest' in ~S." self-types))
                 (setf state :rest))
                (&key
                 (when (member state '(:key :allow-other-keys :aux))
                   (fail "Misplaced '&key' in ~S." self-types))
                 (setf state :key
                       param-list (finfo-keyword finfo)))
                (&allow-other-keys
                 (unless (eq state :key)
                   (fail "Misplaced '&allow-other-keys' in ~S." self-types))
                 (setf state :allow-other-keys))
                (&aux
                 (when (eq state :aux)
                   (fail "Too many '&aux' in ~S." self-types))
                 (setf state :aux
                       param-list (finfo-aux finfo)))
                (t
                 (fail "Unsupported lambda keyword '~S' in ~S." type self-types))))
             (keyworded
              (let ((param (find (first type-spec) all-params :key #'parameter-key)))
                (set-type param)
                (when (and param-list (eq param (first param-list)))
                  ;; When the keyworded spec and the first param agree, treat it as positional.
                  (setf keyworded nil)
                  (pop param-list))))
             (previous-keyworded
              (fail "Consider using keyworded type specification after: ~S" previous-keyworded))
             (t
              (ecase state
                ((:required :optional :key :aux)
                 (set-type (pop param-list)))
                (:rest
                 (set-type (finfo-rest finfo))
                 (setf state :rest-consumed))
                (:rest-consumed
                 (fail "Expected a lambda keyword after the &rest type in ~S" self-types))
                (:allow-other-keys
                 (fail "Expected only '&aux' after '&allow-other-keys' in ~S" self-types)))))))
  finfo)

(defun make-ftype-declaration! (finfo &key inline)
  "Returns an ftype for the function NAME using type information from the self declaration.
  Arguments:
   FINFO - function information.
   INLINE - inline indicator: INLINE or NOINLINE."
  (let* ((name (finfo-name finfo))
         (set-finfo `(setf (finfo ',name) ,finfo))
         (required (finfo-required finfo))
         (optional (finfo-optional finfo))
         (rest (finfo-rest finfo))
         (keyword (finfo-keyword finfo))
         (values (finfo-values finfo))
         (inline (or inline (and (finfo-inline finfo) 'inline))))
    (cond (values
           (let (types)
             (dolist (param required)
               (push (or (parameter-type param) t) types))
             (when optional
               (push '&optional types)
               (dolist (param optional)
                 (push (or (parameter-type param) t) types)))
             (when rest
               (push '&rest types)
               (push (or (parameter-type rest) t) types))
             (when keyword
               (push '&key types)
               (dolist (param keyword)
                 (push `(,(parameter-key param) ,(or (parameter-type param) t)) types))
               (when (finfo-allow-other-keys finfo)
                 (push '&allow-other-keys types)))
             (setf types (nreverse types))
             `(,(cond ((finfo-foldable finfo)
                       `(declaim-foldable ,name ,types ,values))
                      ((finfo-known finfo)
                       `(declaim-known ,name ,types ,values))
                      (t
                       `(declaim (ftype (function ,types ,values) ,name))))
               ,@(when inline `((declaim (,inline ,name))))
               (eval-always ,set-finfo))))
          (inline
           ;; No arg types known, declare the function inline.
           `((declaim (,inline ,name)))))))

(defun make-positional-ftype-declaration! (name params values &key foldable known)
  "Returns an ftype declaration for NAME with keyword parameters replaced by positional ones.
  Assumes same order of keyword parameters as the type declarations.
  Arguments:
   NAME   - the name of the function.
   PARAMS - a list of parsed parameters to the function.
   VALUES - a list of the values of the function.
   FOLDABLE - if true will declare the function foldable.
   KNOWN - if true will declare the function known to compiler.
"
  (let (types (values (or values '*)))
    (dolist (param params)
      (let* ((default (parameter-default param))
             (label (parameter-label param))
             (nullable (member label '(:optional :key)))
             (supplied (parameter-supplied param))
             (type (or (parameter-type param) t)))
        (push (cond ((and type nullable (null default)) (add-null type))
                    ((eq label :rest)                   'list)
                    (t                                  type))
              types)
        (when supplied
          (push 'boolean types))))
    (setf types (nreverse types))
    (cond (foldable
           `((declaim-foldable ,name ,types ,values)))
          (known
           `((declaim-known ,name ,types ,values)))
          (t
           `((declaim (ftype (function ,types ,values) ,name)))))))

(defun derive-positional-lambda-list (params)
  "Given a lambda list ARGS with a &key symbol, derive the lambda list of parameters for the
 function with positional parameters. The lambda keywords are removed except for &aux.
 All symbols starting at '&aux' are retained in the original form.
 The second return value is the list of variables that are also found in the lambda list init-forms.
 Arguments:
  PARAMS - a concatenation of the parsed required, optional, rest, and keyword parameters."
  (let (positional ignorable)
    (labels ((parse-default (default)
               (cond ((symbolp default)
                      (when (member default positional :test #'eq)
                        (pushnew default ignorable :test #'eq)))
                     ((consp default)
                      (dolist (form (rest default))
                        (parse-default form))))))
      (dolist (param params)
        (let ((name (parameter-name param))
              (default (parameter-default param))
              (supplied (parameter-supplied param)))
          (when default (parse-default default))
          (push name positional)
          (when supplied (push supplied positional)))))
    (values (nreverse positional) (nreverse ignorable))))


(defun make-inside-type-declaration! (params &key suppliedp)
  "Returns a declaration for the arguments using type information from the SELF declaration.
  Arguments:
   PARAMS    - a parsed list of function parameters and types.
   SUPPLIEDP - true if the supplied parameter should be added with boolean type."
  (let (types)
    (dolist (param params)
      (let* ((name (parameter-name param))
             (%type (parameter-type param))
             (default (parameter-default param))
             (label (parameter-label param))
             (nullable (member label '(:optional :key)))
             (type (if (and %type nullable (null default))
                       (add-null %type)
                       (or %type t)))
             (supplied (parameter-supplied param)))
        (unless (typep %type 'boolean)
          (push `(type ,type ,name) types))
        (when (and suppliedp supplied)
          (push `(boolean ,supplied) types))))
    (when types
      `((declare ,@(nreverse types))))))

(declaim (declaration self))
;; TODO(czak): Merge this with the definition in macro.lisp.
(deftype function-name ()
  "A type of a function name: symbol or (setf symbol)."
  '(or (and symbol (not null))
    (cons (eql setf) (cons (and symbol (not null)) null))))

(defmacro defun* (&environment env name args &rest body)
  "Defines a function in a way similar to the defun macro.

This macro interprets a special 'self' declaration. If it is present in the declarations'
section of the body, the contents are used to declare the function type.

The self declaration has the following syntax:
(declare (self [inline | notinline] [foldable] [known]
               [([arg-type | lambda-list-keyword]*) return-value-type*]))

The inline flag controls the inline behavior of the function.
If inline is specified, the function is inlined in the optimized builds.

(declare (self inline)) - can be used to declare a function inline without specifying the
type of the function.

The notinline flag informs the compiler that the function should never be inlined.
It also prevents functions with keywords from being split in the optimized compile mode.

The foldable flag informs the compiler that the function is foldable to a constant value.
The known flag allows the compiler to apply deftransforms (SBCL specific).

Note: The syntax for keyword type declaration is flat. I.e. one does not specify the
name of the keyword in the keyword type list as the names have been specified in the arg list.
E.g. (defun* test (&key a b) (declare (self (&key fixnum fixnum) boolean)))

Note: The return-value-types declaration is later surrounded with a (values ... &optional) form.
Given the above example this results in:
(declaim (ftype (function (&key (:a fixnum) (:b fixnum)) (values boolean &optional)) test))

If the function contains keyword parameters, it may be split into two functions.
The first of the functions provides an interface with keywords.
This function is then inlined in order to compile the keyword argument lookup at the invocation.
The second - '%' prefixed function - provides an interface with positional parameters.
The first function calls the second with the set of arguments translated from keywords to
positional ones.

Alternative syntax allows to define an inline function by putting
INLINE after the DEFUN* and before the function name.

Parameters:
  NAME - the symbol name of the function. The macro may also generate a %NAME internal function.
  ARGS - the lambda list of the function's parameters.
  BODY - the body forms of the function.

Examples:
  (defun* function-with-optional (a &optional b)
   (declare (self (fixnum &optional fixnum) fixnum))
   (i+ a (or b 23)))

  (defun* function-with-key (a &key b)
   (declare (self (fixnum &key fixnum) fixnum))
   (i+ a (or b 42)))

  (defun* multiple-values (a)
    (declare (self (fixnum) fixnum boolean))
    (values (if (iminusp a) (i- a) a)
            (iminusp a)))

  (defun* provided-p-flag (&key (a nil a-provided-p))
     (declare (self inline (&key fixnum)))
     ;; Provided predicate is always declared of boolean type.
     ;; The accepted values for :a are of type fixnum while a can be (or null fixnum) here.
     (when a-provided-p (format nil \"~D\" a))
     (values))

  (defun* inline fx+ (a b)
     \"The FX+ function is declared inline.\"
     (declare (self (fixnum fixnum) fixnum))
     (+ a b))
"
  (declare (ignorable env))
  (let* (foldable known
         (inline-indicator
          ;; Parse out the preceding inline indicator if present.
          (when (and (eq name 'inline)
                     (typep args 'function-name)
                     (listp (first body)))
            (shiftf name args (pop body))))
         (self
          (progn
            (check-type name function-name)
            (check-type args list)
            (check-type body list)
            (rest (find-declaration 'self body))))
         (arg-list.
          (loop :for indicator. :on self :do
            (case (car indicator.)
              ((inline notinline) (setf inline-indicator (car indicator.)))
              ((foldable) (setf foldable (car indicator.)))
              ((known) (setf known (car indicator.)))
              (t (return indicator.)))))
         (values* (rest arg-list.))
         (values
          (cond ((not arg-list.)
                 (find-declaration 'values body))
                ((and (consp (car values*)) (eq (caar values*) 'values))
                 (car values*))
                ((eq (car values*) '*)
                 '*)
                (t
                 `(values
                   ,@values*
                   ;; Append &OPTIONAL to state that the VALUES list is fixed.
                   ,@(unless (intersection values* '(&rest &optional))
                       '(&optional))))))
         (setfp (and (consp name) (eq (first name) 'setf)))
         (unusual-name-p (and (consp name) (not setfp)))
         (def (if foldable 'defun! 'defun))
         (finfo (parse-parameter-list args :body body :function-name name))
         (params (finfo-all-parameters finfo :auxp nil))
         (debug (and #-dbg (= (debug-level env) 3)
                     `((declaim (notinline ,name))))))
    (check-type arg-list. list)
    (when (and foldable (not values))
      (warn "FOLDABLE declaration for ~S is missing parameter and value types." name)) ; NOLINT
    (when (and known (not values))
      (warn "KNOWN declaration for ~S is missing parameter and value types." name)) ; NOLINT

    (when arg-list.
      (parse-self-type-declaration finfo (first arg-list.)))

    (setf (finfo-inline finfo) (eq inline-indicator 'inline)
          (finfo-foldable finfo) (and foldable t)
          (finfo-known finfo) (and known t)
          (finfo-values finfo) values)

    (if (or inline-indicator    ; never split inline / notinline code
            unusual-name-p      ; only standard functions and setters are split
            (not (finfo-keyword finfo)) ; only keyworded functions are split
            (finfo-rest finfo)) ; except where the function needs a &rest list.

        `(progn
           ,@(make-ftype-declaration! finfo :inline inline-indicator)

           (,def ,name ,args
            ,@(make-inside-type-declaration! (finfo-all-parameters finfo :restp nil :auxp t))
            ,@(remove-declarations 'self body))
           ,@debug
           ',name)

        ;; Else this is an optimized build and the function has keyword parameters.
        (with-split-body (body declarations docs)
          (declare (list body declarations docs))
          (let* ((%declarations (remove-declarations 'self declarations))
                 (name-symbol (the symbol (if setfp (second name) name)))
                 (%name-symbol (intern (format nil "%~A" name-symbol)
                                       (symbol-package name-symbol)))
                 (%name (if setfp `(setf ,%name-symbol) %name-symbol))
                 (aux-args (member '&aux args))
                 (declarations (remove-declarations
                                'ignore (remove-declarations
                                         'ignorable %declarations))))

            (dolist (aux-var (finfo-aux finfo))
              ;; Remove any mention of auxiliary variables in the keyword stub declarations.
              (setf declarations
                    (remove-type-declarations (parameter-name aux-var) declarations)))

            (multiple-value-bind (%params %ignorable) (derive-positional-lambda-list params)
              (when %ignorable
                ;; Add ignorable declaration for variables mentioned in the init-forms.
                (push `(declare (ignorable ,@%ignorable)) %declarations))

              `(progn
                 ;; Forward declaration for the internal %name function.
                 ,@(make-positional-ftype-declaration!
                    %name params values :foldable foldable :known known)

                 ;; Define the interface function with keyword parameters.
                 ,@(make-ftype-declaration! finfo :inline 'inline)
                 (,def ,name ,(ldiff args aux-args) ,@docs ,@declarations
                   ,(if setfp
                        `(funcall #',%name ,@%params)
                        `(,%name ,@%params)))
                 ,@debug

                 ;; Define the internal function with positional parameters.
                 (,def ,%name ,(append %params aux-args) ,@docs
                   ,@(make-inside-type-declaration!
                      (finfo-all-parameters finfo :restp nil :auxp t)
                      :suppliedp t)
                   ,@%declarations
                   (block ,name-symbol ,@body))

                 ',name)))))))
