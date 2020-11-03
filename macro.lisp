;;; Copyright 2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

;;; Utilities that deal with function definition, lambda body, declarations, ...
;;; The package is designed to be used in other packages.
;;;
;;; defun! - defines a function available also at compile-time.
;;; eval-always - wraps the body in (eval-when (:compile-toplevel :load-toplevel :execute) ...).
;;; find-declaration - searches for a declaration in the body.
;;; find-type-declaration - searches for a type declaration for given variable.
;;; find-ignored - searches for all ignored variables in the body.
;;; remove-declarations - removes declarations from the body.
;;; remove-type-declarations - removes all type declarations for a given variable.
;;; inline-function-p - returns true if the function is declared inline.
;;; interpreted-mode-p - true if the evaluator is in interpreted-mode.
;;; optimize-for-speed-p - true if the build environment is optimized.
;;; optimize-for-speed-p! - a macro that returns true if the build is optimized.
;;; split-body - splits body into declarations, docstring, and body forms.
;;; with-split-body - extracts the body forms, declarations and a docstring from a body.
;;;

(defpackage #:ace.core.macro
  (:use #:cl #:ace.core)
  (:export
   #:1st-or-atom
   #:2nd-or-atom
   #:declaim-foldable
   #:declaim-known
   #:defun!
   #:defalias
   #:eval-always
   #:find-declaration
   #:find-declared
   #:find-type-declaration
   #:inline-function-p
   #:function-has-transforms-p
   #:function-file-path
   #:interpreted-mode-p
   #:issue-compiler-note
   #:line-and-column-numbers
   #:current-file-namestring
   #:lmap
   #:lconc

   #:strcat
   #:symcat
   #:keycat
   #:make-keyword

   #:gensym*
   #:gensymp
   #:with-gensyms

   #:sym*
   #:with-syms

   #:lexenv
   #:remove-declarations
   #:remove-type-declarations
   #:split-body
   #:with-split-body
   #:without-code-deletion-notes
   #:without-compiler-notes
   #:simple-variable-p
   #:variable-type
   #:macroexpand*-1
   #:macroexpand*

   #:once-only
   #:define-compiler-macro*

   #:speed-level
   #:safety-level
   #:debug-level
   #:space-level
   ;; TODO(czak): deprecate and remove those below.
   #:optimize-for-speed-p
   #:optimize-for-speed-p!
   #:optimize-for-debug-p
   #:optimize-for-debug-p!
   #:eval*

   ;; Macro keyword helpers:
   #:KEYS>
   #:FOR>
   #:WITH>
   #:FINALLY-RETURN>))

(in-package #:ace.core.macro)

;;;
;;; Crutches.
;;;

(macrolet ((def (or-atom ordinal)
             `(progn
                (declaim (inline ,or-atom))
                (defun ,or-atom (thing)
                  ,(format nil "~:(~A~) if a list or the THING" ordinal)
                  (if (atom thing) thing (,ordinal thing))))))
  (def 1st-or-atom first)
  (def 2nd-or-atom second))

(defmacro lmap ((var list) &rest rest)
  "LMAP binds the variables specified in REST to the values of
the corresponding lists and executes the body on each of the value sets.

Parameters:
 VAR - is the first variable symbol to bind elements from the LIST.
 LIST - is the first list to iterate over.
 REST - has the form: (VARIABLE LIST)* BODY.
 BODY - is last element of the REST argument list.

See also:
 LCONC - similar, yet concatenates the body results.
 MAPCAR - used as implementation for LMAP.

Returns a list of the results produced by the BODY on each of the value sets.
The list is as long as the shortest of the lists passed.
"
  (let* ((body (last rest))
         (specs (list* `(,var ,list) (ldiff rest body))))
    `(loop ,@(loop :for (var list) :in specs
                   :nconc `(:for ,var :in ,list))
           :collect (locally ,@body))))

(defmacro lconc ((var list) &rest rest)
"LCONC binds the variables specified in REST to the values of
the corresponding lists and executes the body on each of the value sets.

Parameters:
 VAR - is the first variable symbol to bind elements from the LIST.
 LIST - is the first list to iterate over.
 REST - has the form: (VARIABLE LIST)* BODY.
 BODY - is last element of the REST argument list.

See also:
 LMAP - similar, yet collects the body results without concatenating.
 MAPCAN - used as implementation for LCONC.

Returns a concatenated list of the results produced by the BODY on each
of the value sets.
"
  (let* ((body (last rest))
         (specs (list* `(,var ,list) (ldiff rest body))))
    `(loop ,@(loop :for (var list) :in specs
                   :nconc `(:for ,var :in ,list))
           :nconc (locally ,@body))))

(defun lexenv ()
  "Returns the current lexical environment or NIL."
  #+sbcl (and (boundp 'sb-c:*lexenv*) sb-c:*lexenv*)
  #+cmucl (and (boundp 'c::*lexical-environment*) c::*lexical-environment*)
  #+ecl (and (boundp 'compiler::*cmp-env*) compiler::*cmp-env*)
  #+ccl (and (boundp 'ccl::*nx-lexical-environment*) ccl::*nx-lexical-environment*))

(defmacro eval-always (&rest body)
  "Evaluates the BODY at compile, load, and execute time."
  `(eval-when (:compile-toplevel :load-toplevel :execute) ,@body))

(defmacro with-lexical-environment ((env) &body body)
  "Bind the lexical environment to ENV."
  #-(or sbcl cmucl ecl ccl)
  (warn "Access to lexical environment not implemented.")
  `(let (#+sbcl (sb-c:*lexenv* ,env)
         #+cmucl (c::*lexical-environment* ,env)
         #+ecl (compiler::*cmp-env* ,env)
         #+ccl (ccl::*nx-lexical-environment* ,env))
     (locally ,@body)))

(eval-always
(defun eval* (expression &optional (environment (lexenv)) default)
  "Returns (VALUES EXPRESSION-value t) for the EXPRESSION evaluated in the
lexical ENVIRONMENT or if it cannot be evaluated returns (values DEFAULT nil)."
  (cond ((not (constantp expression environment))
         (values default nil))
        (environment
         (values
          (values
           #+sbcl (sb-int:constant-form-value expression environment)
           #-sbcl
           (with-lexical-environment (environment)
             (eval expression)))
          t))
        (t
         (values (eval expression) t))))) ; NOLINT

(defmacro defun! (name args &rest body)
  "Defines a function that is available at compile time.
 Arguments:
  name - the symbol name of the function.
  args - the parameter lambda list of the function.
  body - the body forms of the function."
  `(eval-always (defun ,name ,args ,@body)))

;;;
;;; Alias
;;;

(defmacro defalias (alias function)
  "Defines an ALIAS for a FUNCTION."
  `(progn
     (declaim (ftype function ,alias))
     (when (expect (fboundp ',function))
       (setf (symbol-function ',alias)
             (symbol-function ',function)
             (documentation ',alias 'function)
             (documentation ',function 'function)))
     (define-compiler-macro ,alias (&rest args)
       `(,',function ,@args))))

;;;
;;; Macro expansion.
;;;

(deftype function-name ()
  "symbol or (setf symbol)"
  `(or symbol (cons (eql setf) (cons symbol null))))

(eval-always
(defun macroexpand*-1 (form &optional env)
  "Macroexpands the FORM in ENV inclusive the compiler-macros.

Returns the expanded form and T if it was (really) expanded.
"
  (let ((fun (and (typep form '(cons function-name) env)
                  (compiler-macro-function (car form) env))))
    (if fun
        (let ((new-form (funcall *macroexpand-hook* fun form env)))
          (values new-form (not (eql new-form form))))
        (macroexpand-1 form env)))))

(eval-always
(defun macroexpand* (form &optional env)
  "Repeatedly macroexpands the FORM in ENV inclusive the compiler-macros.

Returns the expanded form and T if it was (really) expanded.
"
  (labels ((recurse (form previously-expanded)
             (multiple-value-bind (new-form newly-expanded)
                 (macroexpand*-1 form env)
               (if newly-expanded
                   (recurse new-form t)
                   (values new-form previously-expanded)))))
    (recurse form nil))))

;;;
;;; Concatenate stuff
;;;

(eval-always
(declaim (ftype (function (string) (values t &optional)) basep)
         (inline basep))
(defun basep (string)
  "True if STRING consists only of base-chars."
  (declare #+sbcl (sb-ext:muffle-conditions sb-ext:compiler-note))
  (etypecase string
    (base-string t)
    (string (every (lambda (c) (typep c 'base-char)) string)))))

(eval-always
(declaim (ftype (function (string) (values string &optional)) maybe-base-string)
         (inline maybe-base-string))
(defun maybe-base-string (string)
  "Coerces STRING to a BASE-STRING if possible."
  (declare (string string))
  (cond ((typep string 'base-string) string)
        ((basep string) (coerce string 'simple-base-string))
        (string))))

(declaim (type (simple-array simple-base-string) +num-to-str+))
(defconstant +num-to-str+
  (if (boundp '+num-to-str+)
      (symbol-value '+num-to-str+)
      `#(,@(loop
             :for i :from 0 :to 1023
             :collect
             (coerce (write-to-string i :base 10 :radix nil) 'base-string))))
  "An array of string representation of numbers.")

(eval-always
(declaim (ftype (function (&rest t) (values simple-string &optional)) str)
         (inline str))
(defun str (x)
  "Return a string representation of a thing X.
Strings, symbols, integers, and characters are converted to a string."
  (declare #+sbcl (sb-ext:muffle-conditions sb-ext:compiler-note))
  (etypecase x
    (simple-string x)
    (null "")
    (symbol (symbol-name x))
    ((integer 0 1023) (aref +num-to-str+ x))
    (base-char (make-string 1 :element-type 'base-char :initial-element x))
    (character (make-string 1 :initial-element x))
    (string (coerce x 'simple-string))
    (integer (write-to-string x :base 10 :radix nil))))
(define-compiler-macro str (&whole whole x &environment env)
  (declare (notinline str))
  (multiple-value-bind (x expandedp) (macroexpand* x env)
    (cond ((constantp x env)
           ;; fold
           (maybe-base-string (str (eval* x env))))
          (expandedp
           `(str ,x))
          (whole)))))

(eval-always
(declaim (ftype (function (&rest t) (values simple-string &optional)) strcat)
         (inline strcat))
(defun strcat (&rest rest)
  "Make a string out of REST arguments.
If no arguments are passed, empty string is returned.
NIL arguments are converted to empty strings.
Integers are converted to a decimal representation."
  (declare (dynamic-extent rest))
  (cond ((cdr rest) (apply #'concatenate 'string (mapcar #'str rest)))
        (rest       (str (car rest)))
        ("")))
(define-compiler-macro strcat (&rest stuff &environment env)
  ;; The compiler macro tries to optimize the STUFF at compile time.
  ;; It will translate everything at compile time into a string - if possible.
  ;; It will then concatenate those strings - reducing their count.
  ;; When possible STUFF is converted to a simple-base-string.
  (labels ((convert (r)
             (let ((r (macroexpand* r env)))
               (if (constantp r env)
                   (maybe-base-string (str (eval* r env)))
                   r)))
           (conc (strings)
             (apply #'concatenate
                    (if (every #'basep strings) 'simple-base-string 'string)
                    strings))
           (str* (s)
             (if (stringp s) s `(str ,s))))
    (let* ((stuff (mapcar #'convert stuff))
           (things
            (nconc
             (loop :for stuff. :on stuff
                   :unless (stringp (car stuff.))
                     :nconc (unless (eq stuff. stuff)
                              (list (conc (ldiff stuff stuff.))))
                     :and :nconc (list (car stuff.))
                     :and :do (setf stuff (cdr stuff.)))
             (and stuff (list (conc stuff))))))
      (cond
        ((null things)
         "")
        ((null (cdr things))
         (str* (car things)))
        ((every #'stringp things)
         (conc things))
        ((every (lambda (x) (or (not (stringp x)) (basep x))) things)
         (let* ((args (loop :for s :in things
                            :collect (if (stringp s)
                                         `(,s)
                                         `(,(gensym "V-") (str ,s)))))
                (vars (remove-if-not #'cdr args)))
           `(let ,vars
              (declare (simple-string ,@(mapcar #'car vars)))
              (if (and ,@(lmap (v vars) `(basep ,(car v))))
                  (concatenate 'simple-base-string ,@(mapcar #'car args))
                  (concatenate 'simple-string ,@(mapcar #'car args))))))
        (t
         `(concatenate 'simple-string ,@(lmap (s things) (str* s)))))))))

(declaim (ftype (function (&rest t) (values symbol &optional)) symcat)
         (inline symcat))
(defun symcat (&rest rest)
  "Intern a symbol concatenated from the REST arguments into *PACKAGE*.
If there are no REST arguments, it returns a NIL."
  (declare (dynamic-extent rest))
  (values
   (cond ((cdr rest)
          (intern (apply #'strcat rest)))
         (rest
          (intern (str (first rest)))))))
(define-compiler-macro symcat (&rest rest)
  (and rest `(values (intern (strcat ,@rest)))))

(defconstant +keywords+ (find-package :keyword) "Keyword package.")

(declaim (ftype (function (&rest t) (values keyword &optional)) keycat)
         (inline keycat))
(defun keycat (&rest rest)
  "Make a keyword concatenated from the REST arguments."
  (declare (dynamic-extent rest))
  (values
   (cond
     ((null rest) :||)
     ((cdr rest)
      (intern (apply #'strcat rest) +keywords+))
     ((keywordp (car rest))
      (car rest))
     (t
      (intern (str (car rest)) +keywords+)))))
(define-compiler-macro keycat (&rest rest)
  (cond
     ((null rest) :||)
     ((cdr rest)
      `(values (intern (strcat ,@rest) +keywords+)))
     ((keywordp (car rest))
      (car rest))
     (t
      `(values (intern (str ,(car rest)) +keywords+)))))

;; alias
(declaim (ftype (function (&rest t) (values keyword &optional)) make-keyword)
         (inline make-keyword))
(defun make-keyword (&rest things)
  "Return a keyword by concatenating THINGS into a string and
then interning it into the keyword package."
  (declare (dynamic-extent things))
  (apply #'keycat things))

(define-compiler-macro make-keyword (&rest things)
  `(keycat ,@things))

;;;
;;; gensyms.
;;;

(eval-always
(defun gensymp (symbol)
  "True if SYMBOL has no package."
  (and (symbolp symbol) (not (symbol-package symbol)))))

(eval-always
(declaim (ftype (function (&rest t) (values string &optional)) genstr*)
         (inline genstr*))
(defun genstr* (thing)
  "Returns a string representation for the THING.
The THING can be a symbol, number, or character - in which case it uses STR.
Otherwise it tries to return the TYPE name or an empty string as last resort.
Used for GENSYM* aesthetic purpose only."
  ;; Look at things verbatim.
  (loop :while (typep thing '(cons (eql quote))) :do
    (setf thing (second thing)))
  ;; Derive gensym name specific to the type of thing.
  (typecase thing
    (string thing)
    ((or integer character)
     (str thing))
    (symbol
     (cond
       ((null thing) "")
       ((gensymp thing) (string-right-trim "-0123456789" thing))
       ((symbol-name thing))))
    ((cons (member function lambda))
     (str :fun))
    (t
     (let ((type (type-of thing)))
       (typecase type
         ((cons symbol) (symbol-name (first type)))
         ((member cons null nil) "")
         (symbol (symbol-name type))
         (t ""))))))
(define-compiler-macro genstr* (&whole whole x &environment env)
  (declare (notinline genstr*))
  (if (constantp x env)
      ;; fold
      (maybe-base-string (genstr* (eval* x env)))
      whole)))

(eval-always
(declaim (ftype (function (&rest t) (values symbol &optional)) gensym*)
         (inline gensym*))
(defun gensym* (&rest rest)
  "Return a gensym based on the string designators in the REST argument."
  (declare (dynamic-extent rest))
  (cond ((null (car rest))
         (gensym))
        ((cdr rest)
         (gensym (apply #'concatenate 'string (mapcar #'genstr* rest))))
        (t
         (gensym (genstr* (car rest))))))
(define-compiler-macro gensym* (&rest rest)
  (if rest
      `(gensym (concatenate 'string ,@(lmap (r rest) `(genstr* ,r)) "-"))
      `(gensym))))

(defmacro with-gensyms ((&rest symbols) &body body)
  "SYMBOLS are bound to gensyms around the BODY forms.
Each of the SYMBOL specifiers can also have the form (SYMBOL NAME)
where NAME is evaluated at runtime to create the gensym.

E.g.:
 (with-gensyms ((s :FOO) t u v)
   `(let ((,s ...))
      ...))
"
  (let ((symbols (lmap (s symbols) (if (consp s) s `(,s ',s)))))
    `(let ,(lmap ((s name) symbols) `(,s (gensym* ,name)))
       ,@body)))


(declaim (ftype (function (&rest t) (values symbol &optional)) gensym*)
         (inline sym*))
(defun sym* (&rest rest)
  "Return a free symbol that is not interned into any package.
 The symbol's name is based on the string designators in the REST argument."
  (declare (dynamic-extent rest))
  (cond ((null (car rest))
         (make-symbol ""))
        ((cdr rest)
         (make-symbol (apply #'concatenate 'string (mapcar #'genstr* rest))))
        (t
         (make-symbol (genstr* (car rest))))))
(define-compiler-macro sym* (&rest rest)
  (if rest
      `(make-symbol (concatenate 'string ,@(lmap (r rest) `(genstr* ,r))))
      `(make-symbol "")))

(defmacro with-syms ((&rest symbols) &body body)
  "SYMBOLS are bound to uninterned symbols around the BODY forms.
Each of the SYMBOL specifiers can also have the form (SYMBOL NAME)
where NAME is evaluated at runtime and used as the symbol name.

E.g.:
 (with-syms ((s :FOO) t u v)
   `(let ((,s ...))
      ...))
"
  (let ((symbols (lmap (s symbols) (if (consp s) s `(,s ',s)))))
    `(let ,(lmap ((s name) symbols) `(,s (sym* ,name)))
       ,@body)))

;;;
;;; Declarations.
;;;

(defmacro declaim-foldable (name arg-types values)
  "Declaims a function constant-foldable and defines the function type.
 This allows the compiler to optimize the function call for constant values.
 Note that the foldable function should be defined at compile-time.
 Arguments:
  NAME         - the symbol name of the function.
  ARG-TYPES    - the argument types as in a function declaration.
  VALUES       - the result values of the function.
 Related:
  defun! - which allows to define the function at compile time."
  #+sbcl
  `(eval-always
    (sb-c::defknown ,name ,arg-types ,values
        sb-c::(movable foldable flushable)
        :overwrite-fndb-silently t))
  #-sbcl
  `(declaim (ftype (function ,arg-types ,values) ,name)))

(defmacro declaim-known (name arg-types values)
  "Declaims a function known to the compiler and defines the function type.
 This allows the compiler to optimize the function call using compiler transforms.
 Note that the compiler-known function should be defined at compile-time.
 Arguments:
  NAME         - the symbol name of the function.
  ARG-TYPES    - the argument types as in a function declaration.
  VALUES       - the result values of the function.
 Related:
  defun! - which allows to define the function at compile time."
  #+sbcl
  `(eval-always
    (sb-c::defknown ,name ,arg-types ,values
        sb-c::(any) :overwrite-fndb-silently t)
    nil)
  #-sbcl
  `(declaim (ftype (function ,arg-types ,values) ,name)))

(declaim (ftype (function (list) (values &optional)) check-declarations))
(defun check-declarations (declare-form)
  "Checks that all declaration forms of the DECLARE-FORM are cons cells."
  (unless (every #'consp (rest declare-form))
    (error "not a proper declaration: ~S in:~%  ~S"
           (find-if-not #'consp (the list (rest declare-form)))
           declare-form))
  (values))

(defun defined-type-name-p (symbol &optional env)
  "True if SYMBOL is a valid type specifier in the lexical environment ENV."
  (declare (ignorable env))
  (or
    #+sbcl (sb-ext:defined-type-name-p symbol env)
    #-sbcl
    (find symbol
          '(array atom bignum bit bit-vector boolean character compiled-function
            complex cons double-float extended-char fixnum float function
            hash-table integer keyword list long-float nil null number package
            pathname random-state ratio rational real readtable sequence
            short-float simple-array simple-bit-vector simple-string simple-vector
            single-float standard-char stream string base-char symbol t vector)
          :test #'eq)))

;; TODO(czak): Remove the DEFAULT here.
(defun find-type-declaration (variable body &optional default env)
  "Returns the declared type of VARIABLE from its last type declaration in the BODY.
Returns NIL in case no type declaration has been found.

Parameters:
 VARIABLE - a symbol for the variable.
 BODY - a list of forms to search for the declarations.
 DEFAULT - is the default value to return.
 ENV - a lexical environment to lookup the type specifiers."
  (let ((type default))
    (dolist (form body type)
      (cond ((typep form '(cons (eql declare)) env)
             (dolist (spec (rest form))
               (cond ((atom spec)) ; invalid
                     ((not (symbolp (first spec)))) ; invalid
                     ((eq (first spec) 'type)
                      (when (member variable (cddr spec) :test #'eq)
                        (setf type (second spec))))
                     ((defined-type-name-p (first spec) env)
                      (when (member variable (rest spec) :test #'eq)
                        (setf type (first spec)))))))
            ((not (stringp form))
             (return type))))))

(defun simple-variable-p (symbol &optional env)
  "True if SYMBOL is the name of a VARIABLE or CONSTANT in the lexical environment ENV.

True for all symbols including constant names except SYMBOL-MACROs."
  (and (symbolp symbol)
       #+sbcl (find (sb-cltl2:variable-information symbol env)
                    '(:special :lexical :constant :global :alien))))

#+sbcl
(defun variable-type (symbol &optional env)
  "Returns (VALUES TYPE KNOWN) of the variable named by SYMBOL in the lexical ENV.

Returns (VALUES T NIL) if the type of the variable is not known.
Returns (VALUES NIL NIL) if SYMBOL cannot be determined to be a variable in ENV."
  (if (simple-variable-p symbol env)
      (multiple-value-bind (kind local alist)
          (sb-cltl2:variable-information symbol env)
        (declare (ignore kind))
        (let ((.type (assoc 'type alist)))
          (cond ((and .type (not (eq (cdr .type) t)))
                 (values (cdr .type) t))
                (local
                 (values t nil))
                (t
                 (let* ((info (sb-int:info :variable :type symbol))
                        (type (and info (sb-kernel:type-specifier info))))
                   (typecase type
                     (boolean (values t nil))
                     (t       (values type t))))))))
      (values nil nil)))

;; Mulligan for non-sbcl
;; TODO: Get this working for other lisps
#-sbcl
(defun variable-type (symbol &optional env)
  "Returns (VALUES NIL NIL) as SYMBOL cannot be determined to be a variable in ENV
for non-sbcl at the current state."
  (values nil nil))

(declaim (ftype (function (symbol list &optional list)
                          (values (or cons null) &optional)) find-declaration))
(defun find-declaration (identifier body &optional default)
  "Returns the last declaration specifier that matches the IDENTIFIER.
 The BODY may contain multiple declarations and strings.
 Returns DEFAULT if the declaration has not been found in the body.
 Example:
  (find-declaration 'timeout '(\"doc\" (declare (fixnum x) (timeout 10)))) -> (timeout 10)"
  (let ((declaration default))
    (dolist (form body declaration)
      (cond ((and (consp form) (eq (first form) 'declare))
             (check-declarations form)
             (setf declaration
                   (or (find identifier (the list (rest form)) :key #'first)
                       declaration)))
            ((not (stringp form))
             (return declaration))))))

(declaim (ftype (function (symbol list) (values list &optional)) remove-declarations))
(defun remove-declarations (identifier body)
  "Returns the BODY with all declarations that match the IDENTIFIER removed.
 The function does not cons when the declaration is not found in the BODY.
 Should a declare form become empty by removing the declaration, the declare form is removed, too.
 Example:
  (remove-declarations 'timeout '(\"doc\" (declare (fixnum x) (timeout 10)) (* x x)))
   -> (\"doc\" (declare (fixnum x)) (* x x))"
  (if (find-declaration identifier body)
      (loop with stop = nil
            for forms on body
            for form = (first forms)
            nconc (cond ((and (consp form) (eq (first form) 'declare))
                         (check-declarations form)
                         (if (find identifier (rest form) :key #'first :test #'eq)
                             (let ((rest (remove identifier (rest form) :key #'first :test #'eq)))
                               (and rest (list (cons 'declare rest))))
                             (list form)))
                        ((stringp form)
                         (list form))
                        (t
                         (setf stop t)
                         forms))
            until stop)
      body))

(declaim (ftype (function (symbol list) (values list &optional)) remove-declarations))
(defun remove-type-declarations (variable body)
  "Returns the BODY with all type declarations for VARIABLE removed.
 The function does not cons when the declaration is not found in the BODY.
 Should a declare form become empty by removing the declaration, the declare form is removed, too.
 Example:
  (remove-type-declarations 'x '(\"doc\" (declare (fixnum x) (timeout 10)) (* x x)))
   -> (\"doc\" (declare (timeout 10)) (* x x))"
  (unless (find-type-declaration variable body)
    (return-from remove-type-declarations body))

  (loop with stop = nil
        for forms on body
        for form = (first forms)
        nconc
        (cond ((typep form '(cons (eql declare)))
               (check-declarations form)
               (let ((specs '()))
                 (dolist (spec (rest form))
                   (cond ((atom spec))
                         ((not (symbolp (first spec))))
                         ((eq (first spec) 'type)
                          (if (member variable (cddr spec) :test #'eq)
                              (let ((rest-vars (remove variable (cddr spec) :test #'eq)))
                                (when rest-vars
                                  (push (list* 'type (second spec) rest-vars) specs)))
                              (push spec specs)))
                         ((defined-type-name-p (first spec))
                          (if (member variable (cdr spec) :test #'eq)
                              (let ((rest-vars (remove variable (cdr spec) :test #'eq)))
                                (when rest-vars
                                  (push (list* (first spec) rest-vars) specs)))
                              (push spec specs)))
                         (t
                          (push spec specs))))
                 (when specs
                   (list (list* 'declare (nreverse specs))))))
              ((stringp form)
               (list form))
              (t
               (setf stop t)
               forms))
        until stop))

(declaim (ftype (function (symbol list) (values list &optional)) find-declared))
(defun find-declared (indicator body)
  "Returns a list of things that have been declared with INDICATOR in the BODY.
 The body may contain multiple declarations and strings.
 INDICATOR maybe e.g.: IGNORE, IGNORABLE, INLINE, DYNAMIC-EXTENT, ...
 Example:
  (find-declared '(\"doc\" (declare (ignore x y) (fixnum z))) 'ignore) => (x y)"
  (let (declared)
    (dolist (form body (nreverse declared))
      (cond ((and (consp form) (eq (first form) 'declare))
             (check-declarations form)
             (dolist (declaration (rest form))
               (when (eq (first declaration) indicator)
                 (dolist (item (rest declaration))
                   (push item declared)))))
            ((not (stringp form))
             (return (nreverse declared)))))))

(declaim (ftype (function (list &key (:docsp boolean))
                          (values list list list &optional)) split-body))
(defun split-body (body &key (docsp t))
  "Split apart the BODY into: {declaration|docs}* forms*.
 If DOCSP is T, as by default, the first string in the BODY header will be treated as documentation.
 Return (values forms declarations docs):
  forms - lists the remaining forms of the input BODY after the declarations and the docstring.
  declarations - lists the collected declarations found in the input BODY.
  docs - is nil or a one element list containing the docstring.
 The function signals an error in case the docstring is duplicated."
  (let ((forms body) declarations docs)
    (loop for form = (car forms) do
      (cond ((and (consp form) (eq (car form) 'declare))
             (check-declarations form)
             (push (pop forms) declarations))
            ((and docsp (cdr forms) (stringp form))
             (unless (null docs) (error "Duplicated docstring ~S in ~S" form body))
             (push (pop forms) docs))
            (t (return))))
    (values forms (nreverse declarations) docs)))

(defmacro with-split-body ((parsed-body &optional declarations (docs nil docsp)) &body macro-body)
  "Splits the PARSED-BODY into the remaining forms, DECLARATIONS and a docstring.
 The declarations retain the (declare ...) special form.
 The output variable DOCS contains a one element list with a docstring or is NIL.
 If DOCS is specified as NIL, the first string in the BODY
 will stop the parsing of the declarations and will not be parsed as a docstring.
 Arguments:
  parsed-body - as input contains the declarations, a docstring and remaining forms;
                as output contains only the remaining forms.
  declarations - contain the collected declarations.
  docs - is nil or a one element list containing the docstring.
 See also:
  find-declaration."
  `(multiple-value-bind (,parsed-body ,@(when declarations `(,declarations)) ,@(when docs `(,docs)))
       (split-body ,parsed-body ,@(when (and docsp (null docs)) '(:docsp nil)))
     ,@macro-body))

(defun speed-level (&optional env)
  "Return the speed optimization level for the lexical ENV environment."
  #+sbcl (sb-c:policy (or env sb-c::*policy*) speed)
  #-sbcl 1)

(defun safety-level (&optional env)
  "Return the safety optimization level for the lexical ENV environment."
  #+sbcl (sb-c:policy (or env sb-c::*policy*) safety)
  #-sbcl 1)

(defun debug-level (&optional env)
  "Return the debug optimization level for the lexical ENV environment."
  #+sbcl (sb-c:policy (or env sb-c::*policy*) debug)
  #-sbcl 1)

(defun space-level (&optional env)
  "Return the space optimization level for the lexical ENV environment."
  #+sbcl (sb-c:policy (or env sb-c::*policy*) space)
  #-sbcl 1)

(defun optimize-for-speed-p (&optional environment)
  "True if the current compiler optimization mode favors speed in the ENVIRONMENT provided."
  #+sbcl (sb-c:policy
          (or environment sb-c::*policy*)
          (and (> speed 1)
               (> speed space)
               (> speed safety)
               (> speed debug))))

(defmacro optimize-for-speed-p! (&environment environment)
  "True if the current lexical ENVIRONMENT compiles as optimized for speed."
  (optimize-for-speed-p environment))

(defun optimize-for-debug-p (&optional environment)
  "True if the ENVIRONMENT is setup to include debug information."
  #+sbcl (sb-c:policy
          (or environment sb-c::*policy*)
          (or (> debug 2)
              (and (> debug 1)
                   (>= debug space)
                   (>= debug speed)))))

(defmacro optimize-for-debug-p! (&environment environment)
  "True if the current lexical ENVIRONMENT compiles to include debug information."
  (optimize-for-debug-p environment))

(defun interpreted-mode-p ()
  "True if the code is compiled or executed by an interpreter."
  (or #+sbcl (not (eq sb-ext:*evaluator-mode* :compile))))

(defun inline-function-p (function)
  "Test if the FUNCTION is declared inline."
  (or #+sbcl (eq (sb-int:info :function :inlinep function) 'inline)))

(defun function-has-transforms-p (function)
  "True if the FUNCTION has source transforms."
  (or #+sbcl
      (sb-c::info :function :source-transform function)
      #+sbcl
      (let ((info (sb-c::info :function :info function)))
        (and info (sb-c::fun-info-transforms info)))))

;;; Source location
;;; TODO(czak): Consider finding a dedicated home for this function

(declaim (ftype (function (symbol) (values (or null pathname) &optional))
                function-file-path))
(defun function-file-path (symbol)
  "If SYMBOL names a bound function, returns the pathname from its source
location information."
  (let* ((function (and (fboundp symbol) (fdefinition symbol))))
    (and function
         #+sbcl (sb-introspect:definition-source-pathname
                 (sb-introspect:find-definition-source function)))))

(defun line-and-column-numbers (&optional whole)
  "Returns the starting and ending line and column numbers of the form processed by the compiler.
 WHOLE is a form for which the line and column numbers are to be returned.
 Returns (values start-line start-column)."
  #+google3 (sb-ext:current-line-and-column whole))

(defun current-file-namestring ()
  "Returns the namestring for the current file being compiled."
  (cond #+sbcl
        (sb-c::*source-namestring*)
        (*compile-file-pathname* (namestring *compile-file-pathname*))
        (*load-pathname* (namestring *load-pathname*))))

(defun issue-compiler-note (&rest args)
  "When in compilation mode, issue a simple compiler note constructed from ARGS."
  #+sbcl
  (when (boundp 'sb-c:*lexenv*)
    (apply #'sb-c:compiler-notify args)))

(defmacro without-compiler-notes (&body body)
  "Muffle compiler notes within the BODY."
  `(locally (declare #+sbcl (sb-ext:muffle-conditions sb-ext:compiler-note))
     ,@body))

(defmacro without-code-deletion-notes (&body body)
  "Muffle compiler code deletion notes within the BODY."
  `(locally (declare #+sbcl (sb-ext:muffle-conditions sb-ext:code-deletion-note))
     ,@body))


(defmacro KEYS> (&rest keywords)
  "For each of the KEYWORDS, if non-NIL expand to :KEYWORD KEYWORD.
The keywords can have an alternate form (:KEY VAR) then the VAR is checked
for NIL and the keyword is expanded to :KEY VAR instead.
Use case:

  `(foo ,@(KEYS> a b (:foo bar)))

will expand to:

  `(foo :a ,a :b ,b :foo ,bar) - if all the variables are present.
"
  `(append
    ,@(lmap (v keywords)
             (if (consp v)
                 `(and ,(second v) `(,',(first v) ,,(second v)))
                 `(and ,v `(,',(keycat v) ,,v))))))

(defun FOR> (variable code)
  "Inserts a conditional :FOR loop statement for the VARIABLE using CODE like:

  `(loop ,@(FOR> index `(fixnum :from 0)) ...)"
  (and variable `(:for ,variable ,@code)))

(defun WITH> (variable code)
  "Inserts a conditional :WITH loop statement for the VARIABLE using CODE like:

  `(loop ,@(WITH> index `(fixnum = -1)) ...)"
  (and variable `(:with ,variable ,@code)))

(defun FINALLY-RETURN> (variable &optional (code variable))
  "Inserts a conditional :FINALLY RETURN loop statement
for the VARIABLE using CODE. E.g.:

  `(loop :do ...
     ,@(FINALLY-RETURN> result `(nreverse ,result)))"
  (and variable `(:finally (return ,code))))
