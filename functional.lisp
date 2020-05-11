;;; Package containing utilities related to functional programming.
;;;

(defpackage #:ace.core.functional
  (:use #:common-lisp
        #:ace.core #:ace.core.macro
        #:ace.core.once-only
        #:ace.core.simplify)
  #+sbcl (:import-from #:sb-cltl2 #:function-information)
  (:export #:∘
           ;; Deprecated, use the shorter version: ∘.
           #:compose
           ;; Binds arguments of a function.
           #:bind))

(in-package #:ace.core.functional)

(deftype designator ()
  "Thing used to designate a function."
  '(or function symbol cons))

(deftype designator-form ()
  "Form a of a function designator."
  '(cons (member function quote)
    (cons (or symbol (cons (eql setf) (cons symbol null))) null)))

(defvar *lexenv* nil "Lexical environment.")

#+sbcl
(defun function-lambda-list (fn &optional (env *lexenv*))
  "Return the LAMBDA-LIST for a function FN in lexical environment ENV."
  ;; This effectively reconstructs the LAMBDA-LIST using lexical environment
  ;; bound to the *LEXENV* variable.
  (unless env
    (return-from function-lambda-list
      (multiple-value-bind (ll known) (ace.core.type:function-lambda-list fn)
        (cond (known
               (values ll :info))
              ;; TODO(czak): Cannot use this as it would be misleading
              ;;   to optimize the compiled code based on info not in OPT.
              #+(or)
              ((fboundp fn)
               ;; Introspect only works in DEVELOPER/DEBUG mode.
               (values (sb-introspect:function-lambda-list fn) :introspect))
              (t
               (values nil nil))))))

  ;; Lexical environment is only good for local functions.
  (let ((fn.info (assoc fn (sb-c::lexenv-funs env))))
    (typecase fn.info
      (null
       (let ((env (sb-c::lexenv-parent env)))
         (function-lambda-list fn env)))
      ((cons symbol sb-c::clambda)
       (let ((lvars (sb-c::lambda-vars (cdr fn.info))))
         (if lvars
             (values (mapcar #'sb-c::lambda-var-%source-name lvars) :clambda)
             ;; The clambda may come from FTYPE declaration only.
             (values nil nil))))
      ((cons symbol sb-c::optional-dispatch)
       (let (args optionalp keyp)
         (dolist (arg (sb-c::optional-dispatch-arglist (cdr fn.info)))
           (let* ((name (sb-c::lambda-var-%source-name arg))
                  (info (sb-c::lambda-var-arg-info arg))
                  (kind (and info (sb-c::arg-info-kind info)))
                  (supp (and info (sb-c::arg-info-supplied-p info)))
                  (sup-name (and supp (sb-c::lambda-var-%source-name supp)))
                  (key  (and info (sb-c::arg-info-key info)))
                  (default (and info (sb-c::arg-info-default info))))
             (ecase kind
               ((nil :required)
                (push name args))
               (:optional
                (unless optionalp
                  (push '&optional args)
                  (setf optionalp t))
                (push (cond (sup-name `(,name ,default ,sup-name))
                            (default `(,name ,default))
                            (t name))
                      args))
               (:rest
                (push '&rest args)
                (push name args))
               (:keyword
                (unless keyp
                  (push '&key args)
                  (setf keyp t))
                (let ((var (if (or (eq name key)
                                   (string= (symbol-name name)
                                            (symbol-name key)))
                               name
                               `(,key ,name))))
                (push (cond (sup-name `(,var ,default ,sup-name))
                            (default `(,var ,default))
                            ((eq var name) name)
                            (t `(,name)))
                      args))))))
         (when (sb-c::optional-dispatch-allowp (cdr fn.info))
           (push '&allow-other-keys args))
         (values (nreverse args) :optional-dispatch))))))

(defun* ftype-declaration (name &optional (env *lexenv*))
  "Return the FTYPE for a function name in ENV environment."
  (declare (self (symbol &optional t) list))
  (multiple-value-bind (type local declarations) (function-information name env)
    (declare (ignore type local))
    (cdr (assoc 'ftype declarations))))

(defun rest-lambda-list ()
  "Default lambda list for derived functions where there is not enough info."
  (let ((rest (gensym* :rest)))
    (values `(&rest ,rest) rest nil)))

(defun simple-lambda-list (designator &key (applied-count 0) (env *lexenv*))
  "Returns a simple lambda list for COMPOSE or BIND functions
given a function DESIGNATOR and a lexical environment ENV.
As a second value returns all the parameter names from the lambda-list.
As a third value return argument TYPE declarations if any.
APPLIED-COUNT describes the partially applied number of arguments."
  (declare (fixnum applied-count))
  (unless (typep designator 'designator-form env)
    ;; Cannot derive any function signature?
    (return-from simple-lambda-list (rest-lambda-list)))

  (unless (eq (first designator) 'function)
    ;; Use global lexical environment.
    (setf env nil))

  (let* ((ftype (ftype-declaration
                 (second designator)
                 (and (eq (first designator) 'function) env)))
         (types (second ftype)))
    (labels ((optional-lambda-list (ll args)
               ;; LL and ARGS have already been shifted by applied count.
               ;; But the argument types need to be adjusted.
               (assert (or (null types) (member '&optional types))) ; NOLINT
               (let ((types (nthcdr applied-count (remove '&optional types))))
                 (values ll args (lmap (a args) (at types) `(type ,at ,a)))))
             (fixed-lambda-list (ll)
               ;; Args and argument types need to be shifted by applied count.
               (assert (not (intersection types lambda-list-keywords))) ; NOLINT
               (let ((args (mapcar #'gensym* (nthcdr applied-count ll)))
                     (types (nthcdr applied-count types)))
                 (values
                  args args (lmap (a args) (at types) `(type ,at ,a)))))
             (simple-lambda-list (ll)
               (cond
                 (ll (fixed-lambda-list ll))
                 ((and ftype (not (intersection types lambda-list-keywords)))
                  ;; Derive the fixed lambda list from the ftype.
                  (fixed-lambda-list (mapcar #'gensym* types)))
                 (t
                  ;; Fall back to the rest list - no info given.
                  (rest-lambda-list)))))
      (multiple-value-bind (ll ll-source)
          (function-lambda-list (second designator) env)
        (ecase ll-source
          ((:optional-dispatch :introspect :info)
           ;; If we have the real lambda-list and there are no supplied
           ;; parameters and the defaults are constant, than it can be optimized
           ;; Otherwise use &rest for the optional, and rest arguments.
           (cond
             ((intersection ll '(&rest &key))
              ;; There is no optimization possible.
              (rest-lambda-list))
             ((find '&optional ll)
              ;; Split the list by applied-count.
              ;; If remaining optional args have supplied-p parameters,
              ;; or their defaults are non-const,
              (let* ((opts (member '&optional ll))
                     (fixed (ldiff ll opts))
                     (fixed-count (length fixed))
                     new-ll new-args)
                (declare (fixnum fixed-count))
                (pop opts)
                (cond
                  ((< applied-count fixed-count)
                   (setf fixed (nthcdr applied-count fixed))
                   (decf fixed-count applied-count))
                  (t
                   (setf fixed nil
                         opts (nthcdr (- applied-count fixed-count) opts)
                         fixed-count 0)))
                (when (some (lambda (opt)
                              (and (listp opt)
                                   (or (not (constantp (second opt) env))
                                       (third opt))))
                            opts)
                  (return-from simple-lambda-list (rest-lambda-list)))
                (dolist (f fixed)
                  (push (gensym* f) new-ll)
                  (push (car new-ll) new-args))
                (when opts (push '&optional new-ll))
                (dolist (o opts)
                  (let ((arg (gensym* (if (atom o) o (first o)))))
                    (push (if (and (listp o) (cdr o))
                              `(,arg ,(second o))
                              arg)
                          new-ll)
                    (push arg new-args)))
                (optional-lambda-list
                 (nreverse new-ll) (nreverse new-args))))
             ((intersection ll lambda-list-keywords)
              ;; Just to make sure.
              (rest-lambda-list))
             (t
              (simple-lambda-list ll))))
          ((nil :clambda) ; simple args
           (simple-lambda-list ll)))))))

;;;
;;; Function composition
;;;

;;; The functional composition symbol is 'RING OPERATOR/COMPOSITE' U+2218
;;; In Emacs unicode can be input using:
;;;  - C-x 8 RET 2218 RET
;;;  - C-q 2 1 0 3 0 SPACE
;;; It is preferred to create a key binding.
;;; E.g.: (global-set-key (kbd "C-c o") "∘")
;;;
;;; In VIM: <C-v>u2218
;;;
;;; Linux: S-C-u 2218 SPACE.
;;; After pressing SHIFT-CTRL-U an underlined 'u' character should appear.
;;; If the 'u' character is not there, make sure that iBUS input-method is
;;; selected and that the iBUS demon is running.
;;;
(defun* ∘ (&rest functions)
  "Compose the FUNCTIONS to a function: (∘ f g h)(x) => (f (g (h x)))."
  (declare (self foldable (&rest designator) function)
           (dynamic-extent functions))
  (cond ((cdr functions)
         (let (%functions)
           (dolist (f functions)
             (push (coerce f 'function) %functions))
           ;; Actual lambda result.
           (lambda (&rest rest)
             (let ((result (apply (the function (first %functions)) rest)))
               (dolist (f (rest %functions) result)
                 (setf result (funcall (the function f) result)))))))
        (functions
         (coerce (first functions) 'function))
        (t
         #'values)))

(define-compiler-macro* ∘
    (&rest (fns (lmap (f fns) `(coerce ,f 'function))) &environment *lexenv*)
  (declare (function fns) (inline fns))
  (cond ((cdr fns)
         (let* ((fns (nreverse fns))
                (1st (first fns)))
           (multiple-value-bind (lambda-list args types)
               (simple-lambda-list 1st :env *lexenv*)
             `(lambda ,lambda-list
                ,@(when types `((declare ,@types)))
                ,(reduce
                  (lambda (g f) (simplify `(funcall ,f ,g) *lexenv*))
                  (rest fns) :initial-value
                  (if (eq (first lambda-list) '&rest)
                      `(apply ,1st ,args)
                      (simplify `(funcall ,1st ,@args) *lexenv*)))))))
        (fns
         (first fns))
        (t
         '#'values)))

(defun* compose (&rest functions)
  "DEPRECATED: Alias for the composition operation of FUNCTIONS: ∘ ."
  (declare (self inline foldable (&rest designator) function))
  (apply #'∘ functions))

(define-compiler-macro compose (&rest functions) `(∘ ,@functions))

(setf (symbol-function 'compose) (symbol-function '∘))

;;;
;;; Partial Application.
;;;

(defun* bind (f &rest args)
  "Return a function created from F where the values of first ARGS are bound."
  (declare (self inline foldable (designator &rest t) function))
  (let ((f (coerce f 'function)))
    (declare (function f))
    (lambda (&rest rest)
      (declare (dynamic-extent rest))
      (multiple-value-call f (values-list args) (values-list rest)))))

(define-compiler-macro* bind ((f `(coerce ,f 'function))
                              &rest args &environment *lexenv*)
  (declare (function f) (inline f))
  (unless args (return f))
  (multiple-value-bind (lambda-list rest types)
      (simple-lambda-list f :applied-count (length args) :env *lexenv*)
    `(lambda ,lambda-list
       ,@(when types `((declare ,@types)))
       ,(if (eq (first lambda-list) '&rest)
           `(apply ,f ,@args ,rest)
            (simplify `(funcall ,f ,@args ,@rest) *lexenv*)))))
