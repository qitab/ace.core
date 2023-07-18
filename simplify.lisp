;;; Copyright 2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

;;; The package allows to simplify Lisp forms in macros.

(defpackage #:ace.core.simplify
  (:use #:ace.core.macro #:cl)
  (:export #:simplify))

(in-package #:ace.core.simplify)

(defun common-lisp-symbol-p (symbol)
  "True if SYMBOL is in the COMMON-LISP package."
  (eql (find-package :common-lisp) (symbol-package symbol)))

#+sbcl
(defun foldable-function-p (name &optional env)
  "True if the function with NAME is not local in the environment ENV
and is foldable without side-effects.

Returns (VALUES FOLDABLE KNOWN) where:
 - FOLDABLE indicates that the function is foldable.
 - KNOWN indicates that the information could be determined.
"
  (multiple-value-bind (type local) (sb-cltl2:function-information name env)
    (cond (local (values nil nil))
          ((eq type :function)
           (let ((info (sb-int:info :function :info name)))
             (if info
                 (values (sb-c::constant-fold-arg-p name)
                         t)
                 (values nil nil))))
          (t (values nil nil)))))

(defgeneric simplify* (name form &optional env lets)
  (:documentation
   "Simplify a function FORM where the first element is NAME.

Returns a RESULT-FORM.
The RESULT-FORM is the simplifyd form or the input FORM.

Parameters:
 ENV - the lexical environment.
 LETS - the let bindings of variables to their forms.
"))

(defun %coerce-to-function-form (form env lets)
  "Takes the a COERCE or a THE FORM for a function and returns an simplifyd one.

Parameters:
 FORM - a COERCE or a THE form.
 ENV - lexical environment.
 LETS - the let bindings of variables to their simplifyd value forms.
"
  (let* ((fform (simplify
                 (ecase (first form)
                   (coerce (second form))
                   (the (third form)))
                 env lets)))
    (cond ((and (eq (first form) 'coerce)
                (typep fform '(cons (eql quote) (cons symbol)) env)
                (common-lisp-symbol-p (second fform)))
           ;; Common Lisp functions are not supposed to change.
           `(function ,(second fform)))
          ((typep fform '(cons (eql function)))
           (if (typep (second fform) '(cons (eql lambda)))
               ;; Just return the lambda.
               (second fform)
               fform))
          ((typep fform '(cons (eql lambda)))
           fform)
          ((typep fform '(cons (eql constantly)))
           (with-gensyms (args)
             `(lambda (&rest ,args) (declare (ignore ,args))
                ,(second fform))))
          ((typep fform '(cons (eql complement)))
           (with-gensyms (arg)
             `(lambda (,arg)
                (not (funcall
                      ,(simplify `(coerce ,(second fform) 'function) env lets)
                      ,arg)))))
          #+sbcl
          ((multiple-value-bind (type known) (variable-type fform env)
             (when (and known type (subtypep type 'function env))
               fform)))
          (t
           form))))

(defmethod simplify* ((name (eql 'coerce)) form &optional env lets)
  (let ((type (simplify (third form) env lets)))
    (cond ((and (typep type '(cons (eql quote)) env)
                (subtypep (second type) 'function env))
           (%coerce-to-function-form form env lets))
          ;; The compiler does a decent job otherwise.
          (t
           form))))

(defmethod simplify* ((name (eql 'the)) form &optional env lets)
  (let ((subform (simplify (third form) env lets)))
    (cond ((eq (second form) t)
           subform)
          ;; TODO(czak): This lets type information be deleted.
          ((let ((type (variable-type (third form) env)))
             (and type (subtypep type (second form) env)))
           subform)
          ((subtypep (second form) 'function env)
           (%coerce-to-function-form form env lets))
          ((and (constantp subform env)
                (typep (eval* subform env) (second form) env))
           subform)
          ((not (typep subform '(cons (eql the)) env))
           `(the ,(second form) ,subform))
          ;; (THE <type1> (THE <type2 ....
          ((subtypep (second subform) (second form) env)
           ;; inner type is more specific.
           subform)
          ((subtypep (second form) (second subform) env)
           ;; outer type is more specific.
           `(the ,(second form) ,(third subform)))
          (t
           `(the (and ,(second form) ,(second subform)) ,(third subform))))))

(defmethod simplify* ((name (eql 'if)) form &optional env lets)
  (let ((condition (simplify (second form) env lets)))
    (cond ((constantp condition env)
           (if (eval* condition env)
               (simplify (third form) env lets)
               (simplify (fourth form) env lets)))
          ;; Those are unnull and unlikely to produce side-effects.
          ((typep condition '(cons (member function lambda)) env)
           (simplify (third form) env lets))
          (t
           `(if ,condition
                ,(simplify (third form) env lets)
                ,(simplify (fourth form) env lets))))))

(defun side-effect-free-p (form &optional env)
  "True if the (already simplifyd) FORM is sure to have no side-effects.

Parameters:
 FORM - on already simplifyd form.
 ENV - lexical environment.
"
  (or (constantp form env)
      (simple-variable-p form env)
      (typep form '(cons (member function lambda declare)) env)))

(defun binding-free-p (form env lets)
  "True if FORM is unlikely to capture any bindings in the lexical environment ENV.
LETS contain the lexical variable bindings."
  (or (constantp form env)
      (and (simple-variable-p form env)
           (not (assoc form lets :test #'eq)))
      (typep form '(cons (eql function) (cons symbol)))))

(defun %let-binding-form (form env lets)
  (let ((let* (eq (first form) 'let*))
        (body (cddr form))
        (ienv env)
        (ilets lets)
        (vars nil))
    (loop for (v vform) in (second form) do
      (let ((vform (simplify vform (if let* env ienv) (if let* lets ilets))))
        (unless (side-effect-free-p vform env)
          ;; Cannot shed bindings if those cause side-effects.
          (return-from %let-binding-form form))
        #+sbcl
        (let ((type (find-type-declaration v body nil env)))
          (setf env (sb-cltl2:augment-environment
                     env :variable `(,v) :declare (and type `((type ,type ,v))))))
        (push (cons v vform) lets)
        (push v vars)))
    (loop for subform. on (cddr form)
          for opt = (simplify (car subform.) env lets)
          do (cond ((null (cdr subform.)))
                   ((not (side-effect-free-p opt env))
                    (return form)))
          finally
       (return
         (cond ((typep opt '(cons (eql declare))) nil)
               ;; Nothing was bound.
               ((eq lets ilets) opt)
               ;; Don't let vars escape.
               ((and (symbolp opt) (find opt vars :test #'eq))
                (let ((type (find-type-declaration opt body nil env))
                      (vform (lookup opt lets)))
                  (if type
                      ;; Keep any type declarations.
                      (simplify `(the ,type ,vform) ienv ilets)
                      vform)))
               ;; Unlikely to capture any bindings.
               ((binding-free-p opt env lets) opt)
               ;; Cannot determine if any variable was captured.
               (t form))))))

(defmethod simplify* ((name (eql 'let)) form &optional env lets)
  (%let-binding-form form env lets))

(defmethod simplify* ((name (eql 'let*)) form &optional env lets)
  (%let-binding-form form env lets))

(defmethod simplify* ((name (eql 'function)) form &optional env lets)
  (declare (ignore lets))
  (if (typep (second form) '(cons (eql lambda)) env)
      (second form)
      form))

(defmethod simplify* ((name (eql 'funcall)) form &optional env lets)
  (let ((fun (simplify (second form) env lets)))
    (typecase fun
      ((cons (eql function) (cons symbol)) `(,(second fun) ,@(cddr form)))
      (t form))))

(defmethod simplify* ((name (eql 'or)) form &optional env lets)
  (let (terms)
    (dolist (subform (rest form))
      (let ((opt (simplify subform env lets)))
        (push opt terms)
        (cond ((constantp opt env)
               (if (eval* opt env)
                   (return)
                   (pop terms)))
              ((typep opt '(cons (member function lambda)) env)
               (return))
              ((symbolp opt)
               (multiple-value-bind (type known) (variable-type opt env)
                 (when (and known type)
                   (multiple-value-bind (nullp known) (subtypep 'null type env)
                     (when (and known (not nullp))
                       (return)))))))))
    (setf terms (nreverse terms))
    (cond ((null terms) nil)
          ((or (null (cdr terms))
               (constantp (first terms) env)
               (typep (first terms) '(cons (member function lambda)) env))
           (first terms))
          (t
           `(or ,@terms)))))

(defun self-evaluating-p (form &optional env)
  "True if FORM is self evaluating in lexical environment ENV."
  (typep form '(or number keyword array boolean) env))

(defun folded-form (form &optional env lets)
  "Returns a folded FORM if it was constant or foldable.

Parameters:
 FORM - the form under the test.
 ENV - lexical environments.
 LETS - let bindings of variables to their value forms.
"
  (cond ((constantp form env)
         (let ((value (eval* form env)))
           (if (self-evaluating-p value env)
               (values value (not (eq value form)))
               (values `(quote ,value) t))))
        #+sbcl
        ((and (typep form '(cons symbol))
              (foldable-function-p (first form) env))
         (let* ((foldablep t)
                (args
                 (loop for arg in (rest form)
                       for opt-arg = (simplify arg env lets)
                       ;; TODO(czak): Can also pass foldable functions.
                       ;;  even if they are not constants.
                       ;;  Lists bound in LETs can also be passed.
                       unless (constantp opt-arg env)
                         do (setf foldablep nil)
                       collect opt-arg)))
           (if foldablep
               (let ((value (eval* `(,(first form) ,@args) env)))
                 (if (self-evaluating-p value env)
                     (values value t)
                     (values `(quote ,value) t)))
               (values `(,(first form) ,@args) nil))))
        (t
         (values form nil))))

(defmethod simplify* (name form &optional env lets)
  (multiple-value-bind (fform folded) (folded-form form env lets)
    (if folded fform
        (multiple-value-bind (eform expanded) (macroexpand*-1 fform env)
          (if (and expanded (not (equal fform eform)))
              (simplify eform env lets)
              eform)))))

(defun lookup (symbol bindings &key (default symbol))
  "Lookup the SYMBOL in the let BINDINGS.

Returns the binding if found or the DEFAULT if not.
"
  (let ((binding (assoc symbol bindings :test #'eq)))
    (if binding (cdr binding) default)))

;; The name simplify is used in the above code.
;; TODO(czak): Return as the second value the possibly determined type of the form value.
;; TODO(czak): Return a flag indicating whether the simplifyd form is free from LET bindings.
(defun simplify (form &optional env lets)
  "Return a simplified FORM given the lexical environment ENV.
LETS is an ASSOC list of lexical lets to look up forms bound to local variables."
  (if (consp form)
      (simplify* (first form) form env lets)
      (typecase form
        (null   nil)
        (symbol (let ((vform (lookup form lets)))
                  (cond ((eq vform form) vform)
                        ;; Otherwise, those are unlikely to bind anything.
                        ((binding-free-p vform env lets) vform)
                        ;; Cannot return non-constant values.
                        (t form))))
        (t      form))))
