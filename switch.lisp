;;; Copyright 2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

;;;;
;;;; A simple switch macro, sans the problems of other implementations.
;;;;

(defpackage #:ace.core.switch
  (:use #:cl #:ace.core #:ace.core.macro)
  (:export #:switch #:eswitch #:switch*))

(in-package #:ace.core.switch)

(defun! parse-switch-form (clauses)
  "Parses the CLAUSES of a switch form. Returns (values CLAUSES OTHERWISE TEST)."
  (declare (list clauses))
  (let ((test '#'eql))
    (when (eq (first clauses) :test)
      (pop clauses)
      (setf test (pop clauses)))
    (let ((otherwise (member :otherwise clauses)))
      (values (ldiff clauses otherwise) (rest otherwise) test))))

(defun! make-switch-form (value test clauses otherwise)
  "Returns a SWITCH form given VALUE, TEST, CLAUSES, and OTHERWISE."
  `(cond ,@(loop for clause in clauses
                 collect `((funcall ,test ,value ,(first clause)) ,@(rest clause)))
         ,@(when otherwise
             `((t ,@otherwise)))))

(defmacro switch (value &rest clauses)
  "Provides a CASE like construct that evaluates the VALUE and compares it to every
 CAR of the CLAUSES using TEST. If a clause matches, the rest of the clause is executed and
 the execution of the SWITCH form terminates. The OTHERWISE clause is executed if none of
 the previous clauses matched. This is implemented using a COND.
 :TEST can be placed right after the first SWITCH value FORM.
 :OTHERWISE has to follow all other CLAUSES.
 Example:
  (switch (object-color object) :test #'string=
    (\"RED\" :red)
    (\"BLUE\" :blue)
    :otherwise (error \"Unknown color\"))"
  (multiple-value-bind (clauses otherwise test) (parse-switch-form clauses)
    (once-only (value test)
      (make-switch-form value test clauses otherwise))))

(defmacro eswitch (value &rest clauses)
  "Provides a ECASE like construct that evaluates the VALUE and compares it to every
 CAR of the CLAUSES using TEST. If a clause matches, the rest of the clause is executed and
 the execution of the ESWITCH form terminates. If no clause matches, an error is signaled.
 This is implemented using a COND.
 Example:
  (eswitch (object-color object) :test #'string=
    (\"RED\" :red)
    (\"BLUE\" :blue))"
  (multiple-value-bind (clauses otherwise test) (parse-switch-form clauses)
    (when otherwise (error "ESWITCH does not accept an OTHERWISE clause."))
    (once-only (value test)
      (make-switch-form
       value test clauses
       (if (find-if-not #'constantp clauses :key #'first)
           `((error "Value ~S did not match any of the ESWITCH clauses." ,value))
           `((error "Value ~S did not match any of the ESWITCH clauses. Expected:~{ ~S~}."
                    ,value ',(mapcar #'first clauses))))))))

(defmacro switch* (value &rest clauses)
  "Provides a CASE like construct that evaluates the VALUE and compares it to every
 CAR of the CLAUSES using TEST. If a clause matches, the rest of the clause is executed.
 The following clauses are also executed without evaluating their test forms.
 To prevent the evaluation of a subsequent clause a RETURN statement can be used.
 The OTHERWISE clause is executed if no previous clause matched or returned.
 SWITCH* is implemented using the PROG environment.
 :TEST can be placed right after the first SWITCH value FORM.
 :OTHERWISE has to follow all other CLAUSES.
 Example:
  (switch* (object-color object) :test #'string=
    (\"RED\" (format t \"Actually, RED\"))
    (\"BLUE\" (return :blue))
    :otherwise (error \"Unknown color\"))"
  (multiple-value-bind (clauses otherwise test) (parse-switch-form clauses)
    (once-only (value test)
      (let ((tags (loop for clause in clauses collect (gensym "TAG")))
            (otherwise-tag (gensym "OTHERWISE")))
        `(prog ()
            (cond
              ,@(loop for clause in clauses
                      for tag in tags
                      collect `((funcall ,test ,value ,(first clause)) (go ,tag))))
            (go ,otherwise-tag)
            ,@(loop for clause in clauses
                    for tag in tags
                    append `(,tag (progn ,@(rest clause))))
            ,otherwise-tag
            ,@(when otherwise
                `((return (progn ,@otherwise)))))))))
