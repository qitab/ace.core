;;;; Provides macros for collecting values
;;;;

(defpackage #:ace.core.collect
  (:nicknames #:google.core.collect)
  (:use #:common-lisp)
  (:export
   #:with-collectors
   #:with-collected-values))

(in-package #:ace.core.collect)

;;;
;;; With collectors
;;;

(deftype mode ()
  "The mode in which collector functions operate."
  '(member nil :cons :nconc :append))

(defun make-collector-flet-form (name place tail &key test type mode)
  "Makes the body of a collector definition.
 NAME is the name of the collector in the definition.
 The collector collects elements or cons cells starting at PLACE and ending in TAIL.
 The TEST is a uniqueness test for the elements of the collected list.
 The TYPE is the type of the elements to be collected.
 MODE can be (member nil :cons :nconc :append) and decides the mode of the collector function.
 See WITH-COLLECTORS for meaning of those values."
  (let ((value (gensym "VALUE"))
        (value-type (ecase mode
                      ((nil) type)
                      (:cons
                       (if type `(cons ,type) 'cons))
                      ((:append :nconc)
                       (if type `(or null (cons ,type list)) 'list)))))
    (flet ((name () name)
           (wrap-unique (value place test form)
             (if test
                 `(unless (find ,value ,place :test ,test)
                    ,form)
                 form)))
      `(,(name) (,value)
        ,@(when value-type `((declare (type ,value-type ,value))))
        ,(ecase mode
           ((nil) (wrap-unique
                   value place test
                   `(setf ,tail
                          (let ((,value (list ,value)))
                            (if ,tail
                                (setf (cdr ,tail) ,value)
                                (setf ,place ,value))))))
           (:cons (wrap-unique
                   `(car ,value) place test
                   `(setf ,tail
                          (if ,tail
                              (setf (cdr ,tail) ,value)
                              (setf ,place ,value)))))
           (:nconc
            `(let ,(when test
                     `((,value (delete-if
                                (lambda (elt)
                                  (find (the ,type elt) ,place :test ,test))
                                ,value))))
               (when ,value
                 (setf ,tail
                       (last (if ,tail
                                 (setf (cdr ,tail) ,value)
                                 (setf ,place ,value)))))))
           (:append
            `(let ((,value
                    ,(if test
                         `(remove-if
                           (lambda (elt) (find (the ,type elt) ,place :test ,test))
                           ,value)
                         `(copy-list ,value))))
               (when ,value
                 (setf ,tail
                       (last (if ,tail
                                 (setf (cdr ,tail) ,value)
                                 (setf ,place ,value))))))))))))

(defun parse-specs (specs)
  "Iterate over SPECS and return (values VARS INITS COLLECTORS TAIL-VARS TAIL-INITS).
 VARS are the variables used to store the final product.
 INITS are the init-forms for the VARS variables.
 COLLECTORS are the function forms for the collectors.
 TAIL-VARS are the gensym VARS that store the pointer to the tails of VARS lists.
 TAIL-INITS are the init-forms for the TAIL-VARS."
  (let (vars inits collectors tests tail-vars tail-inits finally)
    (dolist (spec specs)
      (let ((tail-var (gensym (format nil "~A-TAIL" (if (consp spec) (first spec) spec)))))
        (push tail-var tail-vars)
        (cond ((atom spec)
               (push spec vars)
               (push spec inits)
               (push tail-var tail-inits)
               (push (make-collector-flet-form spec spec tail-var) collectors))
              (t
               (destructuring-bind (var collector &key init unique type mode) spec
                 (unless (typep mode 'mode)
                   (error "Wrong mode ~A specified for collector ~A." mode collector))
                 (push var vars)
                 (push (if init `(,var ,init) var) inits)
                 (push (if init `(,tail-var (last ,var)) tail-var) tail-inits)
                 (when (eq mode :cons)
                   (push `(when ,tail-var (setf (cdr ,tail-var) nil)) finally))
                 (let ((test (and unique
                                  ;; (lambda ...) and (function ...) are (fun)called directly.
                                  ;; This inlines the code in place of funcall.
                                  (if (and (consp unique) (find (car unique) '(function lambda)))
                                      (list unique)
                                      `(,(gensym "TEST") (coerce ,unique 'function))))))
                   (when (and test (cdr test)) (push test tests))
                   (push (make-collector-flet-form
                          collector var tail-var :test (car test) :type type :mode mode)
                         collectors)))))))
    (values
     (nreverse vars)
     (nreverse inits)
     (nreverse collectors)
     (nreverse tests)
     (nreverse tail-vars)
     (nreverse tail-inits)
     (nreverse finally))))

(defmacro with-collectors ((&rest SPECS) &body body)
  "Creates collector functions for SPECS.
SPECS is a list of variable (and collector) function names.
Each place may be a single symbol denoting the variable and collector name
or it may be a lambda list (PLACE COLLECTOR &key INIT UNIQUE TYPE MODE).
The INIT form is evaluated outside the WITH-COLLECTORS scope and needs to return a proper list.
The resulting INIT list will be appended destructively.
The COLLECTOR is the name for the collector function in the scope of the macro body.
If UNIQUE is a function, it is used to test the collected values for uniqueness.
TYPE is the type of the resulting list elements that is declared and checked in safe mode.
MODE can take three values:
 NIL     - the list is build out of the elements passed to the collector function.
 :NCONC  - the list is build by destructively appending lists passed to the collector function.
 :APPEND - the list is build by non-destructively appending copies of lists passed to the collector.
 :CONS   - the list is build out of cons cells passed to the collector function.
           Note that the cons cells are terminated by setting the last CDR to NIL.
Note it is undefined what happens when the PLACE variable is rebound.
Examples:
  (with-collectors (bindings
                    (infos add-info :init (list 'a))
                    (foo add-foo :unique '=)
                    (bar add-bars :nconc list))
     (bindings '(x 0))
     (bindings '(y 1))
     (add-info 'z)
     (add-foo 1)
     (add-foo 1)
     (add-bars '(1 2 3))
     (add-bars '(4 5))
     (values bindings infos foo bar)) =>
             ((x 0) (y 1))
             (a z)
             (1)
             (1 2 3 4 5)
 See also:
  CL-UTILITIES:WITH-COLLECTORS
  QUUX:WITH-COLLECTORS
  cl-protobufs/utilities PROTO-IMPL:WITH-COLLECTORS
  IT.BESE.ARNESI:WITH-COLLECTORS"
  ;; Note that the performance of this tail collector is better than push/nreverse.
  ;; This is so because we touch each of the conses only once when creating them and appending.
  ;; The list head is tested here with each append operation.
  ;; On the other hand, push/nreverse has to touch the memory locations in two scans.
  ;; So the performance gain is from memory locality.
  (if specs
      (multiple-value-bind (vars inits collectors tests tail-vars tail-inits finally)
          (parse-specs specs)
        `(let (,@inits ,@tests)
           (declare (list ,@vars)
                    ,@(when tests `((type (function (t t) t) ,@(mapcar #'car tests)))))
           (let ,tail-inits
             (declare (list ,@tail-vars))
             (flet ,collectors
               (declare (inline ,@(mapcar #'first collectors)))
               ,@(if finally
                     `((multiple-value-prog1 (progn ,@body) ,@finally))
                     body)))))
      body))

(defmacro with-collected-values ((&rest collectors) &body body)
  "Creates a form where COLLECTORS are local functions that collect elements into lists.
Those lists are returned as VALUES in the number and order of the COLLECTORS.
The COLLECTORS spec can be COLLECTOR or a lambda list (COLLECTOR &key INIT UNIQUE TYPE MODE).
The INIT form is evaluated outside the WITH-COLLECTORS scope and needs to return a proper list.
The resulting INIT list will be appended destructively.
The COLLECTOR is the name for the collector function in the scope of the macro body.
If UNIQUE is a function, it is used to test the collected values for uniqueness.
TYPE is the type of the resulting list elements that is declared and checked in safe mode.
MODE can take three values:
 NIL     - the list is build out of the elements passed to the collector function.
 :NCONC  - the list is build by destructively appending lists passed to the collector function.
 :APPEND - the list is build by non-destructively appending copies of lists passed to the collector.
 :CONS   - the list is build out of cons cells passed to the collector function.
           Note that the cons cells are terminated by setting the CDR to NIL."
  (let* ((collectors (mapcar (lambda (c) (if (consp c) c (list c))) collectors))
         (results (mapcar (lambda (c) (gensym (symbol-name (first c)))) collectors)))
    `(with-collectors ,(mapcar #'list* results collectors)
       ,@body
       (values ,@results))))
