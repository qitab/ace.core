;;; Copyright 2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

;;; Variations on CHECK assertion.
;;;
;;; EXPECT       - an assertion that only warns instead of signaling an error.
;;; CHECK        - an assertion that does not insert the CONTINUE restart code.
;;; DCHECK       - a debug mode version of CHECK that returns (VALUES).
;;;
;;; cllint: disable=invalid-assert
;;; cllint: disable=warn-not-log

(defpackage #:ace.core.check.condition
  (:export
   #:alternate-truth-form
   #:*on-missed-expectation*
   #:check-file
   #:check-line
   #:failed
   #:failed*
   #:missed
   #:missed*))

(defpackage #:ace.core.check
  (:use #:ace.core
        #:ace.core.check.condition
        #:common-lisp)
  (:import-from #:ace.core.macro
                ace.core.macro:strcat
                ace.core.macro:current-file-namestring
                ace.core.macro:line-and-column-numbers
                ace.core.macro:without-code-deletion-notes)
  ;; TODO(czak): Extract the printer generator out of here.
  ;; Generating a form of a function/macro call for pseudo-types
  ;; should be a different aspect from this stuff.
  (:import-from #:ace.core.type
                ace.core.type:variable-information
                ace.core.type:remove-null
                ace.core.type:info-format
                ace.core.type:function-information
                ace.core.type:function-form-argument-types)
  (:export
   #:check
   #:dcheck
   #:expect))

(in-package #:ace.core.check)

;;;
;;; Define the conditions system.
;;;

(declaim (type (or null function) *failure-hook*))
(defvar *on-missed-expectation* nil
  "Functions called from the missed EXPECT handler.")

(define-condition check (condition)
  ((file
    :type (or null string) :reader check-file :initarg :file :initform nil)
   (line
    :type (or null integer) :reader check-line :initarg :line :initform nil)
   (package
    :type (or null package)
    :reader check-package
    :initarg :package
    :initform nil)
   (format-control
    :type (or string function)
    :reader check-format-control
    :initarg :format-control
    :initform (warn "No :format-control string specified."))
   (format-arguments
    :type list
    :reader check-format-arguments
    :initarg :format-arguments
    :initform nil))
  (:report print-check))

(defgeneric fill-values (check values)
  (:documentation
   "Returns the CHECK condition, with additional format arguments substituted.
If VALUES list is empty, the original CHECK condition is returned.")
  (:method (check (values null)) check))

(defmethod fill-values ((check check) (values list))
  (if (and (every #'null values)
           (= (ash (length values) 1)
              (1- (length (check-format-arguments check)))))
      check ;; Nothing changes then.
      (with-slots (format-control format-arguments file line package) check
        (make-condition
         (type-of check)
         :file file :line line :package package
         :format-control format-control
         :format-arguments
         (list* (first format-arguments)
                (loop :for (form) :on (rest format-arguments) :by #'cddr
                      :for value :in values
                      :nconc `(,form ,value)))))))

(defun print-check (check stream)
  "Print the CHECK condition to STREAM possibly including file and line."
  (declare (type check check))
  (with-slots (format-control format-arguments file line package) check
    (let ((*package* (or package (find-package :common-lisp-user)))
          (*print-circle* t)
          #+sbcl
          (sb-ext:*suppress-print-errors* t))
      (write-string
       (with-output-to-string (stream)
         (declare (stream stream))
         (pprint-logical-block (stream format-arguments)
           (declare (stream stream))
           (cond (file
                  (format stream "[~A~@[:~D~]] " (pathname-name file) line))
                 (package
                  (format stream "[:~(~A~)] " (package-name package))))
           (handler-case
               (apply #'format stream format-control format-arguments)
             (error (e)
               (format stream "<could not print ~A condition: ~A>"
                       (type-of check)
                       (or (ignore-errors (princ-to-string e))
                           (type-of e)))))))
       stream))))

;;; CHECK

(define-condition failed (error check) ()
  (:documentation "A type of ERROR used for failed checks."))

(declaim (ftype (function (failed) nil) failed))
(defun failed (check)
  "Handles a CHECK failure."
  (error check))

(declaim (ftype (function (failed &rest t) nil) failed*))
(defun failed* (check &rest args)
  "Handles a CHECK failure. ARGS are used to fill values for CHECK."
  (error (fill-values check args)))

(defgeneric check (result &optional datum &rest arguments)
  (:documentation
   "Returns RESULT if non-NIL. Otherwise signals error.

Note: this is not a pure generic-function. It may be compiler-optimized.

DATUM can be a symbol or string. In that case the ARGUMENTS
are used together with DATUM to create the condition.
If DATUM is a string, it is used as a format control string.
If DATUM is a symbol, it is used as the first argument to MAKE-CONDITION."))

(defmethod check (result &optional datum &rest arguments)
  (declare (ignore datum arguments))
  result)

(defmethod check ((result null) &optional datum &rest arguments)
  (declare (ignore result))
  (error
   (apply #'coerce-to-condition 'failed nil nil nil datum arguments)))

(define-compiler-macro check (&whole whole &environment env &rest ignore)
  (declare (ignore ignore))
  (%make-check-form whole env))

;;; EXPECT

(define-condition missed (warning check) ()
  (:documentation "A type of WARNING used for missed EXPECT conditions."))

(declaim (ftype (function (missed) (values null &optional)) missed))
(defun missed (missed)
  "Handles a MISSED expectation."
  (warn missed)
  (when *on-missed-expectation*
    (funcall (the function *on-missed-expectation*) missed))
  nil)

(declaim (ftype (function (missed &rest t) (values null &optional)) missed*))
(defun missed* (missed &rest args)
  "Handles a MISSED expectation. ARGS are used to fill the values for MISSED."
  (missed (fill-values missed args)))

(defgeneric expect (result &optional datum &rest arguments)
  (:documentation
   "Returns RESULT if non-NIL. Otherwise signals a warning.

Note: this is not a pure generic-function. It may be compiler-optimized.

DATUM can be a symbol or string. In that case the ARGUMENTS
are used together with DATUM to create the condition.
If DATUM is a string, it is used as a format control string.
If DATUM is a symbol, it is used as the first argument to MAKE-CONDITION."))

(defmethod expect (result &optional datum &rest arguments)
  (declare (ignore datum arguments))
  result)

(defmethod expect ((result null) &optional datum &rest arguments)
  (declare (ignore result))
  (missed
   (apply #'coerce-to-condition 'missed nil nil nil datum arguments)))

(define-compiler-macro expect (&whole whole &environment env &rest ignore)
  (declare (ignore ignore))
  (%make-check-form whole env))

;;; DCHECK macro

(defmacro dcheck (test-form &optional datum &rest arguments)
  "Specialized form of ASSERT and CHECK.

Compared to CHECK, this code is only inserted in unoptimized code.
DCHECK also does not return any VALUES.
DCHECK will signal an ASSERTION-ERROR in case the TEST-FORM evaluates to NIL.
The use case for DCHECK is in non-interactive production code for tests
that are very expensive and should only run in a debug build.

Parameters:
 TEST-FORM - is evaluated and a NIL value will trigger an error.
 DATUM - a specification of the error to be thrown in case the assertion fails.
 ARGUMENTS - are the arguments to the DATUM specification.

See: CL:ASSERT, CHECK, EXPECT"
  (declare (ignorable test-form datum arguments))
  (if (find :opt *features* :test 'eq)
      `(values)
      `(without-code-deletion-notes
         (check ,test-form ,@(when datum `(,datum)) ,@arguments)
         (values))))

;;; CHECK helpers

;; Needs to be inline for the compiler to derive further actions.

(defgeneric alternate-truth-form (from)
  (:documentation
   "Augment the FROM used a check predicate.
This is useful for injecting code into every check and used in tests."))

(defmethod alternate-truth-form (form) form)

;; TODO(czak): The cached-formatter should be put in ace.core.macro.
(defvar *formatter-cache* (make-hash-table :test #'equal)
  "Saves cached formatters.")

(defmacro cached-formatter (fctrl)
  "Returns a FORMATTER for the FORMAT control string FCTRL."
  (declare (string fctrl))
  `(or (gethash ,fctrl *formatter-cache*)
       (setf (gethash ,fctrl *formatter-cache*)
             (formatter ,fctrl))))

(defun %make-check-form (whole env)
  ;; Generate a simple check form that used DATUM and ARGUMENT to
  ;; provide a warning or an error message to the user.
  ;;
  ;; Example forms generated:
  ;;
  ;;  (or (eq foo :foo)
  ;;      (error (coerce-to-condition 'failed "foo.lisp" 42 "CHECK failed!")))
  ;;
  ;;  (let ((#:bar-23 bar))
  ;;    (or (x:= #:bar-23 10)
  ;;        (missed (%make-condition
  ;;                 'missed "bar.lisp" 172 "Expected that ~S" '(x:= bar 10))))
  ;;
  ;; Parameters
  ;;  WHOLE - whole check form.
  ;;  ENV - lexical environment.
  (let* ((action
          (ecase (first whole)
            ((check) 'failed)
            ((expect) 'missed)))
         (action*
          (ecase (first whole)
            ((check) 'failed*)
            ((expect) 'missed*)))
         (test-form (second whole))
         (file (current-file-namestring))
         (line (line-and-column-numbers whole))
         (prefix (ecase action
                   ((failed) "Failed check~:_ ~S")
                   ((missed) "Expected~:_ ~S")))
         (op (and (consp test-form) (first test-form))))
    (cond
      ((third whole) ; datum
       `(or ,(alternate-truth-form test-form)
            (,action
             (coerce-to-condition
              ',action ,file ,line ,*package* ,@(cddr whole)))))
      ((and (member op '(null not))
            (consp (cadr test-form))
            (eq (function-information (caadr test-form) env) :function))
       ;; Construct a message using the NOT form.
       ;; Also extract the bindings for the inside form if any.
       (multiple-value-bind (%inside-form inside-bindings inside-formatters)
           (%get-formatters (second test-form) env)
         (multiple-value-bind (%test-form not-bindings not-formatters)
             (%get-formatters `(,op ,%inside-form) env)
           `(let ,inside-bindings
              (let ,not-bindings
                (or ,(alternate-truth-form %test-form)
                    (,(if (or inside-bindings not-bindings) action* action)
                     (load-time-value
                      (make-condition
                       ',action
                       ,@(when file `(:file ,file))
                       ,@(when line `(:line ,line))
                       :package ,*package*
                       :format-control
                       (cached-formatter
                        ,(%make-format-control
                          prefix (append inside-formatters not-formatters)))
                       :format-arguments
                       '(,test-form
                         ,@(loop :for (%var form) :in inside-bindings
                                 :append `(,form nil))
                         ,@(and not-bindings
                            `(,(second test-form) nil)))))
                     ,@(mapcar #'car inside-bindings)
                     ,@(and not-bindings `(,(caar not-bindings))))))))))
      ((eq op 'and)
       (multiple-value-bind (%test-form bindings)
           (%get-formatters test-form env)
         (declare (list bindings))
         (let* ((%args (gensym* :args))
                (%test-args
                 (loop
                   :for arg :in (rest test-form)
                   :for %arg :in (rest %test-form)
                   :for %index
                     = (unless (eq %arg arg)
                         (position %arg bindings :key #'car :test #'eq))
                   :collect
                   (if %index
                       `(or (setf (nth ,%index ,%args) ,arg)
                            (setf (cdr (nthcdr ,%index ,%args)) nil))
                       arg)))
                (fctrl (strcat prefix "~@[~:_ with~@{~_ ~S = ~S~^,~}~].")))
           `(let ((,%args (make-list ,(length bindings))))
              (declare (dynamic-extent ,%args))
              (or ,(alternate-truth-form `(,op ,@%test-args))
                  (apply
                   #',action*
                   (load-time-value
                    (make-condition
                     ',action
                     ,@(when file `(:file ,file))
                     ,@(when line `(:line ,line))
                     :package ,*package*
                     :format-control
                     (cached-formatter ,fctrl)
                     :format-arguments
                     '(,test-form ,@(loop :for (%var form) :in bindings
                                          :append `(,form nil)))))
                   ,%args))))))
      ((and op (eq (function-information op env) :function))
       ;; Construct a message using TEST-FORM.
       (multiple-value-bind (%test-form bindings formatters)
           (%get-formatters test-form env)
         `(let ,bindings
            (or ,(alternate-truth-form %test-form)
                (,(if bindings action* action)
                 (load-time-value
                  (make-condition
                   ',action
                   ,@(when file `(:file ,file))
                   ,@(when line `(:line ,line))
                   :package ,*package*
                   :format-control
                   (cached-formatter ,(%make-format-control prefix formatters))
                   :format-arguments
                   '(,test-form ,@(loop :for (%var form) :in bindings
                                        :append `(,form nil)))))
                 ,@(mapcar #'car bindings))))))
      (t ; atom or special form.
       `(or ,(alternate-truth-form test-form)
            (,action
             (load-time-value
              (make-condition
               ',action
               ,@(when file `(:file ,file))
               ,@(when line `(:line ,line))
               :package ,*package*
               :format-control (cached-formatter ,(strcat prefix "."))
               :format-arguments '(,test-form)))))))))

(defun coerce-to-condition (default file line package datum &rest arguments)
  "Coerces the DATUM specification and its ARGUMENTS to a condition.

If DATUM is a string, it is used as a format control for the ARGUMENTS.
If DATUM is a symbol, it is used with ARGUMENTS when calling MAKE-CONDITION.

FILE and LINE are passed to the constructor of the DEFAULT condition.

Parameters:
 DEFAULT - the default type of the condition.
 FILE, LINE - the location from where the condition is signaled.
 PACKAGE - the package in which the condition was defined (for printing).
 DATUM - a symbol, string, function, or condition - specifies the condition.
 ARGUMENTS - arguments for the DATUM.
"
  (etypecase datum
    (null
     (make-condition
      default :format-control
              (cond ((subtypep default 'failed) "Failed CHECK.")
                    ((subtypep default 'missed) "Missed EXPECT condition."))
              :file file :line line :package package))
    (symbol (apply #'make-condition datum arguments))
    ((or string function)
     (make-condition
      default :format-control datum
              :format-arguments arguments
              :file file :line line :package package))
    (condition datum)))

(defun freep (form env)
  "True if the FORM is side-effect free in the lexical environment ENV."
  (or (constantp form env)
      (typep form '(cons (member function lambda)) env)))

;; TODO(czak): Extract the printer generator out of here.
;; Generating a print-form for a function/macro call with pseudo-types
;; is a different aspect and can be moved out.
(defun %get-formatters (form env)
  "Returns a list of format control directives for the FORM and
the corresponding sub-forms.

Parameters:
 FORM - the form for which we derive the format directives.
 ENV - the lexical environment.
"
  (let (arg-types alt-form bindings formatters)
    (push (first form) alt-form)
    (flet ((add-binding (arg formatter &aux %prev)
             (cond ((and
                     (symbolp arg)
                     (not (eq (variable-information arg env) :symbol-macro))
                     (setf %prev (car (find arg bindings :key #'second))))
                    ;; Deduplicate variables from the display.
                    (push %prev alt-form))
                   (t
                    (let ((%var (gensym* arg)))
                      (push formatter formatters)
                      (push `(,%var ,arg) bindings)
                      (push %var alt-form))))))
      (multiple-value-bind (type local) (function-information (first form) env)
        (cond
          ((and (eq type :function) (not local)
                ;; Use debug info to derive argument types.
                (setf arg-types (function-form-argument-types form)))
           (loop
             :for arg :in (rest form)
             :for type :in arg-types
             :unless (freep arg env) :do
               (let* ((type-info
                       (and (typep type '(and symbol (not boolean)))
                            (get type 'ace.core.type:info)))
                      (formatter
                       (and type-info (info-format type-info))))
                 (add-binding
                  arg (etypecase formatter
                        (null "~S")
                        (string formatter)
                        (symbol (format nil "~~/~S/" formatter)))))
             :else :do
               (push arg alt-form)))
          (t
           (loop
             :for arg :in (rest form)
             :unless (freep arg env) :do
               (add-binding arg "~S")
             :else :do
               (push arg alt-form))))))
    (values (nreverse alt-form) (nreverse bindings) (nreverse formatters))))

(defun %make-format-control (prefix formatters)
  "Makes a format control string for a failure condition.

Parameters:
 PREFIX - contains the prefix.
 FORMATTERS - a list of FORMAT control directives for each binding.
"
  (format nil "~A~@[~~:_ with~{~~_ ~~S = ~A~^,~}~]." prefix formatters))
