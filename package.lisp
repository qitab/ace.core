;;; Copyright 2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

;;;;
;;;; Provides utilities related to the package system.
;;;; Notably, this file is loaded by default with ace.core as
;;;; it provides the defpackage* and local package aliases.
;;;;

(defpackage #:ace.core.package
  (:use #:cl #:ace.core)
  #-sbcl
  (:import-from #:trivial-garbage
                #:make-weak-hash-table)
  #-sbcl
  (:import-from #:ace.core.thread
                #:make-mutex
                #:with-mutex)
  (:export #:defpackage*
           #:define-summary-package
           #:aliases
           #:add-alias
           #:add-namespace))

(in-package #:ace.core.package)

(deftype string-designator ()
  "The type of a string designator: SYMBOL, STRING, or CHARACTER."
  '(or symbol string character))

(deftype package-designator ()
  "The type of a package designator: PACKAGE, SYMBOL, STRING, or CHARACTER."
  '(or package symbol string character))

(deftype find-package-function ()
  "The type of the FIND-PACKAGE function."
  '(function (package-designator) (values (or package null) &optional)))

;;;
;;; The implementation idea of local package nicknames is based on simply
;;; overriding the find-package function the is used by the reader.
;;; Package aliases can be added using ADD-ALIAS function.
;;; The DEFPACKAGE* macro, exported from CL, allows to define the
;;; local aliases in a CL:DEFPACKAGE way.
;;;

(declaim (type find-package-function +builtin-find-package-function+))
(defconstant +builtin-find-package-function+
  (if (boundp '+builtin-find-package-function+)
      (symbol-value '+builtin-find-package-function+)
      #'common-lisp:find-package)
  "The holder for the original FIND-PACKAGE function.")

(declaim (ftype find-package-function builtin-find-package) (inline builtin-find-package))
(defun builtin-find-package (designator)
  "Calls the builtin FIND-PACKAGE function with DESIGNATOR as the argument."
  (funcall +builtin-find-package-function+ designator))

#-sbcl
(progn
  ;; Alias table of the package.
  (defconstant +lock-table+ (or #-(or sbcl ccl) t)
    "Indicates that the table are not synchronized and should be locked.")

  (defvar *alias-table*
    (apply #'make-weak-hash-table :test #'eq :weakness :key
           #+sbcl '(:synchronized t)
           #+ccl  '(:shared :lock-free))
    "A hash map to an alist with package local aliases.")

  (defvar *alias-mutex* (make-mutex "PACKAGE::*ALIAS-TABLE*")
    "A mutex for the package ALIAS-TABLE.")

  ;; TODO(czak): Maybe make those errors public.

  (define-condition alias-error (error)
    ((alias :initarg :alias :reader alias-error-alias)
     ;; The scope in which the alias is placed.
     (scope :initarg :package :reader alias-error-scope)
     ;; The package to which the alias points to.
     (package :initarg :package :initform nil :reader alias-error-package))
    (:report
     (lambda (error out) (declare (optimize (speed 0)))
             (format out "The alias ~S for ~S in ~S causes an error"
                     (alias-error-alias error)
                     (package-name (alias-error-package error))
                     (package-name (alias-error-scope error)))))
    (:documentation "An error for signaled with respect to local package aliases."))

  (define-condition alias-missing-error (alias-error) ()
    (:report
     (lambda (error out) (declare (optimize (speed 0)))
             (format out "The package ~S has also following package aliases:~%~3T~S"
                     (package-name (alias-error-scope error)) (alias-error-alias error))))
    (:documentation "An error for signaled with respect to local package aliases."))

  (define-condition alias-conflict-error (alias-error)
    ((new-package :initarg :new :reader alias-conflict-error-new-package))
    (:report
     (lambda (error out) (declare (optimize (speed 0)))
             (format out "The new alias ~S for ~S in ~S conflicts with an existing one for ~S"
                     (alias-error-alias error)
                     (package-name (alias-conflict-error-new-package error))
                     (package-name (alias-error-scope error))
                     (package-name (alias-error-package error)))))
    (:documentation "An error signaled when a used alias is created for a new package."))

  (defun %find-package (designator)
    "Internal reimplementation of the FIND-PACKAGE function.
 Takes a DESIGNATOR of type PACKAGE, SYMBOL, STRING, or CHARACTER.
 Returns the package by local alias in the SCOPE or global name.
 Returns NIL if package was not found."
    (let ((scope (and (boundp '*package*) *package*))
          (name (if (typep designator 'package)
                    (return-from %find-package designator)
                    (string designator))))
      (or (and scope (plusp (length name))
               (cdr (assoc name (aliases scope) :test #'string=)))
          (builtin-find-package name)))))

(declaim (ftype (function (&optional package-designator) (values list &optional)) aliases))
(defun aliases (&optional (scope *package*))
  "Return the package aliases for the SCOPE package designator."
  (let ((scope (find-package scope)))
    #+sbcl (sb-ext:package-local-nicknames scope)
    #-sbcl
    (with-mutex (*alias-mutex* :lockp +lock-tables+)
      (values (gethash scope *alias-table*)))))

(declaim (ftype (function (string-designator package-designator &optional package-designator)
                          (values (or null package) &optional))))
(defun add-alias (alias package &optional (scope *package*))
  "Adds an ALIAS for PACKAGE in the SCOPE package.
 ALIAS is a string designator that can be used as package prefix in SCOPE package.
 The PACKAGE, and SCOPE can be package designators.
 The SCOPE and PACKAGE designators are resolved in the global package namespace.
 It is an error to add an existing ALIAS to a different package.
 It is an error if SCOPE or PACKAGE do not exist."
  #+sbcl
  (sb-ext:add-package-local-nickname alias package scope)
  #-sbcl
  (let* ((scope-designator scope)
         (scope (builtin-find-package scope))
         (local-aliases (and scope (aliases scope)))
         (package-designator package)
         (package (builtin-find-package package))
         (alias (string alias))
         (cell (assoc alias local-aliases :test #'string=)))
    (unless scope
      (error "Cannot add a package alias to ~S. Package not found." scope-designator))
    (unless package
      (error "Cannot alias ~S as ~S. Package not found." package-designator alias))
    (if cell
        (if (string= (package-name package) (package-name (cdr cell)))
            ;; Replace the package definition.
            (setf (cdr cell) package)
            ;; else - signal an alias-conflict-error.
            (restart-case
                (error 'alias-conflict-error
                       :alias alias :package (cdr cell) :scope scope :new package)
              (continue ()
                :report "Overwrite the old alias and point it to the new package."
                (setf (cdr cell) package))
              (cancel ()
                :report "Cancel the operation and keep the old alias."
                (return-from add-alias))))
        (with-mutex (*alias-mutex*)
          (push (cons alias package) (gethash scope *alias-table*))))
    package))

(declaim (ftype (function (package-designator &optional package-designator) (values))
                add-namespace))
(defun add-namespace (namespace &optional (scope *package*))
  "Adds all packages prefixed with '<NAMESPACE>.' to the SCOPE package as aliases."
  (let* ((scope (builtin-find-package scope))
         (prefix (format nil "~A." (package-name namespace)))
         (prefix-length (length prefix)))
    (dolist (package (list-all-packages))
      (let ((package-name (package-name package)))
        (when (and (> (length package-name) prefix-length)
                   (string= prefix package-name :end2 prefix-length))
          (add-alias (subseq package-name prefix-length) package scope))))))

(defmacro defpackage* (package &rest options)
  "Reimplementation of the CL:DEFPACKAGE macro with addition of
 the :USE-ALIAS and :USE-NAMESPACE.

 The :USE-ALIAS section has the following syntax:
   (:use-alias <package-designator> ...)
   or
   (:use-alias (<alias-designator> <package-designator>) ...)
 The first form defines an alias for a package by taking the last part of package name after the
 dot (#\.) as its local alias name.
 The second form uses the alias-designator to define a local alias for the package referenced by the
 following package-designator.

 The :USE-NAMESPACE section has the following syntax:
   (:use-namespace <namespace-string-designator>*)
 It has two effects:
 1. Enumerates all the packages with the namespace dot prefix and adds the shortened aliases.
 2. :USE-NAMESPACE has also the effect of :USE <namespace-string-designator>.

 The PACKAGE is a package designator and rest of OPTIONS are forwarded to CL:DEFPACKAGE."
  (check-type package string-designator)
  (let* ((package (string package))
         (standard-options
          (remove-if (lambda (x) (member (first x) '(:use-alias :use-namespace))) options))
         (aliases (mapcan #'rest (remove :USE-ALIAS options :key #'first :test-not #'eq)))
         (namespaces (mapcan #'rest (remove :USE-NAMESPACE options :key #'first :test-not #'eq))))

    (setf aliases
          (loop for alias in aliases
                collect (cond ((consp alias) alias)
                              ((typep alias 'string-designator)
                               ;; Take the part of the package after the last dot to be an alias.
                               (let* ((name (string alias))
                                      (dot (position #\. name :from-end t))
                                      (shortcut (and dot (subseq name (1+ dot)))))
                                 (unless dot
                                   (error "Cannot make a package name alias out of ~S." name))
                                 (list shortcut alias)))
                              (t
                               (error "Cannot make a package name alias out of ~S." alias)))))

    (loop for (aliased alias) in aliases do
         (check-type aliased string-designator)
         (check-type alias string-designator))

    (loop for namespace in namespaces do
         (check-type namespace string-designator))

    (setf namespaces (mapcar #'string namespaces))

    (push `(:use ,@(remove package namespaces :test #'string=)) standard-options)

    `(progn
       (defpackage ,package ,@standard-options)
       (eval-always
        ,@(loop for namespace in namespaces
             collect `(add-namespace ',namespace ',package))
        ,@(loop for (alias aliased) in aliases
             collect `(add-alias ',(string alias) ',(string aliased) ',package)))

       ;; TODO(czak): Maybe warn about additional packages.

       ;; Return the package from the form.
       (find-package ,(string package)))))

(defmacro define-summary-package (package &rest options)
    "Defines a summary PACKAGE that exports all external symbols from :USE-REEXPORT
 and all symbols specified by the :SYMBOLS option.
 OPTIONS contain other options to CL:DEFPACKAGE."
  (let ((package-designator (string package))
        (subpackages (mapcar #'string (rest (find :USE-REEXPORT options :test #'eq :key #'first))))
        (symbols (rest (find :SYMBOLS options :test #'eq :key #'first))))
    `(eval-always
      (let ((summary-package (or (find-package ,package-designator)
                                 (make-package ,package-designator))))
        (dolist (subpackage ',subpackages)
          (unless (find-package subpackage)
            (error "Cannot reexport the symbols of ~S. Package does not exist." subpackage))
          (do-external-symbols (symbol (find-package subpackage))
            (import symbol summary-package)
            (export symbol summary-package)))
        (dolist (symbol ',symbols)
          (import symbol summary-package)
          (export symbol summary-package))))))

;;;
;;; Patching the Lisp implementation.
;;;

(eval-always
 #-sbcl
 (handler-bind ((warning #'muffle-warning)
                (condition #'continue))
   ;; This seems like a generic way to support it on most Lisp implementations.
   ;; Some Lisps like CMUCL need to override an internal function.
   ;; Some Lisps like CLISP need to patch C sources.
   (compile '%find-package)
   (setf (symbol-function 'cl:find-package) #'%find-package)))
