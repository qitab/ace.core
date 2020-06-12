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
  (:import-from
   #+sbcl      #:sb-ext
   #+ccl       #:ccl
   #+ecl       #:ext
   #+abcl      #:ext
   #+clasp     #:ext
   #+lispworks #:hcl
   #+allegro   #:excl
   #:package-local-nicknames
   #:add-package-local-nickname)
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

(declaim (ftype (function (&optional package-designator) (values list &optional)) aliases))
(defun aliases (&optional (scope *package*))
  "Return the package aliases for the SCOPE package designator."
  (let ((scope (find-package scope)))
    (package-local-nicknames scope)))

(declaim (ftype (function (string-designator package-designator &optional package-designator)
                          (values (or null package) &optional))))
(defun add-alias (alias package &optional (scope *package*))
  "Adds an ALIAS for PACKAGE in the SCOPE package.
 ALIAS is a string designator that can be used as package prefix in SCOPE package.
 The PACKAGE, and SCOPE can be package designators.
 The SCOPE and PACKAGE designators are resolved in the global package namespace.
 It is an error to add an existing ALIAS to a different package.
 It is an error if SCOPE or PACKAGE do not exist."
  (add-package-local-nickname alias package scope))

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
