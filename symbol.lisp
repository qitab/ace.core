;;; Copyright 2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

;;; Function helping with creating symbols and keywords.
;;; The symbols in this package are designed to be referenced with the package prefix.
;;; Use ACE.CORE namespace for simple syntax
;;;

;;; TODO(czak): Unify the libraries across google3/lisp and travel/qpx.

(defpackage #:ace.core.symbol
  (:use #:cl #:ace.core)
  (:shadow #:format)
  (:export
   #:cat
   #:cat!
   #:cat*
   #:cat$
   #:format
   #:format!
   #:format*))

(in-package #:ace.core.symbol)

(deftype package-designator ()
  "A type of package designator used in SYMBOL:FORMAT."
  `(or symbol string character package))

(deftype string-designator ()
  "A type of string designator used in CL:STRING."
  `(or symbol string character))

(declaim (ftype (function (package-designator list) (values symbol &optional)) %cat))
(defun %cat (package args)
  (let ((name (apply #'cl:concatenate 'string (mapcar #'string args))))
    (if package
        (values (intern name (if (eq package t) *package* package)))
        (make-symbol name))))

(declaim
 (ftype (function (package-designator &rest string-designator) (values symbol &optional)) cat))
(defun cat (package &rest args)
  "Makes a symbol and interns it into the PACKAGE.
 The symbol is built by concatenating the ARGS.
 Then the resulting string is interned into the PACKAGE specified.
 PACKAGE can be T - in that case the package used is the current package (*PACKAGE*).
 PACKAGE can be NIL - in that case the function returns an uninterned symbol.
 This function should only be used at macroexpansion-/compile-time.

 Arguments:
  PACKAGE - the package for the symbol to be interned within.
  ARGS - string designators used to build the symbol.
 Examples:
  (let ((*package* (find-package \"TEST\")))
     (symbol:cat nil :foo- 'string:to-keyword \"*\") => #:FOO-TO-KEYWORD*
     (symbol:cat t :foo- 'string:to-keyword \"*\")) => TEST:FOO-TO-KEYWORD*
 Related:
  symbol:cat!
  symbol:cat*
  qpx:symbolize
  qpx:fsymbolize"
  (%cat package args))

(declaim (ftype (function (&rest string-designator) (values symbol &optional)) cat!))
(defun cat! (&rest args)
  "Makes a symbol and interns it into the current package.
 The symbol is built by concatenating the ARGS.
 Then the resulting string is interned into the current package.
 This function should only be used at macroexpansion-/compile-time.
 This is a shortcut for SYMBOL:CAT.

 Arguments:
  ARGS - string designators used to build the symbol.
 Examples:
  (let ((*package* (find-package \"TEST\")))
     (symbol:cat! :foo- 'string:to-keyword \"*\")) => TEST:FOO-TO-KEYWORD*
 Related:
  symbol:cat
  symbol:cat*
  alexandria:symbolicate
  qpx:symbolize
  qpx:fsymbolize"
  (%cat t args))

(declaim (ftype (function (&rest string-designator) (values symbol &optional)) cat*))
(defun cat* (&rest args)
  "Makes a new symbol and interns it into the package of the first symbol from ARGS.
 The new symbol is built by concatenating the ARGS.
 Then the full string is interned into the package of the first SYMBOL argument.
 This is a shortcut for SYMBOL:CAT.

 Arguments:
  ARGS - string designators used to build the symbol.
 Example:
  (symbol:cat* #\% 'my-function 'internal) => %MY-FUNCTION-INTERNAL
  (symbol:cat* :foo 'my-function 'internal) => :FOO-MY-FUNCTION-INTERNAL
 Related:
  symbol:cat
  symbol:cat!"
  (let ((symbol (find-if #'symbolp args)))
    (assert symbol () "At least one symbol required as argument.") ; NOLINT
    (%cat (symbol-package symbol) args)))

(declaim (ftype (function (&rest string-designator) (values symbol &optional)) cat$))
(defun cat$ (&rest args)
  "Makes a new symbol and interns it into the package of the first non-keyword symbol from ARGS.
 The new symbol is built by concatenating the ARGS.
 Then the full string is interned into the package of the first non-keyword SYMBOL argument.
 This is a shortcut for SYMBOL:CAT.

 Arguments:
  ARGS - string designators used to build the symbol.
 Example:
  (symbol:cat$ :foo 'my-function 'internal) => FOO-MY-FUNCTION-INTERNAL
 Related:
  symbol:cat
  symbol:cat!"
  (let ((symbol (find-if (lambda (a) (and (symbolp a) (not (keywordp a)))) args)))
    (assert symbol () "At least one non-keyword symbol required as argument.") ; NOLINT
    (%cat (symbol-package symbol) args)))

;;; TODO(czak): The symbol format stuff is deprecated.

(declaim (ftype (function (package-designator string list) (values symbol &optional)) %format))
(defun %format (package control args)
  (let ((name (apply #'cl:format nil control args)))
    (if package
        (values (intern name (if (eq package t) *package* package)))
        (make-symbol name))))

(declaim (ftype (function (package-designator string &rest t) (values symbol &optional)) format))
(defun format (package control &rest args)
  "Makes a symbol and interns it into the PACKAGE.
 The symbol is built by applying the format CONTROL string to the rest of the arguments.
 Then the resulting string is interned into the PACKAGE specified.
 PACKAGE can be T - in that case the package used is the current package (*PACKAGE*).
 PACKAGE can be NIL - in that case the function returns an uninterned symbol.
 This function should only be used at macroexpansion-/compile-time.

 Arguments:
  CONTROL - format control.
  ARGS - arguments for the control string.
 Examples:
  (let ((*package* (find-package \"TEST\")))
     (symbol:format t \"%~A\" 'string:to-keyword)) => TEST:%TO-KEYWORD
 Related:
  symbol:format!
  symbol:format*
  alexandria:format-symbol
  alexandria:symbolicate
  qpx:symbolize
  qpx:fsymbolize
  sb-int:symbolicate"
  (%format package control args))

(declaim (ftype (function (string &rest t) (values symbol &optional)) format!))
(defun format! (control &rest args)
  "Makes a symbol and interns it into the current package.
 The symbol is built by applying the format CONTROL string to the rest of the arguments.
 Then the resulting string is interned into the current package.
 This function should only be used at macroexpansion-/compile-time.
 This is a shortcut for SYMBOL:FORMAT.

 Arguments:
  CONTROL - format control.
  ARGS - arguments for the control string.
 Examples:
  (let ((*package* (find-package \"TEST\")))
     (symbol:format! \"%~A\" 'string:to-keyword)) => TEST:%TO-KEYWORD
 Related:
  symbol:format
  symbol:format*
  alexandria:format-symbol
  alexandria:symbolicate
  qpx:symbolize
  qpx:fsymbolize
  sb-int:symbolicate"
  (%format t control args))

(declaim (ftype (function (string &rest t) (values symbol &optional)) format*))
(defun format* (control &rest args)
  "Makes a new symbol and interns it into the package of SYMBOL specified as the second argument.
 The new symbol is built by applying the CONTROL format string to the arguments inclusive SYMBOL.
 Then the full string is interned into the package of the SYMBOL argument.
 This is a shortcut for SYMBOL:FORMAT.

 Arguments:
  CONTROL - format control.
  ARGS    - arguments for the control string.
 Example:
  (symbol:format* \"%~A-~A\" 'my-function 'internal) => %MY-FUNCTION-INTERNAL
 Related:
  symbol:format
  symbol:format!
  alexandria:format-symbol"
  (%format (symbol-package (first args)) control args))
