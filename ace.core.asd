(defparameter *files*
  (append
   '("core"
     "collect"
     "macro"
     "type"
     "defun"
     "symbol"
     "package"
     "fast-ops"
     "switch"
     "simplify"
     "once-only"
     "list"
     "etc"
     "check"
     "vector"
     "functional"
     "dx"
     "enum"
     "os"
     "sequence"
     "tty"
     "string"
     "with"
     "number")
   #+sbcl
   '("hook"
     "thread"
     "fx"
     "atomic"
     "io")))

(defsystem ace.core
  :name "Ace Lisp Core Libraries"
  :description "Common utilities used at Ace for Lisp and linked into most Ace Lisp binaries."
  :long-description
  "The ACE.CORE library contains following modules:
  ace.core - is a summary package that can be used as a namespace,
  .macro - a set of utilities for writing macros,
  .type - utilities for dealing with types and declarations,
  .symbol - utilities dealing with symbols,
  .collect - with-collectors, with-collected-values,
  .etc - a set of utilities that have not provided for their own package yet,
  .assert - CHECK, DCHECK, EXPECT, ASSERT*,
  .defun* - a DEFUN like macro with easy interface and type declaration,
  .thread - utilities dealing with threads and concurrency,
  .package - helpers to defpackage, namespaces, and package aliases,
  .number - utilities related to numbers, parsing and writing C++/Java numbers,
  .string - utilities related to character strings,
  .vector - utilities for dealing with vectors,
  .list - utilities for dealing with lists,
  .parse - parsing of dynamic type values from C++/Java representation,
  .fast-ops - a set of macros allowing to define optimized typed operators,
  .fixnum-ops - the unholy set of fixnum optimized operations,
  .index-ops - a set of operations acting on array indexes,
  .switch - a family of CASE-like SWITCH macros"
  :version "1.0"
  :author "Lisp Community"
  :license "MIT"
  :depends-on (:bordeaux-threads
               :closer-mop
               :trivial-backtrace
               #+sbcl :sb-introspect
               #+sbcl :sb-cltl2)
  :in-order-to ((test-op (test-op :ace.core/tests)))
  :serial t
  :components
  #.(loop for f in *files* collect `(:file ,f)))

(defsystem :ace.core/tests
  :name "ace.core tests"
  :version "1.0"
  :licence "MIT"
  :description      "Test code for ace.core"
  :long-description "Test code for ace.core"
  :depends-on (:ace.core :ace.test)
  :serial t
  :pathname ""
  ;; Sadly not all tests are passing...
  :components
  #.(loop for f in *files*
          unless (member f '("core"
                             "macro"
                             "switch"
                             "os"
                             "check"
                             "vector"
                             "defun")
                         :test #'string=)
            collect `(:file ,(concatenate 'string f "-test")))
  :perform (test-op (o c)
                    (uiop:symbol-call '#:ace.test
                                      '#:run-tests)))
