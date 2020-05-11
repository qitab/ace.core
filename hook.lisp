;;; The module provides CLOS based functionality useful to define hooks.
;;; The DEFINE-HOOK-FUNCTION is a macro that defines a generic-function
;;; which uses the HOOK:COMBINATION of methods that is different from the
;;; standard combination. The main difference is that it allows to specify
;;; a hook "identifier" and thus allows to chain multiple methods with the
;;; same specializers, which is not possible with the standard combination.
;;; Another feature is that the HOOK:COMBINATION allows to specify
;;; SEQUENCE numbers, or :AFTER and :BEFORE qualifiers useful in ordering
;;; the methods in a hook function invocation.
;;; For more information on combination methods please refer to:
;;; http://www.lispworks.com/documentation/lw70/CLHS/Body/m_defi_4.htm
;;;

(ace.core:defpackage* #:ace.core.hook
  (:use #:cl #:ace.core)
  (:use-alias (#:mop #:closer-mop)
              #:ace.core.list)
  (:import-from #:ace.core.list #:unionf #:nreversef)
  (:export
   #:define-hook-function
   #:define-hook-method
   #:abort
   #:clear
   #:at-restart
   #:at-exit
   #:at-image-save
   #:after-gc))

(in-package #:ace.core.hook)

(deftype around () '(or list (member t)))

(defclass hook-method (standard-method)
  ((identifier :type symbol :accessor method-identifier)
   (number     :type number :accessor method-number :initform 0)
   (after      :type list   :accessor method-after  :initform nil)
   (before     :type list   :accessor method-before :initform nil)
   (around     :type around :accessor method-around :initform nil))
  (:documentation
   "A hook method metaclass with special properties.
 The properties are parsed and added by ADD-METHOD."))

(defmacro define-hook-function (hook-function args &key
                                                   (operator 'progn)
                                                   (order :most-specific-first)
                                                   documentation
                                                   default-method)
  "Defines a generic function under the name HOOK-FUNCTION.
 The generic function uses HOOK:COMBINATION and accepts HOOK:METHODS.
 ARGS - are the arguments for the hook function to be passed to the methods.
 OPERATOR - is the operator used to combine the methods which is by default PROGN.
 ORDER - allows to sort the hook methods and defaults to :MOST-SPECIFIC-FIRST.
 DOCUMENTATION - adds a documentation string to the generic function.
 DEFAULT-METHOD - if non-nil, will add a default method with the specified body.

 The hook methods are combined using the HOOK:COMBINATION as follows.
 Each of the hook methods require a specific method qualifier pattern:
   symbol number
 or
   symbol [:after qualifier*] [:before qualifier*]
 or
   symbol :around qualifier*

 Each pattern starts with a symbol that is the hook method identifier.

 The first pattern adds a primary method to the hook function.
 Hook methods with a number as the second qualifier are ordered according to
 their number with 0 being the default for other methods.

 The second pattern form also adds a primary method to the hook function.
 If the symbol identifier is followed by an :AFTER or :BEFORE keyword,
 the method invocation is ordered after or before the methods named
 by the rest of the symbols following :AFTER or :BEFORE.
 Since, methods with :AFTER or :BEFORE qualifiers have a sequence number of 0,
 they are ordered after all methods with negative sequence numbers and before
 all methods with positive sequence numbers.

 The third pattern using an :around qualifier will insert the method
 around the invocation of all the methods named by the rest of symbols
 following the :AROUND keyword.
 The around methods have a CALL-NEXT-METHOD function defined.

 Example:
  (define-hook-function at-query-start (query)
   :documentation \"Methods called when a QUERY is about to be processed.\")

  (defmethod at-query-start check-parameters (query)
    \"The method is called at query start time to check parameters of the QUERY.\"
    ...)

 Example:
  This example uses the HOOK:AT-RESTART hook function.
  (defmethod hook:at-restart init-foo () ...)
  (defmethod hook:at-restart write-restart :before init-foo () (write-string \"RESTART\"))
  (defmethod hook:at-restart log-init-foo :around init-foo ()
     (write-string \"Before INIT-FOO\")
     (call-next-method)
     (write-string \"After INIT-FOO\"))

  This defines three hook methods called after the image was restarted.
  The first one (init-foo) initializes some functionality foo.
  The second (write-restart) is called before INIT-FOO and writes RESTART to the standard output.
  The third (log-init-foo) is an around method for the INIT-FOO hook method.

  Calling (hook:at-restart) would invoke all the three hook methods."
  (check-type hook-function symbol)
  (check-type args list)
  (check-type operator symbol)
  (check-type order (member :most-specific-first :most-specific-last))
  (check-type documentation (or null string))
  `(defgeneric ,hook-function ,args
     ,@(when documentation `((:documentation ,documentation)))
     (:method-combination combination :operator ,operator :order ,order)
     (:method-class hook-method)
     ,@(when default-method
         `((:method default ,args ,default-method)))))

(defmacro define-hook-method (hook-function identifier &rest rest)
  "Defines a HOOK:METHOD for the HOOK-FUNCTION generic function with the given identifier.
 The REST arguments have a specific syntax depicted below:
 (define-hook-method HOOK-FUNCTION IDENTIFIER
     [<sequence-number>]
     [:after <identifier>*]
     [:before <identifier>*]
     [:around [<identifier>*]]
     LAMBDA-LIST
     BODY)
 Arguments:
  HOOK-FUNCTION - is the name of the hook generic function.
  IDENTIFIER - is the identifier symbol of this hook method.
  SEQUENCE-NUMBER - is a sequence number allowing to sort the methods.
  AFTER - is an identifier or list of identifiers for methods after which this one will be run.
  BEFORE - is an identifier or a list of identifiers for methods after which this one will be run.
  AROUND - is an identifier or list of identifiers for methods this one will wrap around.
    If the list is empty the method wraps around the whole hook point.
  LAMBDA-LIST - is the lambda list of arguments for the hook method.

 Note that SEQUENCE-NUMBER, BEFORE or AFTER, and AROUND cannot be passed at the same time."
  (check-type hook-function symbol)
  (check-type identifier symbol)
  `(defmethod ,hook-function ,identifier ,@rest))

(defstruct node
  "Structure used to sort hook methods."
  (methods      nil :type list)
  ;; This node will be ordered before nodes with identifiers listed in this field.
  (order-before nil :type list)
  ;; STATE tells if the node was visited or is currently on the stack.
  (state        nil :type (member nil :visiting :visited)))
(declaim (ftype (function (node) (values list)) node-methods))

(defun sort-and-combine-methods (methods)
  "Sorts the METHODS according to the primary hook method order.
 Combines with around methods.
 Returns a list of NODES sorted and combined."
  (let ((result '())
        (primary-methods '())
        (around-methods '())
        (map (make-hash-table :test #'eq))
        (top-around-methods '()))
    (declare (list result primary-methods around-methods) (hash-table map))
    (multiple-value-setq (around-methods primary-methods)
      (list:partition methods #'method-around))
    ;; Better processed in the reverse order.
    ;; The loops below use PUSH to build lists of methods which then appear in proper order.
    (nreversef primary-methods)
    (nreversef around-methods)
    ;; Map by identifier -> node.
    ;; Where METHODS are the methods with the given identifier.
    (dolist (primary primary-methods)
      (let ((node (gethash (method-identifier primary) map)))
        (cond (node
               (unionf (node-order-before node) (method-before primary) :test #'eq)
               (push primary (node-methods node)))
              (t
               (setf (gethash (method-identifier primary) map)
                     (make-node :methods (list primary)
                                :order-before (method-before primary)))))))
    ;; Add the :AFTER dependencies as before dependencies to the corresponding node if found.
    (dolist (primary primary-methods)
      (dolist (after-identifier (method-after primary))
        (let ((node (gethash after-identifier map)))
          (when node
            (pushnew (method-identifier primary) (node-order-before node) :test #'eq)))))
    ;; Sort the MAP by depth first.
    (labels ((visit (node)
               (unless node (return-from visit))
               (ecase (node-state node)
                 ((nil)
                  (setf (node-state node) :visiting)
                  (dolist (later-node-identifier (node-order-before node))
                    (visit (gethash later-node-identifier map)))
                  (setf (node-state node) :visited)
                  ;; Nodes are pushed onto the list in reverse order.
                  ;; The result list needs not be reversed.
                  (push node result))
                 (:visited)
                 (:visiting
                  (method-combination-error
                   "Hook methods cannot be sorted topologically. ~S is found in a cycle."
                   (method-identifier (first (node-methods node))))))))
      (mapc (lambda (m) (visit (gethash (method-identifier m) map))) primary-methods))
    ;; Fill in the around methods for each primary method.
    (dolist (around around-methods)
      (etypecase (method-around around)
        ((eql t)
         (push around top-around-methods))
        (list
         (dolist (identifier (method-around around))
           (let ((primary (gethash identifier map)))
             (when primary
               (push around (node-methods primary))))))))
    (values
     ;; Sort by the number.
     (stable-sort
      result #'< :key (lambda (n) (method-number (first (node-methods n)))))
     ;; Top-level around methods
     (stable-sort top-around-methods #'< :key #'method-number))))

(defun* %with-method-wrapper (body-fn &optional (stream *debug-io*))
  (declare (self (function &optional stream) &rest t))
  (flet ((on-error (e)
           (let* (#+sbcl
                  (sb-ext:*suppress-print-errors* t)
                  (msg (ignore-errors (format nil "~A" e))))
             (format stream "~&~S: ~A~%" (type-of e) (or msg "<print-error>"))
             (ignore-errors #+sbcl (sb-debug:print-backtrace :stream stream))
             (ignore-errors (terpri stream))
             (ignore-errors (finish-output stream)))))
    (declare (dynamic-extent #'on-error))
    (handler-bind ((error #'on-error))
      (funcall body-fn))))
;; Something about interpreted GFs and custom method combination is broken.
;; Compiling this function avoids a GC failure.
(compile '%with-method-wrapper)

(defmacro with-method-wrapper ((&key stream) &body body)
  "Surround the BODY with a form that logs errors.
 STREAM is the stream to print the errors to - by default *DEBUG-IO*."
  ;; TODO(czak): Move this to some more generic log/error handling code.
  (ace.core.macro:once-only (&body body)
    `(%with-method-wrapper ,body ,@(when stream `(,stream)))))

(define-method-combination combination
    (&key (operator 'progn) (order :most-specific-first))
  ;; Accept all method qualifiers here as proper parsing was done in ADD-METHOD.
  ((methods * :order order))
  (unless (every (lambda (m) (typep m 'hook-method)) methods)
    (method-combination-error "HOOK:COMBINATION accepts only HOOK:METHODs"))
  (multiple-value-bind (primary around) (sort-and-combine-methods methods)
    (let ((primary-form
           `(,operator
             ,@(mapcar
                (lambda (node)
                  (let ((methods (node-methods node)))
                    `(with-method-wrapper ()
                       (call-method ,(first methods) ,(rest methods)))))
                primary))))
      (if around
          `(catch 'abort
             (call-method ,(first around)
                          (,@(rest around) (make-method ,primary-form))))
          `(catch 'abort ,primary-form)))))

(defun clear (hook-function &rest identifiers)
  "Clear the HOOK-FUNCTION function of all methods with the IDENTIFIERS.
 If no IDENTIFIERS are specified, all hook methods are removed from the hook function."
  (let* ((hook-function (coerce hook-function 'function))
         (methods (mop:generic-function-methods hook-function)))
    (dolist (method methods (values))
      (when (or (null identifiers) (member (method-identifier method) identifiers :test #'eq))
        (remove-method hook-function method)))))

(defun parse-method-qualifiers (method)
  "Parses the qualifiers of the METHOD and sets up its attributes."
  (declare (hook-method method))
  (let ((qualifiers (method-qualifiers method)))
    (let ((id (first qualifiers)))
      (typecase id
        ((and symbol (not null))
         (setf (method-identifier method) id))
        (t
         (expect
          nil "First qualifier of a hook method ~A needs to be a symbol. Got ~S."
          method id))))
    (typecase (second qualifiers)
      ((member :around)
       (expect (every #'symbolp (cddr qualifiers))
               "Hook method AROUND qualifiers must be symbols: ~A." method)
       (setf (method-around method) (or (cddr qualifiers) t)))
      ((member :before :after)
       ;; Search for sublists in the qualifiers that start with :before or :after.
       (let ((before (member :before (rest qualifiers)))
             (after (member :after (rest qualifiers))))
         ;; Check which of the sublists is longer and consequently may contain the other.
         (if (> (length after) (length before))
             ;; :after is longer so the :before list maybe a part of it, cut it off.
             (setf after (ldiff after before))
             ;; :before is longer so the :after list maybe a part of it, cut it off.
             (setf before (ldiff before after)))
         ;; Remove the :BEFORE and :AFTER markers to just get the identifiers.
         (pop after)
         (pop before)
         (expect (every #'symbolp after)
                 "~@<Hook method AFTER qualifiers must be symbols:~_ ~A.~:>" method)
         (expect (every #'symbolp before)
                 "~@<Hook method BEFORE qualifiers must be symbols:~_ ~A.~:>" method)
         (setf (method-after method) after
               (method-before method) before)))
      (number
       (expect (not (cddr qualifiers))
               "~@<Hook method ordered by a number should not have further qualifiers:~_ ~A.~:>"
               method)
       (setf (method-number method) (second qualifiers)))
      ((not null)
       (expect nil "~@<Unkown syntax for hook method qualifiers in:~_ ~A.~_ ~
          Should be: symbol [:around ..|:after ..|:before ..|<number>].~:>" method))))
  (values))

(defun remove-redefined-methods (generic-function method)
  "Remove methods from the GENERIC-FUNCTION having the same identifier and specializers as METHOD."
  (declare (standard-generic-function generic-function) (hook-method method))
  (dolist (m (mop:generic-function-methods generic-function))
    (when (and (eq (method-identifier m) (method-identifier method))
               (equalp (mop:method-specializers m) (mop:method-specializers method)))
      (remove-method generic-function m))))

(defmethod initialize-instance :around ((method hook-method) &rest initargs)
  "After the METHOD has been initialized, parse the qualifiers and set HOOK:METHOD specific slots."
  (declare (ignore initargs))
  (call-next-method)
  (parse-method-qualifiers method))

(defmethod add-method ((gf standard-generic-function) (method hook-method))
  "ADD-METHOD is a standard meta object protocol method called each time a METHOD
 is defined for a generic function (GF). This method specializes on HOOK:METHOD and
 removes similar methods based on the identifier and specializers."
  (remove-redefined-methods gf method)
  (call-next-method))

#-sbcl (error "TODO(czak): Activate the below hooks for other Lisps then SBCL.")

(define-hook-function at-restart ()
  :documentation "Methods called when the image is restarted."
  :default-method t)

#+sbcl (pushnew 'at-restart sb-ext:*init-hooks*)

(define-hook-function at-exit ()
  :documentation "Methods called before the Lisp process exits."
  :default-method t)

#+sbcl (pushnew 'at-exit sb-ext:*exit-hooks*)

(define-hook-function at-image-save ()
  :documentation "Methods called before the Lisp process image is saved."
  :default-method t)

#+sbcl (pushnew 'at-image-save sb-ext:*save-hooks*)

(define-hook-function after-gc ()
  :documentation "Methods called after a GC run."
  :default-method t)

(defun compilingp ()
  "True if we are in the middle of a compilation."
  (or *compile-file-pathname* *load-pathname* (ace.core.macro:lexenv)))

(defun %after-gc-maybe ()
  (unless (compilingp)
    (after-gc)))

(defmethod at-restart add-after-gc-hook ()
  (pushnew '%after-gc-maybe sb-ext:*after-gc-hooks*))
