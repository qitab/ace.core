;;; Utilities to work with typed arithmetic and boolean operations on numbers.
;;;

;;; TODO(czak): Unify the libraries across google3/lisp and travel/qpx.
;;; TODO(czak): Implement portable modular arithmetic.

(defpackage #:ace.core.fast-ops
  ;; TODO(czak): Remove nicknames.
  (:nicknames #:google.core.fast-ops)
  (:use #:common-lisp #:ace.core.defun)
  #+sbcl
  (:import-from sb-c sb-c::mask-signed-field)
  (:export #:define-typed-integer-math-functions
           #:mask-signed-field))

(in-package #:ace.core.fast-ops)

#-sbcl
(defun* mask-signed-field (size integer)
  "Extract SIZE lower bits from INTEGER, considering them as a
2-complement SIZE-bits representation of a signed integer."
  (declare (self inline foldable (fixnum integer) integer))
  ;; Default implementation from SBCL.
  (cond ((zerop size)
         0)
        ((logbitp (1- size) integer)
         (dpb integer (byte size 0) -1))
        (t
         (ldb (byte size 0) integer))))

(defmacro %dostack ((var rest-list &optional result (start-index 0)) &body body)
  "Optimized version of dolist for stack allocated REST-LIST.

REST-LIST is evaluated only once and the VAR is bound to successive values of the REST-LIST.
Returns RESULT if specified. Otherwise, returns NIL.

Note, that this macro cannot be used in interpreted code.

Parameters:
  VAR - the symbol of the variable to which each value of the LIST will be bound.
  REST-LIST - the symbol of the list that references the stack allocated list.
  RESULT - is a result form.
  START-INDEX - is an index at which to process the list.
"
  (check-type var symbol)
  (check-type rest-list symbol)
  ;; From sbcl/src/sysmacs.lisp about do-rest-arg - "This is an internal-use-only macro."!
  #+sbcl `(sb-kernel:do-rest-arg ((,var) ,rest-list ,start-index ,result) ,@body)
  #-sbcl `(dolist (,var (nthcdr ,start-index ,rest-list) ,@(when result `(,result)) ,@body)))

(defmacro the! (type &body body)
  "Declares the result of the BODY to be of the TYPE. No safety checks are performed.
 Related: SB-EXT:TRULY-THE."
  `(let ((result (progn ,@body)))
     (locally (declare (optimize (safety 0) (speed 3)))
       (the ,type result))))

(defmacro unary-op (type prefix op)
  "Allows to declare a unary operations on a TYPE.
 No assumptions should be made that the resulting operation is modular to the type.
 Arguments:
  type   - the type of the operation arguments.
  prefix - for the symbol name of the resulting operation.
  op     - the implementing operation."
  (let ((name (intern (format nil "~A~A" prefix op))))
    `(progn
       (declaim (inline ,name))         ; always inline
       (defun* ,name (a)
         ,(format nil "A ~S version of ~S.~%~A" type op (documentation op 'function))
         (declare (self foldable (,type) ,type))
         (declare (optimize (speed 3)))
         (the ,type (,op a))))))

(defmacro binary-op (type prefix op &key (args '(a b)) (types `(,type ,type)) (values `(,type)))
  "Allows to declare a binary operations on a TYPE.
 No assumptions should be made that the resulting operation is modular to the type.
 Arguments:
  type   - the type of the operation arguments.
  prefix - for the symbol name of the resulting operation.
  op     - the implementing operation.
  args   - the names of the arguments (default: a b).
           This can include the &optional lambda keyword between the first and second argument name.
           An &optional keyword indicates that the second argument is optional.
  types  - the types of the arguments (default: type).
  values - the types of the returned values (default: type)."
  (let* ((a (first args))
         (type-a (first types))
         (optional (and (eq (second args) '&optional) '(&optional)))
         (b-param (the (not null) (if optional (third args) (second args))))
         (b (if (atom b-param) b-param (first b-param)))
         (type-b (second types))
         (name (intern (format nil "~A~A" prefix op))))
    `(progn
       (declaim (inline ,name))         ; always inline
       (defun* ,name (,a ,@optional ,b-param)
         ,(format nil "A ~S version of ~S.~%~A" type op (documentation op 'function))
         (declare (self foldable (,type-a ,@optional ,type-b) ,@values))
         (declare (optimize (speed 3) #-dbg (safety 0)))
         (the ,type (,op ,a ,b))))))

(defmacro n-ary-op (type prefix op &key (identity `(the ,type (,op))))
  "Allows to declare an N-ary operations on a type.
 The operator will accept 0 or more arguments.
 The special case of one argument to the new operator will translate to (the TYPE number).
 No assumptions should be made that the resulting operation is modular to the type.
 Arguments:
  type   - the type of the operation arguments.
  prefix - for the symbol name of the resulting operation.
  op     - the implementing operation.
  identity - the value returned when no arguments have been specified to OP."
  (let ((name (intern (format nil "~A~A" prefix op))))
    `(progn
       ;; No need to define this inline since it has a compiler-macro.
       (defun* ,name (&rest numbers)
         ,(format nil "A ~S version of ~S.~%~A" type op (documentation op 'function))
         (declare (self foldable (&rest ,type) ,type))
         (declare (dynamic-extent numbers))
         (declare (optimize (speed 3) #-dbg (safety 0)))
         ;; Only used if the number of arguments is unknown at compile-time.
         (the ,type
              (if numbers
                  (let ((result (the! ,type (first numbers))))
                    (declare (type ,type result))
                    (%dostack (number numbers result 1)
                      (setf result (the ,type (,op result (the ,type number))))))
                  ,identity)))
       #+sbcl (compile ',name) ;; Do not interpret.
       (define-compiler-macro ,name (&rest args)
         `(locally (declare (optimize (speed 3) #-dbg (safety 0)))
            ,(if args
                 (reduce (lambda (x y) `(the ,',type (,',op ,x (the ,',type ,y)))) (rest args)
                         :initial-value `(the ,',type ,(first args)))
                 ,identity))))))

(defmacro n1-ary-op (type prefix op)
  "Allows to declare an N-ary operations on a type.
 The operator will accept 1 or more arguments.
 No assumptions should be made that the resulting operation is modular to the type.
 Arguments:
  type   - the type of the operation arguments.
  prefix - for the symbol name of the resulting operation.
  op     - the implementing operation."
  (let ((name (intern (format nil "~A~A" prefix op))))
    `(progn
       ;; No need to define this inline since it has a compiler-macro.
       (defun* ,name (number &rest more-numbers)
         ,(format nil "A ~S version of ~S.~%~A" type op (documentation op 'function))
         (declare (self foldable (,type &rest ,type) ,type))
         (declare (dynamic-extent more-numbers))
         (declare (optimize (speed 3)))
         ;; Only used if the number of arguments is unknown at compile-time.
         (the ,type
              (if more-numbers
                  (let ((result number))
                    (declare (type ,type result))
                    (%dostack (number more-numbers result)
                      (declare (type ,type number))
                      (setf result (the ,type (,op result number)))))
                  (,op (the ,type number)))))
       #+sbcl (compile ',name) ;; Do not interpret.
       (define-compiler-macro ,name (number &rest more-numbers)
         `(locally (declare (optimize (speed 3) #-dbg (safety 0)))
            ,(if more-numbers
                 (reduce (lambda (x y) `(the ,',type (,',op ,x (the ,',type ,y)))) more-numbers
                         :initial-value `(the ,',type ,number))
                 ;; Special case for (fx- foo).
                 `(the ,',type (,',op (the ,',type ,number)))))))))

(defmacro unary-predicate (type prefix op)
  "Allows to declare a unary predicate on a type.
 The predicate is optimized to not check for the type in compiled code.
 In interpreted code the type checks could still be applied.
 Arguments:
  type   - the type of the predicate arguments.
  prefix - for the symbol name of the resulting predicate.
  op     - the implementing predicate."
  (let ((name (intern (format nil "~A~A" prefix op))))
    `(progn
       (declaim (inline ,name))         ; always inline
       (defun* ,name (number)
         ,(format nil "A ~S version of ~S.~%~A" type op (documentation op 'function))
         (declare (self foldable (,type) boolean))
         (declare (optimize (speed 3)))
         (,op number)))))

(defmacro binary-predicate (type prefix op &optional (args '(a b)))
  "Allows to declare a binary predicate on a type.
 The predicate is optimized to not check for the type in compiled code.
 In interpreted code the type checks could still be applied.
 Arguments:
  type   - the type of the predicate arguments.
  prefix - for the symbol name of the resulting predicate.
  op     - the implementing predicate.
  args   - the names for the arguments (default: a b)."
  (destructuring-bind (a b) args
    (let ((name (intern (format nil "~A~A" prefix op))))
      `(progn
         (declaim (inline ,name))         ; always inline
         (defun* ,name (,a ,b)
           ,(format nil "A ~S version of ~S.~%~A" type op (documentation op 'function))
           (declare (self foldable (,type ,type) boolean))
           (declare (optimize (speed 3)))
           (,op ,a ,b))))))

(defmacro n1-ary-predicate (type prefix op)
  "Allows to declare an N-ary predicate on a type.
 The predicate always needs at least one argument.
 The predicate is optimized to not check for the type in compiled code.
 In interpreted code the type checks could still be applied.
 Arguments:
  type   - the type of the predicate arguments.
  prefix - for the symbol name of the resulting predicate.
  op     - the implementing predicate."
  (let ((name (intern (format nil "~A~A" prefix op))))
    `(progn
       ;; No need to define this inline since it has a compiler-macro.
       (defun* ,name (arg &rest more-args)
         ,(format nil "A ~S version of ~S.~%~A" type op (documentation op 'function))
         (declare (self foldable (,type &rest ,type) boolean))
         (declare (dynamic-extent more-args))
         (declare (optimize (speed 3)))
         ;; Only used if the number of arguments is unknown at compile-time.
         (if more-args
             (let ((a arg))
               (%dostack (b more-args t)
                 (declare (type ,type b))
                 (unless (,op (the ,type a) b) (return nil))
                 (setf a b)))
             (,op (the ,type arg))))
       #+sbcl (compile ',name) ;; Do not interpret.
       (define-compiler-macro ,name (&rest args)
         `(locally (declare (optimize (speed 3) #-dbg (safety 0)))
            (,',op ,@(mapcar (lambda (arg) `(the ,',type ,arg)) args)))))))

(defmacro define-typed-number-math-functions (type prefix)
  "Defines typed arithmetic predicates and operations.
 The functions are optimized to not check for the type in compiled code.
 In interpreted code the type checks could still be applied.
 Arguments:
  type   - the type of operations.
  prefix - the prefix for all of the predicates and operations.
 Example:
  (define-typed-number-math-functions fixnum \"I\")
    will define: iash itruncate ifloor imod irem i+ i- ...."
  (flet (($ (name) (intern (format nil "~A~A" prefix name))))
    `(progn
       (unary-op ,type ,prefix 1+)
       (unary-op ,type ,prefix 1-)

       (binary-op ,type ,prefix truncate :args (number &optional (divisor 1)) :values (,type ,type))
       (binary-op ,type ,prefix floor    :args (number &optional (divisor 1)) :values (,type ,type))
       (binary-op ,type ,prefix ceiling  :args (number &optional (divisor 1)) :values (,type ,type))
       (binary-op ,type ,prefix mod      :args (number divisor))
       (binary-op ,type ,prefix rem      :args (number divisor))

       (n-ary-op ,type ,prefix +)
       (n-ary-op ,type ,prefix *)
       (n1-ary-op ,type ,prefix -)
       (n1-ary-op ,type ,prefix /)

       (n1-ary-op ,type ,prefix min)
       (n1-ary-op ,type ,prefix max)

       (unary-predicate ,type ,prefix minusp)
       (unary-predicate ,type ,prefix plusp)
       (unary-predicate ,type ,prefix zerop)

       (n1-ary-predicate ,type ,prefix  <)
       (n1-ary-predicate ,type ,prefix  >)
       (n1-ary-predicate ,type ,prefix <=)
       (n1-ary-predicate ,type ,prefix >=)
       (n1-ary-predicate ,type ,prefix  =)
       (n1-ary-predicate ,type ,prefix /=)

       (define-modify-macro ,($ 'decf) (&optional (delta 1)) ,($ '-))
       (define-modify-macro ,($ 'incf) (&optional (delta 1)) ,($ '+))
       (define-modify-macro ,($ 'maxf) (&rest more-args)     ,($ 'max))
       (define-modify-macro ,($ 'minf) (&rest more-args)     ,($ 'min)))))

(defmacro define-typed-integer-math-functions (type prefix)
  "Defines typed integer arithmetic predicates and operations.
 It will also apply (define-typed-arithmetic type prefix) for related predicates and operations.
 The functions are optimized to not check for the type in compiled code.
 In interpreted code the type checks could still be applied.
 No assumptions should be made that the resulting functions are modular to the type.

 Arguments:
  type   - the type of operations.
  prefix - the prefix for all of the predicates and operations.
  logand-identity - the value returned from the LOGAND operation without arguments.
 Example:
  (define-typed-integer-math-functions fixnum I)
    will define: iash ilogand iodd as well as itruncate ifloor imod irem ...."
  (flet (($ (name) (intern (format nil "~A~A" prefix name))))
    `(progn
       (define-typed-number-math-functions ,type ,prefix)

       (binary-op ,type ,prefix ash :args (,type count))
       (binary-op ,type ,prefix logandc1)
       (binary-op ,type ,prefix logandc2)

       (n-ary-op ,type ,prefix logior)
       (n-ary-op ,type ,prefix logxor)

       ,(if (typep -1 type)
          `(n-ary-op ,type ,prefix logand)
          `(n1-ary-op ,type ,prefix logand))

       (unary-predicate ,type ,prefix oddp)
       (unary-predicate ,type ,prefix evenp)

       (binary-predicate ,type ,prefix logbitp (index ,type))

       (define-modify-macro ,($ 'ashf)      (count) ,($ 'ash))
       (define-modify-macro ,($ 'logandc1f) (,type) ,($ 'logandc1))
       (define-modify-macro ,($ 'logandc2f) (,type) ,($ 'logandc2))

       (define-modify-macro ,($ 'logiorf) (&rest more-args) ,($ 'logior))
       (define-modify-macro ,($ 'logxorf) (&rest more-args) ,($ 'logxor))
       (define-modify-macro ,($ 'logandf) (&rest more-args) ,($ 'logand)))))
