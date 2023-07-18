;;; Copyright 2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

;;; Definition of the ENUM type. An ENUM is a mapping between keywords and numbers.
;;; The symbols in this package are designed to be prefixed with the package prefix.
;;;
;;; Motivation:
;;; C++ and Java have the concept of an enumeration type. The protocol-buffers thus include enum
;;; as a primary concept. On the other side, Lisp does not explicitly define an equivalent concept
;;; to a C++ or Java enum. The implementation below provides a concept of an enumeration type that
;;; can be used in protocol-buffers and shared with processes in other common languages.
;;; The default way in Lisp is to represent an enumeration as a member type of keywords and
;;; use ECASE to switch on those keywords. In C++, Java, and protocol-buffers those enumerations
;;; are represented with compile time symbols that translate to numerals. Encoding the enumerations
;;; as numerals allows to compress such representation to an octet most of the time.
;;; The enum offers following functionality:
;;;  - defining ordinal constants that are bound to keywords.
;;;  - functions to convert the constant numerals to keywords and back.
;;;  - meta level description of the enum type that can be used by other modules for
;;;    compact representing such enumerations.
;;;  - define keyword aliases to the same constant numerals.
;;;  - exporting the enum type and the corresponding constants with the EXPORT option.
;;;  - Hinting to the maximum bit width of the enum wrt. forward compatibility.
;;; The use cases for this enum package.
;;;  - defining a member type with keywords that also provides a mapping to numerals.
;;;  - defining ordinal/enumerated constants.
;;;  - protocol-buffer enum implementation.
;;;  - storage of enum types in bit packed representations.
;;; An enum defines constants in the current package. E.g.:
;;;  (ENUM:DEFINE MY.COLORS :RED :GREEN :BLUE)
;;; will create following constants:
;;;  (DEFCONSTANT +MY.RED+ 1)
;;;  (DEFCONSTANT +MY.GREEN+ 2)
;;;  (DEFCONSTANT +MY.BLUE+ 3)
;;; "MY." is a scope for the constants. This is similar to C++ and protocol-buffer scope of enums,
;;; with the dot (.) being the scope/namespace separator.
;;; Having similar constant names to the keyword names allows to refer to the same enum value with
;;; the constant/numeral and the keyword. Having C++ scoping makes it easier to translate and
;;; reason about the enum values between this Lisp implementation and C++ or protocol-buffers.
;;;
;;; An enum also defines functions that translate those constant/numerals to keywords and back.
;;; Taken from the above example:
;;;  (MY.COLORS-TO-KEYWORD +MY.RED+) => :RED
;;;  (MY.COLORS-TO-KEYWORD 1) => :RED
;;;  (MY.COLORS-TO-NUMERAL :RED) => 1
;;; Those functions allow for quick translation between the canonical Lisp keyword representation
;;; of enums and a numeral representation used in C++, protocol-buffers, or bit packed data.
;;;
;;; One special aspect of this implementation is the focus on forward compatibility.
;;; The numerals are used to efficiently store and communicate the enums between different
;;; implementations or processes with different versions of the enumeration.
;;; Here a few measures are taken to help with the forward compatibility.
;;; The enumerations usually grow by adding values at the end of the enumeration.
;;; In case someone decides to change the order of the enum values, an alternative syntax allows
;;; to explicitly provide the numerals to each of the enum values, keywords, or constants.
;;; This allows the attribution of numerals to enum values to remain equal across the versions.
;;; One can specify the maximum envisioned numeral of BITS to be used by this and feature versions
;;; of the enum. This means that any bit packed representation will reserve this many bits to store
;;; the numerals of the enumeration.
;;; As mentioned above the enum values can be stored in a numeric form and translated to keywords
;;; using the TO-KEYWORD function. In the case the numeric value has no representation as a keyword
;;; the returned value is NIL. This can happen when the new enum value has been received from a
;;; new version of the software with added enum values. If such new enum value falls within the
;;; range of bits representation, the truthful re-transmission of such value is assured.
;;; If the numeric value falls out of scope of the bit representation, it still can be represented
;;; with the above mentioned NIL value (usually 0). In this later case, the value will not be
;;; properly re-transmitted.
;;;

(ace.core:defpackage* #:ace.core.enum
  (:use #:ace.core
        #:ace.core.etc
        #:ace.core.macro
        #:common-lisp)
  (:use-alias #:ace.core.symbol)
  (:export #:define
           #:numeral
           #:to-keyword
           #:to-numeral
           #:info #:make-info
           #:info-name
           #:info-min-numeral
           #:info-max-numeral
           #:info-bits
           #:info-default-numeral
           #:info-default-constant
           #:info-default-keyword
           #:info-values
           #:info-constants
           #:info-options
           #:info-exportp
           #:info-numeral-to-keyword
           #:info-keyword-to-numeral
           #:*code-generators*))

(in-package #:ace.core.enum)

;;;
;;; Types
;;;

(deftype numeral () "The range of enum numerals." '(signed-byte 32))
(deftype bits () "The numeral of bits used to represent an enum." '(integer 0 32))

;;;
;;; Code genration interface.
;;;

;;; TODO(czak): This interface is experimental and needs more applications and testing.
;;;             Do not use outside here until this notice goes away.

;;; TODO(czak): Put this into ace.core.enum.info package.

(eval-always
 (defstruct info
   "ENUM:INFO describes attributes of an ENUM."
   ;; The name of the enum.
   (name         nil :type symbol)
   ;; Numeral used to represent invalid value. Usually, 0.
   ;; Used for keyword-to-numeral conversions, when the keyword is not known.
   (nil-numeral  nil :type numeral)
   ;; The minimum numeral.
   (min-numeral  nil :type numeral)
   ;; The maximum known numeral.
   (max-numeral    nil :type numeral)
   ;; The width of this enum in bits, when encoded as min-max range.
   (bits         nil :type bits)
   ;; The corresponding default keyword - i.e. first one in the definition.
   ;; Not used anywhere in this code; reserved for extensions.
   (default      nil :type (or keyword null))
   ;; A sorted list of values (numeral keyword &rest options).
   (values       nil :type list)
   ;; A list of (cons constant numeral)..
   (constants    nil :type list)
   ;; Function name that returns numerals for keywords.
   (keyword-to-numeral nil :type symbol)
   ;; Function name that returns keywords for numerals.
   (numeral-to-keyword nil :type symbol)
   ;; True if the name, functions, and constants should be exported.
   (exportp      nil :type boolean)
   ;; A list of options provided to the enum definition.
   (options      nil :type list)))

(defmethod make-load-form ((self info) &optional env)
  (make-load-form-saving-slots self :environment env))

(defvar *code-generators* (list 'make-conversion-functions
                                'make-constants
                                'make-set-info-form)
  "A list of symbols for code generators that operate on the enum:info data.")

;;;
;;; Generic interface
;;;

(eval-always
 (defun %info (enum env)
   (and (constantp enum env) (get (eval* enum env) 'info))))

(declaim (ftype (function (t t &optional t) *) to-keyword))
(defgeneric to-keyword (enum-type numeral &optional default)
  (:documentation
   "Given an ENUM-TYPE, return the keyword for the given NUMERAL.
If no keyword was found, return the DEFAULT value."))

;; Make this a fast generic.
(define-compiler-macro to-keyword
    (&whole whole &environment env enum-type &rest rest)
  (let ((info (%info enum-type env)))
    (if info `(,(info-numeral-to-keyword info) ,@rest) whole)))

(declaim (ftype (function (t t &optional t) *) to-numeral))
(defgeneric to-numeral (enum-type keyword &optional default)
  (:documentation
   "Given an ENUM-TYPE, return the numeral for the given KEYWORD (or numeral).
If no numeral was found, return the DEFAULT value."))

;; Make this a fast generic.
(define-compiler-macro to-numeral
    (&whole whole &environment env enum-type &rest rest)
  (let ((info (%info enum-type env)))
    (if info `(,(info-keyword-to-numeral info) ,@rest) whole)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ENUM:DEFINE macro
;;;
;;; For efficiency reasons the enums are represented as int32 constants.
;;; There are auxiliary methods that convert between the int32 constants and keywords.
;;; The :BITS option allows to optimize space for an enum when stored in bit packed fields.
;;; So specifying :BITS 0 will always allocate the minimum number of bits necessary.
;;;
;;; The bit width is computed as (INTEGER-LENGTH (- max-numeral min-numeral -1)).
;;; The basic assumption is that by default enums count from 1 and the default min-numeral is 1.
;;; (1- min-numeral) is the value that represents NIL. So the minimum number of bits needed to
;;; represent the ENUM also includes the NIL value.
;;; The other assumption is that by default with the time new values get numerals assigned that are
;;; larger then the max-numeral.
;;;
;;; Since the range of keywords and constant values can grow,
;;; an implementation should not assume that the type is limited to those keywords or numerals.
;;; In a distributed system with many versions, the older systems will encounter new
;;; enum numerals so it is unwise to define the ENUM type by the range of known numerals.
;;; On the other hand, it is unlikely that the old systems will understand the new enum numerals
;;; as new keywords. This allows to define the ENUM type as a collection of keywords plus NIL
;;; for the yet unknown VALUES.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro define (name-and-options &rest tokens)
  "Defines an ENUM type. NAME-AND-OPTIONS specifies the name of the enum.

NAME-AND-OPTIONS can be a symbol or
(name &key bits prefix allow-aliases constants export)
Each of the options is described below:
  :BITS          - bits to be reserved for the representation.
  :PREFIX        - the prefix used to define ENUM constants (can be overridden at value level).
  :ALLOW-ALIASES - if non-nil, the ENUM can contain aliases.
  :CONSTANTS     - if nil, the constants are not generated.
  :EXPORT        - if non-nil, the name, enum-info, functions, and constants are exported.

If the NAME contains a dot (.), the part of the name up to the dot inclusive becomes a scope
for the produced numeral constants. The name of the constants has the following syntax.
   +[scope.][prefix-]<value>+.
E.g. for the following MY.BRIGHT.COLORS enum definition:
  (ENUM:DEFINE MY.BRIGHT.COLORS :RED 10 :GREEN (20 :BLUE :prefix :ULTRA))
the constant are:
  +MY.BRIGHT.RED+ => 1
  +MY.BRIGHT.GREEN+ => 10
  +MY.BRIGHT.ULTRA-BLUE+ => 20

BITS can be either unspecified, which defaults to 32 bits,
it can be 0, which will reserve the minimum necessary bits,
or it can be an explicit amount.

Each one of the TOKENS can have the following syntax:
 :KEYWORD - in this case the constant numeral starts counting at 1.
 (:KEYWORD &key prefix ...) - The numeral is counted automatically as above.
        PREFIX - is the prefix used for the text representation and constant name.
        ... - any other keyword options can be specified.
 (numeral :KEYWORD &key prefix ..) - As above, only the counted numeral is set to the specified.
 numeral - this sets the current counted value numeral for the next value.
This allows for a flexible syntax of the tokens. E.g. all the following define the same:
 (enum:define colors :RED :BLUE :YELLOW)
 (enum:define colors 1 :RED 2 :BLUE 3 :YELLOW)
 (enum:define colors (1 :RED) (2 :BLUE) (3 :YELLOW))"
  (let* ((name (if (consp name-and-options) (car name-and-options) name-and-options))
         (docstring (when (stringp (first tokens)) (list (pop tokens))))
         (info (parse-definition name-and-options tokens :package *package*)))
    `(progn
       ;; The type is defined by keywords that are just listed in the enumeration.
       (deftype ,name () ,@docstring '(member ,@(mapcar #'second (info-values info))))
       ,@(when (info-exportp info) `((eval-always (export ',name))))

       ,@(loop for generator in (reverse *code-generators*)
               nconc (funcall generator info))
       ',name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Implementation details.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun parse-definition (name-and-options tokens &key (package *package*))
  "Parses the enum definition. Returns enum INFO.
 Arguments:
  NAME-AND-OPTIONS - (name &key bits prefix allow-alias export)
  TOKENS           - are the tokens specified in the ENUM:DEFINE form.
  PACKAGE          - the package to define new symbols in."
  (declare (optimize (speed 1) (safety 3)))

  (destructuring-bind (enum-symbol &key
                                   (constants t)
                                   (bits 32)
                                   prefix
                                   allow-aliases
                                   export)
      (if (consp name-and-options) name-and-options (list name-and-options))
    (let* ((*package* package)
           (enum-name (symbol-name enum-symbol))
           (enum-options
            (when (consp name-and-options)
              (rest name-and-options)))
           (default-options (when prefix `(:prefix ,prefix)))
           (first-numeral
            (if (numberp (first tokens)) (first tokens) 1))
           (dot (position #\. enum-name :test #'char= :from-end t))
           (scope (and dot (subseq enum-name 0 dot)))
           (values '())
           (constantsp constants)
           (constants '()))

      (loop for numeral of-type number from first-numeral
            for token in tokens
            for value-options
              = (etypecase token
                  ;; A simple keyword gets expanded with default value options
                  (symbol (list* token default-options))
                  ;; A value that is already a list is retained.
                  (list
                   (when (numberp (first token))
                     (setf numeral (the numeral (pop token))))
                   (append token default-options))
                  ;; A number just sets the numeral value
                  (number
                   (setf numeral (- token 1))
                   nil))
            do
         (when value-options
           (destructuring-bind (keyword &key prefix &allow-other-keys) value-options
             ;; Use C++ scoping. In order to avoid clashes between the constants each of
             ;; the enumeration name can be split into a scope prefix and the enumeration name.
             ;; E.g.:
             ;;  (enum:define my.colors :red :green :blue)
             ;;  (enum:define your.colors :yellow :orange :red)
             ;; Here MY. and YOUR. are the scope prefixes that will also be used for the constants.
             ;; The +MY.RED+ and +YOUR.RED+ constants will have different values but will not
             ;; cause symbol conflicts.
             (push (list* numeral value-options) values)
             (when constantsp
               (let ((constant (intern (format nil "+~@[~A.~]~@[~A-~]~A+"
                                               scope prefix keyword))))
                 (push (list numeral constant) constants))))))

      (let* ((values         (nreverse values))
             (constants      (nreverse constants))
             (count          (length values))
             (default        (second (first values)))
             (sorted-values  (sort values #'< :key #'first))
             (first-value    (first sorted-values))
             (first-numeral  (first first-value))
             (last-value     (first (last sorted-values)))
             (last-numeral   (first last-value))
             (nil-value      (find nil values :key #'second))
             (nil-numeral    (or (first nil-value) (1- first-numeral)))
             (min-numeral    (min first-numeral nil-numeral))
             ;; All the numerals plus the NIL value.
             (span           (1+ (- last-numeral min-numeral)))
             ;; If bits = 0 was specified, use the span width.
             (bits           (if (zerop bits) (integer-length span) bits))
             (enum-info
              (make-info
               :name               enum-symbol
               :nil-numeral        nil-numeral
               :min-numeral        min-numeral
               :max-numeral        last-numeral
               :bits               bits
               :default            default
               :values             sorted-values
               :constants          constants
               :numeral-to-keyword (symbol:cat! enum-symbol :-to-keyword)
               :keyword-to-numeral (symbol:cat! enum-symbol :-to-numeral)
               :exportp            export
               :options            enum-options)))
        (expect
         (>= bits (integer-length span))
         "The requested ~D bits is less then required ~D bits for enum ~S."
         bits (integer-length span) enum-name)
        (unless allow-aliases
          ;; Remove duplicates by the numeral.
          (let ((dedup (remove-duplicates sorted-values :key #'first)))
            (expect
             (= count (length (the list dedup)))
             "Duplicate enum numerals in ~S:~{ ~S~}" enum-name
             ;; Calculate set difference by the keyword.
             (let ((dups (set-difference sorted-values dedup :key #'second)))
               (sort (mapcar #'first dups) #'<)))))

        enum-info))))

(defun make-set-info-form (info)
  "Generate a from that will create and set ENUM:INFO on the type at the load time."
  `((eval-always (setf (get ',(info-name info) 'info) ,info))))

(defun make-constants (info)
  "Generate the constants for the ENUM using the INFO."
  (when (info-constants info)
    (append
     (loop :for (numeral constant) :in (info-constants info)
           :collect `(defconstant ,constant ,numeral))

     (when (info-exportp info)
       `((eval-always (export ',(mapcar #'second (info-constants info)))))))))

(defun make-conversion-functions (info)
  "Returns forms for functions converting between enum numerals and keywords.
 INFO - the enum info containing metadata describing the enum properties."
  (declare (type info info))
  (with-slots (name nil-numeral min-numeral max-numeral values
               numeral-to-keyword keyword-to-numeral) info
    (let* ((span (1+ (- max-numeral min-numeral)))
           (count (length values))
           (indexed (< span (ash count 1)))
           ;; Use a rounded up array length to facilitate optimization.
           ;; I.e. if the compiler knows that the index fits into 3 bits,
           ;; it can optimize out bounds checks if the array is 8 in length.
           (array-len (and indexed (ash 1 (integer-length span))))
           ;; Two-Way mapping between keywords and numerals.
           (%keyword-numeral-map
            (symbol:cat! :+% name :-keyword-numeral-map%+))
           (%numeral-to-keyword-array
            (when indexed (symbol:cat! :+% name :-numeral-to-keyword-array%+))))

      `((define-constant ,%keyword-numeral-map
            (loop
              :with map = (make-hash-table :test #'eq :size ,count)
              :with info = (get ',name 'info)
              :for (numeral keyword) :in (reverse (info-values info))
              :do
              (setf (gethash keyword map) numeral)
              (setf (gethash numeral map) keyword)
              :finally (return map))
          :test #'equalp)

        ,@(when indexed
            `((define-constant ,%numeral-to-keyword-array
                  ,(loop
                     :with array = (make-array array-len :initial-element nil)
                     :for (numeral keyword) :in (reverse (info-values info))
                     :do (setf (aref array (- numeral min-numeral)) keyword)
                     :finally (return array))
                :test #'equalp)))

        (eval-always ; the values are folded at compile time.

         ;;
         ;; numeral -> keyword
         ;;

         (defun* ,numeral-to-keyword (numeral &optional default)
           "Returns the keyword for an enum value numeral or nil if not (yet) listed."
           (declare (self inline foldable (numeral &optional symbol) symbol))
           ,(if indexed
                `(let ((index (- numeral ,min-numeral)))
                   (declare (numeral index))
                   (or (and (<= 0 index ,(1- array-len))
                            (aref ,%numeral-to-keyword-array index))
                       default))
                `(values (gethash numeral ,%keyword-numeral-map default))))
         ;; Force folding using a compiler-macro.
         (define-compiler-macro ,numeral-to-keyword
             (&whole whole &environment env numeral &optional default)
           (declare (notinline ,numeral-to-keyword))
           (let (default!)
             (cond
               ((and (constantp numeral env) (constantp default env))
                ;; fold
                (,numeral-to-keyword (eval* numeral env) (eval* default env)))
               ((not (constantp default env))
                whole)
               ((setf default! (eval* default env))
                ;; If there is a default specified, and it is not NULL,
                ;; Then one needs to call NUMERAL-TO-KEYWORD.
                (if (eql default! default)
                    whole
                    `(,',numeral-to-keyword ,numeral ,default!)))
               ;; Otherwise, one can optimize the indexed case and avoid
               ;; (or .... NIL) - which SBCL fails to optimize out.
               ,(if indexed
                    `(t
                      (with-gensyms (%index)
                        `(let ((,%index ,(if (zerop ,min-numeral)
                                             numeral
                                             `(- ,numeral ,,min-numeral))))
                           (declare (numeral ,%index))
                           (and (<= 0 ,%index ,,(1- array-len))
                                (aref ,',%numeral-to-keyword-array ,%index)))))
                    '(whole)))))

         (defmethod to-keyword ((type (eql ',name)) numeral &optional default)
           "Return the ENUM keyword for the given NUMBER."
           (declare (ignore type) (type numeral numeral))
           (,numeral-to-keyword numeral default))

         ;;
         ;; keyword -> numeral
         ;;

         (defun* ,keyword-to-numeral (keyword &optional (default ,nil-numeral))
           "Returns the enum value numeral for a keyword or rises an error if not found."
           (declare (self inline foldable
                          ((or symbol numeral) &optional numeral)
                          numeral))
           (etypecase keyword
             (number keyword)
             (symbol (values (gethash keyword ,%keyword-numeral-map default)))))

         ;; Force folding using a compiler-macro.
         (define-compiler-macro ,keyword-to-numeral
             (&whole whole &environment env keyword
                     &optional (default ,nil-numeral))
           (declare (notinline ,keyword-to-numeral))
           (if (and (constantp keyword env) (constantp default env))
               (,keyword-to-numeral (eval* keyword env) (eval* default env))
               whole))

         (defmethod to-numeral ((type (eql ',name)) keyword
                                &optional (default ,nil-numeral))
           "Return the ENUM numeral for a KEYWORD (or numeral)."
           (declare (ignore type) (type (or symbol numeral) keyword))
           (,keyword-to-numeral keyword default))

         ,@(when (info-exportp info)
            `((export '(,numeral-to-keyword ,keyword-to-numeral)))))))))
