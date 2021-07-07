;;; Copyright 2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

;;;;
;;;; Utilities to work with numbers, parsing, an non-finite values...
;;;;

;;;; TODO(czak): Unify the libraries across google3/lisp and travel/qpx.

(cl:defpackage #:ace.core.number
  (:use #:common-lisp #:ace.core.defun)
  #+sbcl
  (:import-from sb-ext
                sb-ext:single-float-positive-infinity
                sb-ext:single-float-negative-infinity
                sb-ext:double-float-positive-infinity
                sb-ext:double-float-negative-infinity
                sb-ext:float-nan-p)

  (:import-from #:ace.core.etc #:defglobal!)
  (:export #:read-number #:read-number* #:read-number-from-string
           #:read-double #:read-double-from-string
           #:read-float  #:read-float-from-string

           #:write-float #:write-number

           #:float-nan-p
           #:float-equal
           #:single-float-equal #:double-float-equal #:short-float-equal #:long-float-equal
           #:without-floating-point-traps
           #:single-float-positive-infinity
           #:single-float-negative-infinity
           #:double-float-positive-infinity
           #:double-float-negative-infinity
           #:double-float-not-a-number
           #:single-float-not-a-number
           #:short-float-not-a-number
           #:long-float-not-a-number
           #:hex-digit-p
           #:fixnump))

(cl:in-package #:ace.core.number)

(defun* fixnump (x) (declare (self inline (t) boolean))
  "True if X is a fixnum."
  (typep x 'fixnum))


(eval-when (:compile-toplevel :load-toplevel :execute)
(defun %without-floating-point-traps (body)
  "Executes BODY with all floating point exceptions switched off."
  (declare (function body))
  #-(or sbcl abcl ccl)
  (return-from %without-floating-point-traps
    ;; WARNING: Floating point traps have not been disabled here.
    (funcall body))
  #+sbcl
  (let ((traps (getf (sb-int:get-floating-point-modes) :traps)))
    (sb-int:set-floating-point-modes :traps nil)
    (unwind-protect (funcall body)
      (sb-int:set-floating-point-modes
       :traps traps :current-exceptions nil :accrued-exceptions nil)))
  #+abcl
  (let ((traps (getf (ext:get-floating-point-modes) :traps)))
    (ext:set-floating-point-modes :traps nil)
    (unwind-protect (funcall body)
      (ext:set-floating-point-modes :traps traps)))
  #+ccl
  (let ((overflow (ccl:get-fpu-mode :overflow))
        (underflow (ccl:get-fpu-mode :underflow))
        (division-by-zero (ccl:get-fpu-mode :division-by-zero))
        (invalid (ccl:get-fpu-mode :invalid))
        (inexact (ccl:get-fpu-mode :inexact)))
    (unwind-protect
         (progn
           (ccl:set-fpu-mode :overflow nil
                             :underflow nil
                             :division-by-zero nil
                             :invalid nil
                             :inexact nil)
           (funcall body))
      (ccl:set-fpu-mode :overflow overflow
                        :underflow underflow
                        :division-by-zero division-by-zero
                        :invalid invalid
                        :inexact inexact)))))

(defmacro without-floating-point-traps (&body body)
  "Executes the BODY with all floating point exceptions switched off."
  `(%without-floating-point-traps (lambda () ,@body)))

(defmacro define-float-nan (name type)
  "Define a NaN variable with the NAME for a give TYPE of float numbers."
  (let ((zero (coerce 0 type)))
    `(progn
       (declaim (,type ,name))
       (defglobal! ,name
         ;; This cannot be constant because the compiler would perform calculations on it
         ;; causing compile time errors. See also: double-float-(positive/negative)-infinity.
         (without-floating-point-traps (eval '(/ ,zero ,zero)))
         ,(format nil "A value that passes as a ~A but is not a number." type)))))

(define-float-nan double-float-not-a-number double-float)
(define-float-nan single-float-not-a-number single-float)
(define-float-nan short-float-not-a-number short-float)
(define-float-nan long-float-not-a-number long-float)

(defmacro define-float-equal (name type)
  "Defines a float equal operation NAME for the given float TYPE."
  `(defun* ,name (float1 float2)
     "Compares the FLOAT1 to FLOAT2. NaN values are considered equal."
     (declare (self foldable inline (,type ,type) boolean))

     (cond ((float-nan-p float1) (float-nan-p float2))
           ((float-nan-p float2) nil)
           ((= float1 float2)))))

(define-float-equal float-equal float)
(define-float-equal single-float-equal single-float)
(define-float-equal double-float-equal double-float)
(define-float-equal short-float-equal short-float)
(define-float-equal long-float-equal long-float)

;;
;; Parsing Java/C++ like numbers.
;;

(defun* hex-digit-p (char)
  (declare (self ((or null character)) boolean))
  "True if CHAR is a hex digit character."
  (and char (digit-char-p char 16) t))

(defun* number-char-p (char)
  (declare (self (character) boolean))
  "True if the CHAR belongs to the set of number characters."
  (and (position char "0123456789.-+abcdefinxABCDEFINX" :test #'char=) t))

(defun* end-of-token-p (char)
  (declare (self ((or null character)) boolean))
  "True if the CHAR terminates a token. I.e. it is whitespace, terminating char, or nil."
  (and (member char '(nil #\( #\) #\" #\' #\` #\, #\;
                      #\Backspace #\Tab #\Newline #\Linefeed #\Page #\Return #\Space)) t))

(defun* read-number (stream &key unsigned-p)
  (declare (self (stream &key boolean) (or number null)))
  "Reads an integer or float in C++/Java syntax. Floats are returned in double precision.
 The integer may be in radix 16 starting with 0x. The #x syntax is not supported.
 Returns a number or non-finite values like 'double-float-not-a-number'.
 Returns nil if the number could not be parsed.
 The STREAM may be advanced even in the case that the number has not been parsed.
 Arguments:
   stream - input stream.
   unsigned-p - reads only unsigned number - returns nil otherwise."

  (let ((chars nil)
        minus-p dot-p exponent-p)
    (declare (boolean minus-p dot-p exponent-p) (list chars))
    (labels ((peek () (peek-char nil stream nil))
             (next () (read-char stream nil))
             (store () (push (next) chars))
             (yield (&optional thing)
               (return-from read-number thing)))
      (declare (inline peek next store) (dynamic-extent #'yield))

      (setf minus-p (eql (peek) #\-))

      (when (or minus-p (eql (peek) #\+))
        (next)  ; Skip '-' or '+'
        (when (member (peek) '(#\- #\+))
          (yield)))

      (case (peek)
        ;; Inf
        ((#\I #\i)
         (next)
         (yield
          (and (member (peek) '(#\N #\n)) (next)
               (member (peek) '(#\F #\f)) (next)
               (end-of-token-p (peek))
               (if minus-p
                   (unless unsigned-p double-float-negative-infinity)
                   double-float-positive-infinity))))
        ;; Nan
        ((#\N #\n)
         (next)
         (yield
          (and (member (peek) '(#\A #\a)) (next)
               (member (peek) '(#\N #\n)) (next)
               (end-of-token-p (peek))
               double-float-not-a-number)))
        ;; Hexadecimal number
        (#\0
         (next)
         (when (member (peek) '(#\x #\X))
           (next)
           (let* ((chars (loop while (hex-digit-p (peek)) collect (next)))
                  (hex (and chars (end-of-token-p (peek))
                            (parse-integer (coerce (the list chars) 'string) :radix 16))))
             (yield (if (and hex minus-p (plusp hex))
                        (unless unsigned-p (- hex))
                        hex))))
         (push #\0 chars)))

      ;; Verify the number.
      (loop for char = (peek) until (end-of-token-p char) do
        (cond ((digit-char-p char)
               (store))
              ((char= char #\.)
               (when (or dot-p exponent-p) (yield))
               (setf dot-p t)
               (store))
              ((member char '(#\e #\E) :test #'char=)
               (when (or exponent-p (null chars)) (yield))
               (setf exponent-p t)
               (store)
               (when (member (peek) '(#\+ #\-) :test #'char=)
                 (store)))
              ((yield))))

      (when chars
        (let* ((*read-default-float-format* 'double-float)
               (*read-base* 10)
               (number (read-from-string (coerce (nreverse chars) 'string))))
          (assert (numberp number))
          (if (and minus-p (plusp number))
              (unless unsigned-p (- number))
              number))))))

(defun* read-number* (stream &key unsigned-p)
  (declare (self (stream &key boolean) (or number (member :inf :-inf :nan nil))))
  "Reads a number from the STREAM in C++/Java syntax.
 Returns a number, :nan, :-inf, :inf, or nil.
 Floats are double precision.
 Arguments:
  stream - input stream.
  unsigned-p - reads only unsigned number."

  (let ((number (read-number stream :unsigned-p unsigned-p)))
    (cond ((null number) nil)
          ((float-nan-p number) :nan)
          ((= number double-float-positive-infinity) :inf)
          ((= number double-float-negative-infinity) :-inf)
          (number))))

;; TODO(czak): Change the read-from-string functions to read directly from string instead of
;;             going through a string->stream->string->stream conversion.

(defun* read-number-from-string (string &key unsigned-p)
  (declare (self (string &key boolean) (or number null)))
  "Reads a number from a STRING. Returns a number, nan, or nil. Floats are double precision.
 Arguments:
  string - input value as string.
  unsigned-p - reads only unsigned number."
  (when (every #'number-char-p string)
    (with-input-from-string (in string)
      (read-number in :unsigned-p unsigned-p))))

(defun* read-double (stream)
  (declare (self (stream) (or double-float null)))
  "Reads a double float from the STREAM. Returns a double float, nan, or nil."
  (let ((number (read-number stream)))
    (and number (coerce number 'double-float))))

(defun* read-double-from-string (string)
  (declare (self (string) (or double-float null)))
  "Reads a double float from the STRING. Returns a double float, nan, or nil."
  (when (every #'number-char-p string)
    (with-input-from-string (in string) (read-double in))))

(defun* read-float (stream)
  (declare (self (stream) (or single-float null)))
  "Reads a single float from the STREAM. Returns a single float, nan, or nil."
  (let ((number (read-number stream)))
    (and number (coerce number 'single-float))))

(defun* read-float-from-string (string)
  (declare (self (string) (or single-float null)))
  "Reads a single float from the STRING. Returns a single float, nan, or nil."
  (declare (string string) (values (or single-float null) &optional))
  (when (every #'number-char-p string)
    (with-input-from-string (in string) (read-float in))))

;;
;; Writing Java/C++ like number.
;;

(defun* write-float (number &optional out)
  (declare (self (number &optional (or stream boolean)) (or null string)))
  "Writes the NUMBER as a float in a C++/Java readable way to string or the OUT stream.
 This will output: -inf, inf, nan for non-finite number.
 It will output number 1.d-3 <= number < 1.d+7 in the default float-point notation.
 It will output number in the scientific E notation for the numbers outside the range.
 Note that a dot is followed by 0 in the E notation and there is no + after E.
 Arguments:
  number - the float number to be printed.
  out - the output stream (default is to return a string)"

  (labels ((%write-float (number out)
             (declare (stream out))
             (cond ((float-nan-p number)
                    (write-string "nan" out))
                   ((minusp number)
                    (write-char #\- out)
                    (%write-float (- number) out))
                   ((zerop number)
                    (write-string "0.0" out))
                   ((and (<= 1.d-3 number) (< number 1.d+7))
                    (format out "~F" number))
                   ((<= number most-positive-double-float)
                    (let* ((string (format nil "~,,,,,,'EE" number))
                           (exp-sign (position #\+ string :from-end t)))
                      ;; NUMBER is positive, so if #\+ is present,
                      ;; then it is exponent sign which we want not to output.
                      (cond (exp-sign
                             (write-string string out :end exp-sign)
                             (write-string string out :start (1+ exp-sign)))
                            (t
                             (write-string string out)))))
                   (t (write-string "inf" out)))))
    (declare (dynamic-extent #'%write-float))
    (if out
        (progn (%write-float number (if (streamp out) out *standard-output*)) nil)
        (with-output-to-string (out nil :element-type 'base-char)
          (%write-float number out)))))

(defun* write-number (number &optional out)
  (declare (self (number &optional (or stream boolean)) (or null string)))
  "Writes a NUMBER in C++/Java readable way to string or the OUT stream.
 Signal error if the number cannot be represented in such way."
  (etypecase number
    ((or single-float double-float)
     (write-float number out))
    ((integer)
     (if out
         (progn (write number :stream out) nil)
         (write-to-string number)))))
