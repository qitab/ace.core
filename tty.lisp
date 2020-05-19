;;; Copyright 2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

;;;
;;; Functions related to TTY. E.g. Printing of ANSI escape codes.
;;;

(defpackage #:ace.core.tty
  (:use #:common-lisp)
  (:export #:ttyp #:*print-ansi*))

(in-package #:ace.core.tty)

(declaim (boolean *print-ansi*))
(defvar *print-ansi* nil "If non-nil, the ANSI escapes are printed by ~/ansi/ function.")

(defun ttyp (&optional (stream *standard-output*))
  "Returns true if the STREAM is a TTY."
  (declare (ignorable stream))
  #+sbcl
  (let* ((stream (sb-impl::stream-output-stream stream))
         (fd (and (sb-sys:fd-stream-p stream)
                  (sb-sys:fd-stream-fd stream))))
    (and (integerp fd)
         (plusp (sb-unix:unix-isatty fd)))))

(defun cl-user::ansi (stream argument &optional colon at &rest codes)
  "Prints the ARGUMENT to a TTY STREAM using ANSI codes if supported.
 The argument is printed as: CSI <code>(;<code>)* m <argument> CSI 0 m.
 At the end of the printing the TTY is reset to normal.
 COLON (:) prints the argument with *PRINT-PRETTY* bound to T.
 AT (@) prints the argument with *PRINT-ESCAPE* and *PRINT-READABLY* bound to T.
 CODES is a list of ANSI control codes.
 The codes are only emitted to an ANSI TTY.
 To force the codes to be always emitted by this function set *PRINT-ANSI*.
 Example:
   (format t \"~31/ansi/~%\" 'this-is-in-green)"
  ;; Optimize output for safety.
  (declare (optimize (speed 1) (safety 3)))
  (let ((*print-pretty* colon)
        (*print-escape* at)
        (*print-readably* at))
    (if (or *print-ansi* (ttyp stream))
        (format stream "~c[~{~D~^;~}m~W~c[0m" #\ESC codes argument #\ESC)
        (write argument :stream stream))))
