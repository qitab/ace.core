;; Package provides rudimentary OS related functionality.

(defpackage :ace.core.os
  (:use :cl :ace.core.defun)
  (:export
   #:getenv
   #:cwd
   #:program-name))

(in-package :ace.core.os)

(defun* getenv (variable &optional default)
  "Return the value of the execution environment VARIABLE and if not found return DEFAULT."
  (declare (self (string &optional (or null string)) (or null string)))
  (or #+sbcl      (sb-posix:getenv variable)
      #+cmu       (cdr (assoc variable ext:*environment-list* :test #'string=))
      #+allegro   (sys:getenv variable)
      #+clisp     (ext:getenv variable)
      #+ecl       (si:getenv variable)
      #+lispworks (lispworks:environment-variable variable)
      #+(and uiop (not (or sbcl cmu allegro clisp ecl lispworks)))
      (uiop/os:getenv variable)
      default))

(defun* (setf getenv) (value variable)
  "Set the value of the execution environment VARIABLE.
If VALUE is NIL, the VARIABLE is unset from the environment."
  (declare (self ((or null string) string) (or null string)))
  #+sbcl
  (if value
      (sb-posix:setenv variable value 1)
      (sb-posix:unsetenv variable))
  value)

(defun* cwd ()
  "Return the current working directory as string."
  (declare (self () string))
  (let* ((cwd (or  #+sbcl (pathname (sb-posix:getcwd))
                   #-sbcl *default-pathname-defaults*))
         (file (file-namestring cwd)))
    (namestring
     (if (plusp (length file))
         ;; Coerce to directory.
         (make-pathname :directory
                        (append (pathname-directory cwd) (list file))
                        :name nil :type nil :version nil :defaults cwd)
         cwd))))

(defun* program-name ()
  "Return the program name for the current process."
  (declare (self () string))
  (or
   #+sbcl (first sb-unix::*posix-argv*)
   ""))
