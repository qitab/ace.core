;;; Copyright 2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

;;; Useful abstractions for the file system.

(defpackage #:ace.core.io
  (:use #:common-lisp
        #:ace.core.defun
        #:ace.core.os)
  (:export
   #:absolute-pathname-p
   #:directory-pathname-p
   #:coerce-to-directory
   #:probe-directory
   #:with-temporary-file
   #:with-temporary-content))

(in-package #:ace.core.io)

(defun absolute-pathname-p (name)
  "Returns NAME if it is an absolute pathname, otherwise NIL."
  (and (pathnamep name)
       (eq :absolute (first (pathname-directory name)))
       name))

(defun directory-pathname-p (name)
  "True if NAME is a directory pathname."
  (and (pathnamep name)
       (not (or (pathname-name name)
                (pathname-type name)))))

(defun coerce-to-directory (pathname)
  "Returns a PATHNAME that is a directory name."
  (if (directory-pathname-p pathname)
      pathname
      (make-pathname :directory (append (pathname-directory pathname)
                                        (list (file-namestring pathname)))
                     :host (pathname-host pathname)
                     :device (pathname-device pathname))))

(defun probe-directory (directory)
  "Returns the DIRECTORY truename or nil if not found.
 Returns a canonical truename /DIRECTORY/ only when the directory exists.
 If DIRECTORY is a file pathname, it is coerced to a directory pathname."
  (when directory
    (let ((dir-path (coerce-to-directory (pathname directory))))
      (and (probe-file (merge-pathnames "." dir-path))
           dir-path))))

(defun tmpdir ()
  "Return a temporary directory PATHNAME."
  (or (dolist (var '("TMPDIR" "TEMP" "TMP"))
        (let ((dir (probe-directory (getenv var))))
          (when dir (return dir))))
      (let ((homedir (probe-directory (user-homedir-pathname))))
        (and homedir (or (probe-directory (merge-pathnames "tmp" homedir))
                         (probe-directory (merge-pathnames "temp" homedir)))))
      (probe-directory "/tmp/")
      (probe-directory "/var/tmp/")))

(declaim (type (or null pathname) *temporary-directory*))
(defvar *temporary-directory* (tmpdir)
  "Contains the pathname for the temporary directory. Reset on each restart. Maybe NIL.")

(defmethod ace.core.hook:at-restart reset-tmpdir ()
  (setf *temporary-directory* (tmpdir)))

(defun* call-with-temporary-file (function &key
                                           element-type
                                           external-format
                                           pathspec)
  "Call the FUNCTION with one argument. The argument is an input/output stream
  for a temporary file with the EXTERNAL-FORMAT, ELEMENT-TYPE, and PATHSPEC."
  (declare (self (function &key t t pathname) &rest t))
  (loop for out = (open (merge-pathnames
                         (format nil "~@[~A-~]~X" (pathname-name pathspec) (random (ash 1 100)))
                         pathspec)
                        :element-type element-type :external-format external-format
                        :direction :io :if-exists nil :if-does-not-exist :create)
        when out
          do
     (unwind-protect
          (return (funcall function out))
       (ignore-errors (delete-file out))
       (ignore-errors (close out)))))

(defmacro with-temporary-file ((stream &key
                                       pathspec
                                       prefix
                                       type
                                       (element-type ''character)
                                       (external-format :default))
                               &body body)
  "Execute BODY with STREAM bound to a stream for a temporary output file.
 The file stores elements of ELEMENT-TYPE and has the EXTERNAL-FORMAT.
 PATHSPEC is used to create the file pathname.
 The default for PATHSPEC, if NIL, is *TEMPORARY-DIRECTORY*.
 PREFIX and TYPE are used to override parts of the PATHSPEC.
 PREFIX is the prefix of the file-name and TYPE is the file extension type.
 A random number is attached to the possibly empty PREFIX.
 The temporary file is deleted after the BODY has been executed."
  `(call-with-temporary-file
    (lambda (,stream) (declare (stream ,stream)) ,@body)
    :element-type ,element-type
    :external-format ,external-format
    :pathspec
    (merge-pathnames (make-pathname :name ,prefix :type ,type)
                     (or ,pathspec *temporary-directory*))))

(defun* call-with-temporary-content (function content &key element-type external-format)
  "Call FUNCTION with a stream argument on a temporary file with the CONTENT.
 ELEMENT-TYPE and EXTERNAL-FORMAT determine the parameters for the input stream."
  (declare (self (function sequence &key t t) &optional))
  (with-temporary-file (tmp :element-type '(unsigned-byte 8))
    (write-sequence content tmp)
    (finish-output tmp)
    (with-open-file (in tmp :element-type element-type :external-format external-format
                            :if-does-not-exist :error)
      (funcall function in))))


(defmacro with-temporary-content ((input content &key
                                         (element-type ''character)
                                         (external-format :default))
                                  &body body)
  "Creates an INPUT stream from a temporary file populated with the CONTENT.
 CONTENT is a binary sequence of octets (UNSIGNED-BYTE 8).
 ELEMENT-TYPE and EXTERNAL-FORMAT decide the type and format of the INPUT stream.
 The temporary file is deleted after the BODY has been executed."
  `(call-with-temporary-content (lambda (,input) (declare (stream ,input)) ,@body) ,content
                                :element-type ,element-type
                                :external-format ,external-format))
