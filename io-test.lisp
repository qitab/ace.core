;;; Test for #:ace.core.io.

(defpackage #:ace.core.io-test
  (:use #:common-lisp
        #:ace.core.io
        #:ace.core.os
        #:ace.test)
  (:import-from #:ace.core.io #:*temporary-directory*))

(in-package #:ace.core.io-test)

(deftest directory-pathname-p-test ()
  (expect (directory-pathname-p (pathname "/")))
  (expect (directory-pathname-p (pathname "/tmp/")))
  (expect (not (directory-pathname-p (pathname "/tmp"))))
  (expect (directory-pathname-p (pathname "/var/tmp/")))
  (expect (not (directory-pathname-p (pathname "/var/tmp.dir")))))

(deftest coerce-to-directory-test ()
  (expect (equal #P"/" (coerce-to-directory (pathname "/"))))
  (expect (equal #P"/tmp/" (coerce-to-directory (pathname "/tmp/"))))
  (expect (equal #P"/tmp/" (coerce-to-directory (pathname "/tmp"))))
  (expect (equal #P"/var/tmp.dir/" (coerce-to-directory (pathname "/var/tmp.dir/"))))
  (expect (equal #P"/var/tmp.dir/" (coerce-to-directory (pathname "/var/tmp.dir")))))

(deftest with-temporary-content-test ()
  (with-temporary-content (in #(65 66 67 10))
    (expect (string= "ABC" (read-line in)))))

(deftest with-temporary-file-test ()
  ;; Let's set the temporary directory to the TEST_UNDECLARED_OUTPUTS_DIR...
  ;; for demonstration purpose that temporary files are always temporary - i.e. they disappear.
  (let ((*temporary-directory*
         (or #+google3 (probe-directory (getenv "TEST_UNDECLARED_OUTPUTS_DIR"))
             *temporary-directory*)))
    (expect
     (eq :body
         (with-temporary-file (s)
           (format s "'TEMPORARY'? You keep using that word. ~
 I don't think it means what you think it means.~%")
           (and s :body))))
    (let ((tmp (with-temporary-file (s) (pathname s))))
      (expect (not (probe-file tmp))))))
