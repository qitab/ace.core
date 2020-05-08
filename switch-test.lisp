;;;;
;;;; Tests the switch utility.
;;;;

(defpackage #:google.core.switch-test
  (:use #:common-lisp
        #:google.core.switch
        #:google.test))

(in-package :google.core.switch-test)

(defvar *red* "RED")
(defvar *blue* "BLUE")
(defvar *green* "GREEN")
(defvar *1* 1)

(deftest test-switch-eql ()
  (expect (eq :A (switch *1*
                   (1 :A)
                   (2 :B))))
  (expect (eq nil (switch *1*
                   (10 :A)
                   (20 :B))))
  (expect (eq :C (switch *1*
                   (10 :A)
                   (20 :B)
                   :otherwise :C)))
  (expect (eq :A (eswitch *1*
                   (1 :A)
                   (2 :B))))
  (assert-error (eswitch *1*
                  (10 :A)
                  (20 :B))))

(deftest test-switch-test ()
  (expect (eq :blue (switch *blue* :test 'string=
                            ("RED" :red)
                            ("BLUE" :blue)
                            :otherwise (error "Unknown color"))))

  (let ((otherwisep nil))
    (expect (eq :otherwise (switch *green* :test 'string=
                                   ("RED" :red)
                                   ("BLUE" :blue)
                                   :otherwise
                                   (setf otherwisep t)
                                   :otherwise)))
    (expect otherwisep))

  (assert-error
    (switch *green* :test 'string=
            ("RED" :red)
            ("BLUE" :blue)
            :otherwise (error "Unknown color"))))

(deftest test-eswitch-test ()
  (assert-macro-error
   (eswitch *blue* :test 'string=
            ("RED" :red)
            ("BLUE" :blue)
            :otherwise (error "Unknown color")))

  (expect (eq :blue (eswitch *blue* :test 'string=
                            ("RED" :red)
                            ("BLUE" :blue))))

  (expect (eq :blue (eswitch (intern "BLUE" :keyword)
                      (:red :red)
                      (:blue :blue))))

  (assert-error
    (eswitch *green* :test 'string=
             ("RED" :red)
             ("BLUE" :blue))))

(deftest test-switch* ()
  (expect (eq :A (switch* *1*
                   (1 (return :A))
                   (2 :B))))
  (expect (eq :B (switch* *1*
                   (1 :A)
                   (2 (return :B)))))
  (expect (eq nil (switch* *1*
                   (10 :A)
                   (20 :B)))))

(deftest test-switch*-test ()
  (expect (eq :blue (switch* *red* :test 'string=
                            ("RED" :red)
                            ("BLUE" (return :blue))
                            :otherwise (error "Unknown color"))))

  (expect (eq :blue (switch* *blue* :test 'string=
                            ("RED" :red)
                            ("BLUE" (return :blue))
                            :otherwise (error "Unknown color"))))

  (let ((otherwisep nil))
    (expect (eq :otherwise (switch* *green* :test 'string=
                                    ("RED" (return :red))
                                    ("BLUE" (return :blue))

                                    :otherwise
                                    (setf otherwisep t)
                                    :otherwise)))
    (expect otherwisep))

  (assert-error
    (switch* *red* :test 'string=
            ("RED"  :red)
            ("BLUE" :blue)
            :otherwise (error "Unknown color")))

  (assert-error
    (switch* *green* :test 'string=
            ("RED" :red)
            ("BLUE" :blue)
            :otherwise (error "Unknown color"))))
