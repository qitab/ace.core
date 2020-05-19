;;; Copyright 2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

;;;;
;;;; Tests for the numbers package.
;;;;

(defpackage #:ace.core.number-test
  (:use #:common-lisp
        #:ace.core.number
        #:ace.test))

(in-package #:ace.core.number-test)

(defun test-number (expected string &key unsigned-p)
  (let ((actual (read-number-from-string string :unsigned-p unsigned-p)))
    (expect (equal expected actual)
            "Failed parsing: ~S, expected: ~A, actual: ~A." string expected actual)))

(deftest test-read-number1 ()
  (test-number 0 "0")
  (test-number 0 "-0")
  (test-number 0 "+0")

  (test-number 1 "1")
  (test-number -1 "-1")

  (test-number 0 "0.")
  (test-number 0 "+0.")
  (test-number 0 "-0.")

  (test-number 1 "1.")
  (test-number -1 "-1.")

  (test-number 1 "+1")
  (test-number 1 "+1")

  (test-number 10 "+10")
  (test-number 11 "+11"))

(deftest test-read-not-number1 ()
  (mapc (lambda (str) (test-number nil str))
        '(""
          " "
          "  "
          "xx"

          "-+0"
          "+-0"

          "-+2"
          "+-2"

          "0-+2"
          "0+-2"

          "--0"
          "0-"
          "0--"
          "-0-0"
          "0-0"
          "-0-00"
          "0-00"
          "-00-0"
          "00-0"
          "00-00"

          "++0"
          "0+"
          "0++"
          "+0+0"
          "0+0"
          "+0+00"
          "0+00"
          "+00+0"
          "00+0"
          "00+00"

          "0nan"
          "0NAN"
          "0inf"
          "0Inf"

          "--INF"
          "--Inf"
          "--inf"

          "NAN-"
          "Nan-"
          "nan-"

          "INF-"
          "Inf-"
          "inf-"

          "NAN0"
          "Nan0"
          "nan0"

          "INF0"
          "Inf0"
          "inf0"

          "0x"
          "00x0"
          "0x-0"
          "00x-0"
          "0x-10"
          "00x-10"
          "-0x-0"
          "-00x-0"
          "-0x-10"
          "-00x-10"

          "0x"
          "00x0"
          "0x+0"
          "00x+0"
          "0x+10"
          "00x+10"
          "-0x+0"
          "-00x+0"
          "-0x+10"
          "-00x+10"

          "0x.0"
          "0x0.0"
          "0x0."

          "0.0eNAN"
          "0.0eNan"
          "0.0enan"

          "0.0eINF"
          "0.0eInf"
          "0.0einf"

          "0-nan"
          "0-NAN"
          "0-inf"
          "0-Inf"

          "i0"
          "a0"
          "f0"
          "n0"
          "y0"
          "I0"
          "A0"
          "F0"
          "N0"
          "Y0"
          ",0"

          "0i"
          "0a"
          "0f"
          "0n"
          "0y"
          "0I"
          "0A"
          "0F"
          "0N"
          "0Y"
          "0,"

          "()"
          "(1)"
          "#x10"

          "0i0"
          "0a0"
          "0n0"
          "0y0"
          "0I0"
          "0A0"
          "0N0"
          "0Y0"
          "0,0"

          ;; valid Lisp numbers
          "0f0"
          "0d0"
          "0F0"
          "0D0"

          "00:"
          ":00"
          ":key"
          "nil:symbol"))
  nil)

(deftest test-read-number-unsigned1 ()

  (test-number 0 "-0" :unsigned-p t)
  (test-number 0 "-000" :unsigned-p t)
  (test-number 0 "0" :unsigned-p t)
  (test-number 1 "1" :unsigned-p t)

  (test-number 0 "+0" :unsigned-p t)
  (test-number 1 "+1" :unsigned-p t)

  (test-number 0 "+0." :unsigned-p t)
  (test-number 1 "+1." :unsigned-p t)

  (test-number nil "-1" :unsigned-p t)

  (test-number 0.0d0 "-000.0" :unsigned-p t)
  (test-number 0.0d0 "-0.0" :unsigned-p t)
  (test-number 0.0d0 "-.0" :unsigned-p t)

  (test-number nil "-000.01" :unsigned-p t)
  (test-number nil "-0.01" :unsigned-p t)
  (test-number nil "-.01" :unsigned-p t))


(deftest test-read-number2 ()
  (test-number 0.0d0 "000.0")
  (test-number 0.0d0 "0.0")
  (test-number 0.0d0 ".0")

  (test-number 0.0d0 "-000.0")
  (test-number 0.0d0 "-0.0")
  (test-number 0.0d0 "-.0")

  (test-number 0.0d0 ".0E201")

  (test-number -0.01d0 "-000.01")
  (test-number -0.01d0 "-0.01")
  (test-number -0.01d0 "-.01")

  (test-number 1.d201 "1.0E201")
  (test-number 1.d-201 "1.0E-201")

  (test-number 0.1d10 "0.1E10")
  (test-number 0.1d-10 "0.1E-10")

  (test-number 0.1d10 ".1e10")
  (test-number 0.1d-10 ".1e-10")

  (test-number 20.1d10 "20.1E10")
  (test-number 20.1d-10 "20.1E-10")

  (test-number 1.d201 "1.0e201")
  (test-number 1.d-201 "1.0e-201")

  (test-number -1.d201 "-1.0E201")
  (test-number -1.d-201 "-1.0E-201")

  (test-number 255 "0xFF")
  (test-number -255 "-0xFF")

  (test-number #x012FF "0x012FF")
  (test-number #x-012FF "-0x012FF")

  (test-number 255 "0xff")
  (test-number -255 "-0xff"))

(deftest test-read-double ()
  (test-number double-float-positive-infinity "inf")
  (test-number double-float-negative-infinity "-inf")
  (test-number double-float-not-a-number "nan")
  (test-number double-float-not-a-number "-Nan")
  (test-number double-float-not-a-number "-NAN")
  (test-number double-float-not-a-number "-nan")

  (test-number 1.d21 "1.0E21")
  (test-number 1.d-21 "1.0E-21")

  (test-number 1.d21 "1.0e21")
  (test-number 1.d-21 "1.0e-21")

  (test-number -1.d21 "-1.0E21")
  (test-number -1.d-21 "-1.0E-21"))

(defun test-float (expected string)
  (let ((actual (read-float-from-string string)))
    (expect (equal expected actual))))

(deftest test-read-float ()
  (test-float single-float-positive-infinity "inf")
  (test-float single-float-negative-infinity "-inf")
  (test-float single-float-not-a-number "nan")
  (test-float single-float-not-a-number "-nan")
  (test-float single-float-not-a-number "-NAN")
  (test-float single-float-not-a-number "-Nan")

  (test-float 0.0 "-000.0")
  (test-float 0.0 "-0.0")
  (test-float 0.0 "-.0")

  (test-float -0.01 "-000.01")
  (test-float -0.01 "-0.01")
  (test-float -0.01 "-.01")

  (test-float 1.e21 "1.0E21")
  (test-float 1.e-21 "1.0E-21")

  (test-float 1.e21 "1.0e21")
  (test-float 1.e-21 "1.0e-21")

  (test-float -1.e21 "-1.0E21")
  (test-float -1.e-21 "-1.0E-21"))


(defun test-write-float (expected number)
  (let ((actual (write-float number)))
    (expect (equal expected actual))))

(deftest test-write-float1 ()
  (test-write-float "0.01" 0.01)
  (test-write-float "-0.01" -0.01)

  (test-write-float "1000.01" 1000.01)
  (test-write-float "-1000.01" -1000.01)

  (test-write-float "0.001" 1.d-3)
  (test-write-float "9999999.0" (- 1.d+7 1))

  (test-write-float "9.9999E-4" 0.99999d-3)
  (test-write-float "1.0E7" 1.d+7)

  (test-write-float "1.0E21" 1.d21)
  (test-write-float "1.0E-21" 1.d-21)

  (test-write-float "-1.0E21" -1.d21)
  (test-write-float "-1.0E-21" -1.d-21))

(deftest test-write-float2 ()

  (expect (equal "0.0" (write-float 0.0)))
  (expect (equal "0.0" (write-float -0.0)))

  (expect (equal "10.0" (write-float 1.d+1)))
  (expect (equal "0.01" (write-float 1.d-2)))

  (expect (equal "-10.0" (write-float -1.d+1)))
  (expect (equal "-0.01" (write-float -1.d-2)))

  (expect (equal "1.0E7" (write-float 1.d+7)))
  (expect (equal "8.0E-4" (write-float 8.d-4)))

  (expect (equal "-1.0E7" (write-float -1.d+7)))
  (expect (equal "-8.0E-4" (write-float -8.d-4)))

  (expect (equal "1.0E201" (write-float 1.d+201)))
  (expect (equal "1.0E-200" (write-float 1.d-200)))

  (expect (equal "-1.0E201" (write-float -1.d+201)))
  (expect (equal "-1.0E-200" (write-float -1.d-200))))

(deftest test-write-nonfinite-floats ()
  (expect (equal "inf" (write-float single-float-positive-infinity)))
  (expect (equal "-inf" (write-float single-float-negative-infinity)))
  (expect (equal "nan" (write-float single-float-not-a-number)))

  (expect (equal "inf" (write-float double-float-positive-infinity)))
  (expect (equal "-inf" (write-float double-float-negative-infinity)))
  (expect (equal "nan" (write-float double-float-not-a-number))))


(deftest test-fixnump ()
  (expect (fixnump 0))
  (expect (fixnump 1))
  (expect (fixnump -1))
  (expect (not (fixnump 1.0)))
  (expect (not (fixnump "A")))
  (expect (not (fixnump '1.0))))

(deftest test-float-equal1 ()
  (expect (float-equal 1.0 1.0s0))
  (expect (float-equal 1.0d0 1.0d0))
  (expect (float-equal 1.0 1.0d0))

  (expect (float-equal -0.0 .0s0))
  (expect (float-equal -0.0d0 .0d0))
  (expect (float-equal -0.0 .0d0))

  (expect (float-equal 1.0 1.0))
  (expect (float-equal 1.0d0 1.0d0))
  (expect (float-equal 1.0 1.0d0))

  (expect (float-equal single-float-positive-infinity
                       single-float-positive-infinity))

  (expect (float-equal double-float-positive-infinity
                       double-float-positive-infinity))

  (expect (float-equal single-float-negative-infinity
                       single-float-negative-infinity))

  (expect (float-equal double-float-negative-infinity
                       double-float-negative-infinity))

  (expect (float-equal single-float-not-a-number
                       single-float-not-a-number))
  (expect (float-equal double-float-not-a-number
                       double-float-not-a-number))
  (expect (float-equal double-float-not-a-number
                       single-float-not-a-number))
  (expect (float-equal double-float-not-a-number
                       single-float-not-a-number)))

(deftest test-float-equal2 ()
  (expect (not (float-equal 0.0 1.0)))
  (expect (not (float-equal 0.0d0 1.0d0)))
  (expect (not (float-equal 0.0 1.0d0)))

  (expect (not (float-equal 0.0 double-float-not-a-number)))
  (expect (not (float-equal single-float-positive-infinity
                            double-float-not-a-number)))
  (expect (not (float-equal double-float-not-a-number
                            single-float-positive-infinity)))
  (expect (not (float-equal double-float-not-a-number
                            double-float-positive-infinity))))



(deftest test-single-float-equal1 ()
  (expect (single-float-equal 1.0 1.0))
  (expect (single-float-equal -0.0 0.0))

  (expect (single-float-equal 1.0 1.0))

  (expect (single-float-equal single-float-positive-infinity
                              single-float-positive-infinity))

  (expect (single-float-equal single-float-negative-infinity
                              single-float-negative-infinity))

  (expect (single-float-equal single-float-not-a-number
                       single-float-not-a-number))
  (expect (single-float-equal single-float-not-a-number
                              single-float-not-a-number)))

(deftest test-single-float-equal2 ()
  (expect (not (single-float-equal 0.0 1.0)))


  (expect (not (single-float-equal 0.0 single-float-not-a-number)))
  (expect (not (single-float-equal single-float-positive-infinity
                                   single-float-not-a-number)))
  (expect (not (single-float-equal single-float-not-a-number
                                   single-float-positive-infinity)))
  (expect (not (single-float-equal single-float-not-a-number
                                   single-float-positive-infinity))))

(deftest test-double-float-equal1 ()
  (expect (double-float-equal 0.0d0 -0.0d0))

  (expect (double-float-equal 1.0d0 1.0d0))

  (expect (double-float-equal double-float-positive-infinity
                              double-float-positive-infinity))

  (expect (double-float-equal double-float-negative-infinity
                              double-float-negative-infinity))

  (expect (double-float-equal double-float-not-a-number
                       double-float-not-a-number))
  (expect (double-float-equal double-float-not-a-number
                              double-float-not-a-number)))

(deftest test-double-float-equal2 ()
  (expect (not (double-float-equal 0.0d0 1.0d0)))

  (expect (not (double-float-equal 0.0d0 1.0d0)))

  (expect (not (double-float-equal 0.0d0 double-float-not-a-number)))
  (expect (not (double-float-equal double-float-positive-infinity
                                   double-float-not-a-number)))
  (expect (not (double-float-equal double-float-not-a-number
                                   double-float-positive-infinity)))
  (expect (not (double-float-equal double-float-not-a-number
                                   double-float-positive-infinity))))
