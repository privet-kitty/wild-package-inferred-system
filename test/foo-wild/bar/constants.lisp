(defpackage :foo-wild/bar/constants
  (:use :cl)
  (:export #:two-pi))

(in-package :foo-wild/bar/constants)

(defconstant TWO-PI (+ PI PI))
