(defpackage :foo-wild/bar/macros
  (:use :cl)
  (:export #:while))

(in-package :foo-wild/bar/macros)

(defmacro while (test &body body)
  `(loop while ,test do ,@body))
