;; -*- coding: utf-8; mode: lisp -*-

(defsystem "foo-wild"
  :license "Public domain"
  :defsystem-depends-on (:wild-package-inferred-system)
  :class "wpis:wild-package-inferred-system"
  :default-package-option ((:use :cl) (:use-reexport :uiop))
  :depends-on ("foo-wild/interface" "foo-wild/bar/*" "foo-wild/baz/*"))

