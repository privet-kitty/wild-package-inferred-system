(in-package :wild-package-inferred-system/test)

(in-suite :wild-package-inferred-system-suite)

(test generate-reexporting-form
  (is (equalp '(uiop:define-package :foo/**/*
                (:use)
                (:use-reexport :foo/bar :foo/bar/baz))
              (wpis::generate-reexporting-form "foo/**/*" '("foo/bar" "foo/bar/baz"))))
  (is (equalp '(uiop:define-package :foo/* (:use) (:use-reexport))
              (wpis::generate-reexporting-form "foo/*" nil))))
