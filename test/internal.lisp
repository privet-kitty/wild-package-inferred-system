(in-package :wild-package-inferred-system/test)

(in-suite :wild-package-inferred-system-suite)

(test generate-reexporting-form
  (is (equalp '(uiop:define-package :foo/**/*
                (:use :cl :uiop)
                (:use-reexport :alexandria :foo/bar :foo/bar/baz))
              (wpis::generate-reexporting-form "foo/**/*"
                                               '("foo/bar" "foo/bar/baz")
                                               '((:use :cl :uiop)
                                                 (:use-reexport :alexandria)))))
  (is (equalp '(uiop:define-package :foo/*
                (:use :cl)
                (:use-reexport))
              (wpis::generate-reexporting-form "foo/*" nil))))

(test excluded-source-pathname-p
  (is (wpis::excluded-source-pathname-p #P"foo/bar.script.lisp"))
  (is (wpis::excluded-source-pathname-p #P"/foo/bar/baz.nosystem.lisp")))
