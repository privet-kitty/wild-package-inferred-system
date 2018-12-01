(in-package :wild-package-inferred-system/test)

(in-suite :wild-package-inferred-system-suite)

(test gen-reexporting-form
  (is (equalp '(uiop:define-package :foo/**/*
                (:nicknames :foo)
                (:use :cl :uiop)
                (:use-reexport :cl-ppcre :foo/bar :foo/bar/baz))
              (wpis::gen-reexporting-form "foo/**/*"
                                          '("foo/bar" "foo/bar/baz")
                                          :default-option '((:use :cl :uiop)
                                                            (:use-reexport :cl-ppcre))
                                          :non-wild-nickname t)))
  (is (equalp '(uiop:define-package :foo/*
                (:use :cl)
                (:use-reexport))
              (wpis::gen-reexporting-form "foo/*" nil))))

(test excluded-source-pathname-p
  (is (wpis::excluded-source-pathname-p #P"foo/bar.script.lisp"))
  (is (wpis::excluded-source-pathname-p #P"/foo/bar/baz.nosystem.lisp"))
  (is (not (wpis::excluded-source-pathname-p #P"foo/bar.lisp"))))

(test gen-wild-package-filename
  (is (equal "foo_SL_WILD-INFERIORS_SL_bar_SL_WILD.lisp"
             (wpis::gen-wild-package-filename "foo/**/bar/*"))))
