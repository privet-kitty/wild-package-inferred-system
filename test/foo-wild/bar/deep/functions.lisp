(uiop:define-package :foo-wild/bar/deep/functions
  (:use :cl :foo-wild/bar/*)
  (:export #:my-princ))

(in-package :foo-wild/bar/deep/functions)

(defun my-princ (str &optional (out *standard-output*))
  (princ str out))
