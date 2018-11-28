(uiop:define-package :foo-wild/baz/hello
  (:use :cl :foo-wild/bar/**/*)
  (:export #:hello))

(in-package :foo-wild/baz/hello)

(defun hello (&optional (out *standard-output*))
  (while (read)
    (my-princ "Hello, world! " out)
    (format out "2pi equals ~A." two-pi)))
