(uiop:define-package :foo-wild/baz/bye
  (:use :cl :foo-wild/bar/**/*)
  (:export #:bye))

(in-package :foo-wild/baz/bye)

(defun bye (&optional (out *standard-output*))
  (while (read)
    (my-princ "Goodbye, world! " out)
    (format out "2pi equals ~A." (round two-pi))))
