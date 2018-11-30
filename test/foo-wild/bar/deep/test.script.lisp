;; This file should be excluded from wild-package-inferred-system

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "asdf"))

(print asdf:*central-registry*)
