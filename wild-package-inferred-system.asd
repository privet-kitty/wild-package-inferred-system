;; -*- mode: lisp -*-

(defsystem "wild-package-inferred-system"
  :version "0.0.1"
  :author "Hugo I."
  :license "MIT license"
  :depends-on ()
  :components ((:module "src"
                :components
                ((:file "package")
                 (:file "system-utilities")
                 (:file "wild-package-inferred-system"))))
  :description ""
  :long-description
  #.(read-file-string
     (subpathname *load-pathname* "README.md"))
  :in-order-to ((test-op (test-op "wild-package-inferred-system-test"))))
