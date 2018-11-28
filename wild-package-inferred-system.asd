;; -*- mode: lisp -*-

(defsystem "wild-package-inferred-system"
  :version "0.0.1"
  :author "Hugo I."
  :license "MIT license"
  :depends-on ("md5")
  :components ((:module "src"
                :components
                ((:file "package")
                 (:file "system-utilities")
                 (:file "wild-package-inferred-system"))))
  :description ""
  :long-description
  #.(read-file-string
     (subpathname *load-pathname* "README.md"))
  :in-order-to ((test-op (test-op "wild-package-inferred-system/test"))))

(defsystem "wild-package-inferred-system/test"
  :depends-on ("wild-package-inferred-system" "fiveam")
  :components ((:module "test"
                :components ((:file "package")
                             (:file "internal")
                             (:file "parse-namestring")
                             (:file "system"))))
  :description "Test system for wild-package-inferred-system"
  :perform (test-op (o c)
                    (uiop:eval-input "(fiveam:run! :wild-package-inferred-system-suite)")))
