;; -*- mode: lisp -*-

#-ASDF3.3
(error "wild-package-inferred-system requires ASDF 3.3 or later. The version of your ASDF is ~A."
       (asdf:asdf-version))

(defsystem "wild-package-inferred-system"
  :version "0.1.1"
  :author "Hugo I."
  :license "MIT license"
  :components ((:module "src"
                :components
                ((:file "package")
                 (:file "internal-utilities")
                 (:file "wild-package-inferred-system"))))
  :description "Introduces wildcards into package-inferred-system"
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
