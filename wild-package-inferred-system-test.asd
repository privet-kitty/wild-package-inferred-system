;; -*- mode: lisp -*-

(defsystem "wild-package-inferred-system-test"
  :depends-on ("wild-package-inferred-system" "fiveam")
  :components ((:module "test"
                :components
                ((:test-file "wild-package-inferred-system"))))
  :description "Test system for wild-package-inferred-system"
  :perform (test-op (o c)
                    (uiop:eval-input "(fiveam:run :wild-packge-inferred-system-suite")))
