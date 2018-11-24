(uiop:define-package :wild-package-inferred-system
    (:nicknames :wpis)
  (:use :cl :uiop :asdf/upgrade :asdf/session :asdf/operation
        :asdf/component :asdf/system :asdf/system-registry :asdf/lisp-action
        :asdf/parse-defsystem :asdf/package-inferred-system
        :asdf/output-translations :asdf/session :asdf/operate)
  (:shadow #:split-unix-namestring-directory-components
           #:parse-unix-namestring
           #:split-name-type
           #:subpathname)
  (:export #:wild-package-inferred-system
           #:empty-wild-system))
