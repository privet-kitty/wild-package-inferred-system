(uiop:define-package :wild-package-inferred-system
  (:nicknames :wpis)
  (:use :cl :uiop :asdf)
  (:import-from :asdf
                #:basic-load-op
                #:primary-system-p
                #:*defpackage-forms*
                #:*package-inferred-systems*
                #:around-compile-hook
                #:relative-pathname)
  (:export #:wild-package-inferred-system
           #:empty-wild-system
           #:reduce-all-wild-packages))
