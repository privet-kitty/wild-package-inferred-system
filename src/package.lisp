(uiop:define-package :wild-package-inferred-system
  (:nicknames :wpis :winfer)
  (:use :cl :uiop :asdf)
  (:import-from :asdf
                #:primary-system-p
                #:*defpackage-forms*
                #:*package-inferred-systems*
                #:around-compile-hook
                #:relative-pathname)
  (:export #:wild-package-inferred-system
           #:empty-wild-system))

(in-package :wild-package-inferred-system)
