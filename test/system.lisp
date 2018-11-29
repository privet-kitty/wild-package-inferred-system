;;
;; Test example systems
;;

(in-package :wild-package-inferred-system/test)

(defun register-directory (dir)
  (pushnew dir asdf:*central-registry* :test #'equal))

(defun set-equal (list1 list2 &key key (test #'eql))
  (and (subsetp list1 list2 :key key :test test)
       (subsetp list2 list1 :key key :test test)))

(in-suite :wild-package-inferred-system-suite)

(register-directory (asdf:system-relative-pathname "wild-package-inferred-system" "test/foo-wild/"))

(test foo-wild
  (finishes (asdf:load-system :foo-wild))
  (signals empty-wild-system (asdf:locate-system "foo-wild/contains/no/such/*/system"))
  (is (set-equal '(:foo-wild/bar/macros
                   :foo-wild/bar/constants)
                 (package-use-list :foo-wild/bar/*)
                 :key #'find-package))
  (is (set-equal '(:foo-wild/bar/macros
                   :foo-wild/bar/constants
                   :foo-wild/bar/deep/functions)
                 (package-use-list :foo-wild/bar/**/*)
                 :key #'find-package))
  (is (equal "Hello, world! 2pi equals 6."
             (let ((out (make-string-output-stream))
                   (*standard-input* (make-string-input-stream "t nil")))
               (uiop:symbol-call :foo-wild :hello out)
               (get-output-stream-string out))))
  (finishes (asdf:load-system :foo-wild/**/*)))
