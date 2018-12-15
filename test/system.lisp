;;;
;;; Test of example systems
;;;

(in-package :wild-package-inferred-system/test)

(defun register-directory (dir)
  (pushnew dir asdf:*central-registry* :test #'equal))

(defun set-equal (list1 list2 &key key (test #'eql))
  (and (subsetp list1 list2 :key key :test test)
       (subsetp list2 list1 :key key :test test)))

(in-suite :wild-package-inferred-system-suite)

(let ((list nil))
  (defun collect-system (system)
    (pushnew system list)
    nil)
  (defun reset-collected-systems ()
    (setf list nil))
  (defun doubly-collected-system-exists-p ()
    (/= (length list)
        (length (remove-duplicates list :test #'equal)))))

(test foo-wild
  ;; initialize
  (register-directory (asdf:system-relative-pathname "wild-package-inferred-system" "test/foo-wild/"))
  (uiop:delete-package* :foo-wild/**/*)

  (reset-collected-systems)
  (let ((asdf:*system-definition-search-functions*
          (cons 'collect-system asdf:*system-definition-search-functions*)))
    (finishes (asdf:load-system :foo-wild)))
  ;; Test if *SYSTEM-DEFINITION-SEARCH-FUNCTIONS* are not called twice
  ;; for a wild system during a session.
  (is (not (doubly-collected-system-exists-p)))
  
  (signals empty-wild-system (asdf:locate-system "foo-wild/contains/no/such/*/system"))
  (is (set-equal '(:cl :uiop
                   :foo-wild/bar/macros
                   :foo-wild/bar/constants)
                 (package-use-list :foo-wild/bar/*)
                 :key #'find-package))
  (is (set-equal '(:cl :uiop
                   :foo-wild/bar/macros
                   :foo-wild/bar/constants
                   :foo-wild/bar/deep/functions)
                 (package-use-list :foo-wild/bar/**/*)
                 :key #'find-package))
  ;; FIXME: The example function HELLO is too complicated.
  (is (equal "Hello, world! 2pi equals 6."
             (let ((out (make-string-output-stream))
                   (*standard-input* (make-string-input-stream "t nil")))
               (uiop:symbol-call :foo-wild :hello out)
               (get-output-stream-string out))))
  (is (fboundp (uiop:find-symbol* :my-princ :foo-wild/bar/deep/functions nil)))
  (is (null (uiop:find-symbol* :my-princ :foo-wild/bar/* nil)))
  (is (null (uiop:find-package* :foo-wild/**/* nil)))
  (finishes (asdf:load-system :foo-wild/**/*)))
