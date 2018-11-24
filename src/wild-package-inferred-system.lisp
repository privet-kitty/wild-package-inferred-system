;;
;; This file largely consists of common code with
;; asdf/package-inferred-system.lisp
;; 

(in-package :wild-package-inferred-system)

(defclass wild-package-inferred-system (package-inferred-system)
  ((reduce-wild :initform nil))
  (:documentation "Is almost the same as ASDF:PACKAGE-INFERRED-SYSTEM, except it can interpret star `*' and globstar `**' in package names."))

;; Is a given form recognizable as a defpackage form?
(defun defpackage-form-p (form)
  (and (consp form)
       (member (car form) *defpackage-forms*)))

;; Find the first defpackage form in a stream, if any
(defun stream-defpackage-form (stream)
  (loop :for form = (read stream nil nil) :while form
        :when (defpackage-form-p form) :return form))

(defun file-defpackage-form (file)
  "Return the first DEFPACKAGE form in FILE."
  (with-input-file (f file)
    (stream-defpackage-form f)))

(defun package-designator-name (package)
  "Normalize a package designator to a string"
  (etypecase package
    (package (package-name package))
    (string package)
    (symbol (string package))))

(defun package-name-system (package-name)
  "Return the name of the SYSTEM providing PACKAGE-NAME, if such exists,
otherwise return a default system name computed from PACKAGE-NAME."
  (check-type package-name string)
  (or (gethash package-name *package-inferred-systems*)
      (string-downcase package-name)))

;; Given package-inferred-system object, check whether its specification matches
;; the provided parameters
(defun same-wild-package-inferred-system-p (system name directory subpath around-compile dependencies)
  (and (eq (type-of system) 'wild-package-inferred-system)
       (equal (component-name system) name)
       (pathname-equal directory (component-pathname system))
       (equal dependencies (component-sideway-dependencies system))
       (equal around-compile (around-compile-hook system))
       (let ((children (component-children system)))
         (and (length=n-p children 1)
              (let ((child (first children)))
                (and (eq (type-of child) 'cl-source-file)
                     (equal (component-name child) "lisp")
                     (and (slot-boundp child 'relative-pathname)
                          (equal (slot-value child 'relative-pathname) subpath))))))))

(define-condition empty-wild-system (warning)
  ((name :initarg :name :reader component-name)
   (path :initarg :pathname :reader component-pathname))
  (:report (lambda (c s)
             (format s "The system ~S corresponds the pathname ~S, which no actual CL source files matched."
                     (component-name c)
                     (component-pathname c)))))

(defun generate-wild-package-filename (system)
  "Generates the filename for a given wild system."
  (strcat (reduce (lambda (x y)
                    (strcat x "_sl_" y))
                  (nth-value 1 (split-unix-namestring-directory-components system :interpret-wild t))
                  :key #'string)
          "_"
          (calc-md5-signature system 4)
          ".lisp"))

(defun calc-wild-package-directory-pathname (toplevel-system-directory)
  ".lisp files for wild system are put under ASDF:*USER-CACHE* by
ASDF-OUTPUT-TRANSLATIONS (in the default configuration)."
  (merge-pathnames* (strcat "__WILD_SYSTEM__/")
                    (apply-output-translations toplevel-system-directory)))

(defun generate-reexporting-form (system dependencies)
  "Generates the UIOP:DEFINE-PACKAGE form for reexporting."
  (let ((primary (primary-system-name system)))
    `(define-package ,(intern (standard-case-symbol-name system) :keyword)
         (:use :cl)
       (:use-reexport
        ,@(mapcar (lambda (dependent-system)
                    (intern (standard-case-symbol-name dependent-system) :keyword))
                  (remove-if (lambda (dependent-system)
                               (not (equal primary (primary-system-name dependent-system))))
                             dependencies))))))

(define-condition wild-package-inferred-system-illegal-file-pathname (system-definition-error)
  ((path :initarg :pathname :reader error-pathname)
   (base :initarg :base-pathname :reader error-base-pathname))
  (:report (lambda (c s)
             (format s "Couldn't derive the system name of the source file ~S w.r.t. the primary system directory ~S"
                     (error-pathname c)
                     (error-base-pathname c)))))

(defun pathname-to-package-name (pathname primary-system)
  "Derives the package name of CL-SOURCE-FILE at PATHNAME
w.r.t. PRIMARY-SYSTEM."
  (let* ((primary-pathname (component-pathname primary-system))
         (relative
           (loop for rest = (pathname-directory pathname) then (cdr rest)
                 for base-rest = (pathname-directory primary-pathname) then (cdr base-rest)
                 when (null base-rest)
                   do (return (make-pathname :name (pathname-name pathname)
                                             :directory (cons :relative rest)))
                 unless (equal (car rest) (car base-rest))
                   do (error (make-condition 'wild-package-inferred-system-illegal-file-pathname
                                             :pathname pathname
                                             :base-pathname primary-pathname)))))
    (strcat (component-name primary-system)
            "/"
            (unix-namestring (make-pathname :name (pathname-name relative)
                                            :directory (pathname-directory relative)
                                            :type nil)))))

(defun excluded-source-pathname-p (pathname)
  "wild-package-inferred-system ignores the file types .nosystem.lisp
and .script.lisp, even if they match a given wild card."
  (let ((second-type (nth-value 1 (split-name-type (pathname-name pathname)))))
    (or (equal second-type "script")
        (equal second-type "nosystem"))))

;; sysdef search function to push into *system-definition-search-functions*
(defun sysdef-wild-package-inferred-system-search (system)
  (let ((primary (primary-system-name system)))
    (unless (equal primary system)
      (let ((top (find-system primary nil)))
        (when (typep top 'wild-package-inferred-system)
          (if-let (dir (component-pathname top))
            (let* ((sub (subseq system (1+ (length primary))))
                   (path (subpathname dir sub :type "lisp")))
              ;; Leaves it to package-inferred-system, if no wildcard is used.
              (when (wild-pathname-p path)
                (let ((files (delete-if #'excluded-source-pathname-p (directory* path))))
                  (unless files
                    (warn (make-condition 'empty-wild-system :name system :pathname path)))
                  (let* ((dependencies (mapcar (lambda (path) (pathname-to-package-name path top))
                                               files))
                         (previous (registered-system system))
                         (around-compile (around-compile-hook top))
                         (translated-dir (calc-wild-package-directory-pathname dir))
                         (dest-filename (generate-wild-package-filename system))
                         (translated-path (merge-pathnames* dest-filename translated-dir)))
                    (if (same-wild-package-inferred-system-p previous system dir translated-path around-compile dependencies)
                        previous
                        (progn
                          (ensure-directories-exist translated-dir)
                          (with-output-file (out translated-path :if-exists :supersede)
                            (writeln (generate-reexporting-form system dependencies)
                                     :stream out))
                          (eval `(defsystem ,system
                                   :class wild-package-inferred-system
                                   :source-file ,(system-source-file top)
                                   :pathname ,dir
                                   :depends-on ,dependencies
                                   :around-compile ,around-compile
                                   :components ((cl-source-file "lisp" :pathname ,translated-path))))))))))))))))

(pushnew 'sysdef-wild-package-inferred-system-search *system-definition-search-functions*)

(defun reduce-all-wild-packages (primary-name &optional (delete t))
  "Reducts all wild packages beginning with PRIMARY-NAME"
  (unless (primary-system-p primary-name)
    (error "~S must be primary name." primary-name))
  (let ((primary-name (standard-case-symbol-name primary-name)))
    (dolist (p (list-all-packages))
      (let ((name (package-name p)))
        (when (and (equal primary-name (primary-system-name name))
                   (wild-pathname-p (parse-unix-namestring name :interpret-wild t)))
          (reduce-package p)
          (when delete
            (delete-package* p)))))))
