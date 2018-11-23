;;
;; This file largely consists of common code with
;; asdf/package-inferred-system.lisp
;; 

(in-package :wild-package-inferred-system)

(defun initial-package-inferred-systems-table ()
  ;; Mark all existing packages are preloaded.
  (let ((h (make-hash-table :test 'equal)))
    (dolist (p (list-all-packages))
      (dolist (n (package-names p))
        (setf (gethash n h) t)))
    h))

(defclass wild-package-inferred-system (package-inferred-system)
  ()
  (:documentation "Is almost same as ASDF:PACKAGE-INFERRED-SYSTEM though this class can interpret star `*' and globstar `**' in package names."))

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

(defun package-dependencies (defpackage-form)
  "Return a list of packages depended on by the package
defined in DEFPACKAGE-FORM.  A package is depended upon if
the DEFPACKAGE-FORM uses it or imports a symbol from it."
  (assert (defpackage-form-p defpackage-form))
  (remove-duplicates
   (while-collecting (dep)
     (loop* :for (option . arguments) :in (cddr defpackage-form) :do
            (ecase option
              ((:use :mix :reexport :use-reexport :mix-reexport)
               (dolist (p arguments) (dep (string p))))
              ((:import-from :shadowing-import-from)
               (dep (string (first arguments))))
              ((:nicknames :documentation :shadow :export :intern :unintern :recycle)))))
   :from-end t :test 'equal))

(defun package-designator-name (package)
  "Normalize a package designator to a string"
  (etypecase package
    (package (package-name package))
    (string package)
    (symbol (string package))))

(defun register-system-packages (system packages)
  "Register SYSTEM as providing PACKAGES."
  (let ((name (or (eq system t) (coerce-name system))))
    (dolist (p (ensure-list packages))
      (setf (gethash (package-designator-name p) *package-inferred-systems*) name))))

(defun package-name-system (package-name)
  "Return the name of the SYSTEM providing PACKAGE-NAME, if such exists,
otherwise return a default system name computed from PACKAGE-NAME."
  (check-type package-name string)
  (or (gethash package-name *package-inferred-systems*)
      (string-downcase package-name)))

;; Given a file in package-inferred-system style, find its dependencies
(defun package-inferred-system-file-dependencies (file &optional system)
  (if-let (defpackage-form (file-defpackage-form file))
    (remove t (mapcar 'package-name-system (package-dependencies defpackage-form)))
    (error 'package-inferred-system-missing-package-error :system system :pathname file)))

(defun package-inferred-system-files-dependencies (files &optional system)
  (delete-duplicates
   (loop for file in files
         append (package-inferred-system-file-dependencies file system))
   :test #'equal))

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
                     (equal (component-name child) "wild")
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
                    (strcat x "_SLASH_" y))
                  (nth-value 1 (split-unix-namestring-directory-components system :interpret-wild t))
                  :key #'string)
          ".lisp"))

(defun calc-wild-package-directory-pathname (toplevel-system-directory)
  (merge-pathnames* (strcat "__WILD_SYSTEM__/")
                    (apply-output-translations toplevel-system-directory)))


(defun generate-reexporting-form (system dependencies)
  "Generates the UIOP:DEFINE-PACKAGE form for reexporting."
  `(define-package ,(intern (standard-case-symbol-name system) :keyword)
       (:use :cl)
     (:use-reexport ,@(mapcar (lambda (dependent-system)
                                (intern (standard-case-symbol-name dependent-system) :keyword))
                              (remove-if #'primary-system-p dependencies)))))

(defun relative-pathname (pathname base &key (test #'null))
  (loop for rest = (pathname-directory pathname) then (cdr rest)
        for base-rest = (pathname-directory base) then (cdr base-rest)
        when (funcall test base-rest)
          do (return (make-pathname :host (pathname-host pathname)
                                    :device (pathname-device pathname)
                                    :name (pathname-name pathname)
                                    :type (pathname-type pathname)
                                    :version (pathname-version pathname)
                                    :directory (cons :relative rest)))
        unless (equal (car rest) (car base-rest))
          do (error "Huh?")))

(defun pathname-to-package-name (pathname toplevel-pathname)
  (let ((relative (relative-pathname pathname toplevel-pathname :test (lambda (x) (null (cdr x))))))
    (unix-namestring (make-pathname :directory (pathname-directory relative)
                                    :type nil
                                    :name (pathname-name relative)))))

;; sysdef search function to push into *system-definition-search-functions*
(defun sysdef-wild-package-inferred-system-search (system)
  (let ((primary (primary-system-name system)))
    (unless (equal primary system)
      (let ((top (find-system primary nil)))
        (when (typep top 'wild-package-inferred-system)
          (if-let (dir (component-pathname top))
            (let* ((sub (subseq system (1+ (length primary))))
                   (path (subpathname dir sub :type "lisp")))
              (when (wild-pathname-p path)
                (let ((files (directory* path)))
                  (unless files
                    (warn (make-condition 'empty-wild-system :name system :pathname path)))
                  (let* ((dependencies (mapcar (lambda (path) (pathname-to-package-name path dir))
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
                          (with-output-file (out translated-path
                                                 :if-exists :supersede
                                                 :if-does-not-exist :create)
                            (writeln (generate-reexporting-form system dependencies)
                                     :stream out))
                          (eval `(defsystem ,system
                                   :class wild-package-inferred-system
                                   :source-file ,(system-source-file top)
                                   :pathname ,dir
                                   :depends-on ,dependencies
                                   :around-compile ,around-compile
                                   :components ((cl-source-file "wild" :pathname ,translated-path))))))))))))))))

(pushnew 'sysdef-wild-package-inferred-system-search *system-definition-search-functions*)
