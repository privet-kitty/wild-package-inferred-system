;;;
;;; This file largely consists of common code with
;;; asdf/package-inferred-system.lisp
;;; 

(in-package :wild-package-inferred-system)

(defclass wild-package-inferred-system (package-inferred-system)
  ((package-option :initform '((:use :cl)) :initarg :default-package-option :reader default-package-option)
   (non-wild-nickname :initform nil :initarg :add-non-wild-nickname :reader non-wild-nickname-p))
  (:documentation "Is almost the same as ASDF:PACKAGE-INFERRED-SYSTEM, except it can interpret star `*' and globstar `**' in package names.

Package options given to :DEFAULT-PACKAGE-OPTION are merged into auto-generated wild package forms. The default is ((:USE :CL)).

If :ADD-NON-WILD-NICKNAME is true, a nickname is given to each wild package, which is the prefix containing no wildcards: e.g. the nickname of :foo/bar/**/baz/* is :foo/bar. Therefore you cannot make packages with a common prefix (e.g. :foo/bar/**/baz* and :foo/bar/*) if you enable this option."))

(defparameter *system-cache-per-oos* nil)

;; The file system of source repository is assumed to be invariant
;; during an OPERATE.
(defmethod operate :around (operation (component wild-package-inferred-system) &rest keys)
  (declare (ignore keys))
  (let ((*system-cache-per-oos* (make-hash-table :test #'equal)))
    (call-next-method)))

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
  "Returns the name of the SYSTEM providing PACKAGE-NAME, if such exists,
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
             (format s "The system ~S corresponds to the pathname ~S, which no actual CL source files matched."
                     (component-name c)
                     (component-pathname c)))))

(defun gen-wild-package-filename (system)
  "Generates the filename for a given wild system."
  (multiple-value-bind (relative dir file type)
      (split-unix-namestring-directory-components** system)
    (declare (ignore relative type))
    (strcat (reduce (lambda (x y) (strcat x "_SL_" y))
                    dir
                    :key #'string)
            "_SL_"
            (string (if (equalp "*" file) :wild file))
            ".lisp")))

(defun calc-wild-package-directory-pathname (toplevel-system-directory)
  "CL source files for wild systems are put under
ASDF:*USER-CACHE* (in the default configuration of
ASDF-OUTPUT-TRANSLATIONS)."
  (merge-pathnames* (strcat "__WILD_SYSTEM__/")
                    (apply-output-translations toplevel-system-directory)))

(defun gen-reexporting-form (system dependencies &key nickname (default-option '((:use :cl))))
  "Generates the UIOP:DEFINE-PACKAGE form for reexporting."
  (let ((primary (primary-system-name system))
        (option (remove :use-reexport default-option :key #'car)) ; except use-reexport
        (use-reexported-packages (cdr (find :use-reexport default-option :key #'car))))
    `(define-package ,(make-keyword system)
         ,@(when nickname
             `((:nicknames ,(make-keyword nickname))))
       ,@option
       (:use-reexport
        ,@(remove-duplicates
           (append
            use-reexported-packages
            (mapcar #'make-keyword
                    (remove-if (lambda (dependent-system)
                                 (not (equal primary (primary-system-name dependent-system))))
                               dependencies))))))))

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
  "wild-package-inferred-system ignores the file names beginning with
dot `.' and file types .nosystem.lisp and .script.lisp even if they
match a given wildcard."
  (let ((name (pathname-name pathname)))
    (or (not (stringp name))
        (char= #\. (char name 0))
        (let ((second-type (nth-value 1 (split-name-type name))))
          (or (equal second-type "script")
              (equal second-type "nosystem"))))))

(defun extract-non-wild-prefix (system)
  "foo/bar/* to foo/bar; foo/**/baz/*/* to foo."
  (check-type system string)
  (assert (not (zerop (length system))))
  (assert (not (char= #\* (char system 0))))
  (loop for pos from 0 below (length system)
        when (and (char= #\* (char system pos))
                  (char= #\/ (char system (- pos 1))))
          do (loop-finish)
        finally (return (subseq system 0 (- pos 1)))))

;; sysdef search function to push into *system-definition-search-functions*
(defun sysdef-wild-package-inferred-system-search (system)
  (or (and *system-cache-per-oos* ; Returns the cached system during an ASDF:OOS
           (gethash system *system-cache-per-oos*))
      (let ((primary (primary-system-name system)))
        (unless (equal primary system)
          (let ((top (find-system primary nil)))
            (when (typep top 'wild-package-inferred-system)
              (if-let (dir (component-pathname top))
                (let* ((sub (subseq system (1+ (length primary))))
                       (path (subpathname** dir sub :type "lisp")))
                  ;; Leaves it to package-inferred-system if it contains no wildcard.
                  (when (wild-pathname-p path)
                    (let ((files (delete-if #'excluded-source-pathname-p (directory* path))))
                      (unless files
                        (warn (make-condition 'empty-wild-system :name system :pathname path)))
                      (let* ((dependencies (mapcar (lambda (path)
                                                     (pathname-to-package-name path top))
                                                   files))
                             (previous (registered-system system))
                             (around-compile (around-compile-hook top))
                             (translated-dir (calc-wild-package-directory-pathname dir))
                             (dest-filename (gen-wild-package-filename system))
                             (translated-path (merge-pathnames* dest-filename translated-dir))
                             (nickname (when (non-wild-nickname-p top)
                                         (make-keyword (extract-non-wild-prefix system)))))
                        (if (same-wild-package-inferred-system-p previous system dir translated-path around-compile dependencies)
                            (when *system-cache-per-oos*
                              (setf (gethash system *system-cache-per-oos*) previous))
                            (let ((new-form (gen-reexporting-form
                                             system
                                             dependencies
                                             :nickname nickname
                                             :default-option (default-package-option top))))
                              (ensure-directories-exist translated-dir)
                              ;; Doesn't touch the source file if the
                              ;; reexporting forms are identical.
                              (unless (and (file-exists-p translated-path)
                                           (equalp new-form (read-file-form translated-path)))
                                (with-output-file (out translated-path :if-exists :supersede)
                                  (writeln new-form :stream out)))
                              (let ((new-system (eval
                                                 `(defsystem ,system
                                                    :class wild-package-inferred-system
                                                    :source-file ,(system-source-file top)
                                                    :pathname ,dir
                                                    :depends-on ,dependencies
                                                    :around-compile ,around-compile
                                                    :components ((cl-source-file "lisp" :pathname ,translated-path))))))
                                (when *system-cache-per-oos*
                                  (setf (gethash system *system-cache-per-oos*) new-system))
                                new-system))))))))))))))

(pushnew 'sysdef-wild-package-inferred-system-search *system-definition-search-functions*)

(defun reduce-all-wild-packages (primary-name &optional (delete t))
  "Reduces all wild packages beginning with
PRIMARY-NAME. (experimental)"
  (unless (primary-system-p primary-name)
    (error "~S must be primary name." primary-name))
  (let ((primary-name (standard-case-symbol-name primary-name)))
    (dolist (p (list-all-packages))
      (let ((name (package-name p)))
        (when (and (equal primary-name (primary-system-name name)) ; FIXME: primary-system-name will be inappropriate because NAME is a package name
                   (wild-pathname-p (parse-unix-namestring** name)))
          (reduce-package p)
          (when delete
            (delete-package* p)))))))
