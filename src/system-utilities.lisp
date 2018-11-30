;;;;
;;;; Internally used utilities
;;;;

(in-package :wild-package-inferred-system)

(defun split-unix-namestring-directory-components**
      (unix-namestring &key ensure-directory dot-dot)
  "Is almost same as UIOP:SPLIT-UNIX-NAMESTRING-DIRECTORY-COMPONENTS
but interprets star `*' and globstar `**'."
    (check-type unix-namestring string)
    (check-type dot-dot (member nil :back :up))
    (if (and (not (find #\/ unix-namestring)) (not ensure-directory)
             (plusp (length unix-namestring)))
        (values :relative () unix-namestring t)
        (let* ((components (split-string unix-namestring :separator "/"))
               (last-comp (car (last components))))
          (multiple-value-bind (relative components)
              (if (equal (first components) "")
                  (if (equal (first-char unix-namestring) #\/)
                      (values :absolute (cdr components))
                      (values :relative nil))
                  (values :relative components))
            (setf components (remove-if #'(lambda (x) (member x '("" ".") :test #'equal))
                                        components))
            (setf components (substitute (or dot-dot :back) ".." components :test #'equal))
            (setf components (substitute :wild-inferiors "**" components :test #'equal))
            (setf components (substitute :wild "*" components :test #'equal))
            (cond
              ((equal last-comp "")
               (values relative components nil nil)) ; "" already removed from components
              (ensure-directory
               (values relative components nil nil))
              (t
               (values relative (butlast components) last-comp nil)))))))


(defun split-name-type** (filename)
  "Is almost same as UIOP:SPLIT-NAME-TYPE but interprets star `*'."
  (check-type filename string)
  (assert (plusp (length filename)))
  (destructuring-bind (name &optional (type *unspecific-pathname-type*))
      (split-string filename :max 2 :separator ".")
    (when (equal name "*")
      (setf name :wild))
    (when (equal type "*")
      (setf type :wild))
    (if (equal name "")
        (values filename *unspecific-pathname-type*)
        (values name type))))

(defun parse-unix-namestring** (name &rest keys &key type defaults dot-dot ensure-directory &allow-other-keys)
  "Is almost same as UIOP:PARSE-UNIX-NAMESTRING but interprets star
`*' and globstar `**'."
  (block nil
    (check-type type (or null string (eql :directory)))
    (when ensure-directory
      (setf type :directory))
    (etypecase name
      ((or null pathname) (return name))
      (symbol
       (setf name (string-downcase name)))
      (string))
    (multiple-value-bind (relative path filename file-only)
        (split-unix-namestring-directory-components**
         name :dot-dot dot-dot :ensure-directory (eq type :directory))
      (multiple-value-bind (name type)
          (cond
            ((or (eq type :directory) (null filename))
             (values nil nil))
            (type
             (values (split-name-type** filename) type))
            (t
             (split-name-type** filename)))
        (apply 'ensure-pathname
               (make-pathname
                :directory (unless file-only (cons relative path))
                :name name :type type
                :defaults (or #-mcl defaults *nil-pathname*))
               (remove-plist-keys '(:type :dot-dot :defaults) keys))))))

(defun subpathname** (pathname subpath &key type)
  "Is almost same as UIOP:SUBPATHNAME but interprets star `*' and `**'
globstar."
  (or (and (pathnamep subpath) (absolute-pathname-p subpath))
      (merge-pathnames* (parse-unix-namestring** subpath :type type :want-relative t)
                        (pathname-directory-pathname pathname))))


;; (defun calc-md5-signature (string &optional (byte 16))
;;   (let ((signature (make-array (+ byte byte) :element-type 'base-char))
;;         (md5sum (md5:md5sum-string string))
;;         (dict #(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\a #\b #\c #\d #\e #\f)))
;;     (dotimes (idx byte signature)
;;       (let ((units (logand (aref md5sum idx) #x0F))
;;             (tens-place (ash (logand (aref md5sum idx) #xF0) -4))
;;             (sig-idx (+ idx idx)))
;;         (setf (aref signature sig-idx) (aref dict tens-place))
;;         (setf (aref signature (1+ sig-idx)) (aref dict units))))))

(defun reduce-package (package-designator)
  "Reduces the package from the graph of user-usee relationship: for
example, (A B) using C using (D E F) is transformed to A using (D E F)
and B using (D E F) if package C is reduced. "
  (let ((using-list (package-used-by-list package-designator))
        (used-list (package-use-list package-designator)))
    (dolist (using using-list)
      (unuse-package package-designator using)
      (dolist (used used-list)
        (use-package used using)))))
