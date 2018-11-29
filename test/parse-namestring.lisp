;;
;; Portability test of (PARSE-NAMESTRING <WILD-PATHNAME>).
;;
;; (Currently parse-namestring is not used in
;; wild-package-inferred-system. It is for future.)
;;

(in-package :wild-package-inferred-system/test)

(in-suite :wild-package-inferred-system-suite)

(test |* as one directory|
  (is (pathname-match-p #P"/foo/bar/baz" #P"/foo/*/baz"))
  (is (not (pathname-match-p #P"/foo/bar/baz/qux" #P"/foo/*/qux"))))

(test |** as zero or more directory|
  (is (pathname-match-p #P"/foo/bar/baz/qux" #P"/foo/**/qux"))
  (is (pathname-match-p #P"/foo/qux" #P"/foo/**/qux")))

(test |/* at the end means the PATHNAME-NAME is :WILD|
  (is (pathname-match-p #P"/foo/bar/baz" #P"/foo/bar/*"))
  (is (eql :wild (pathname-name #P"/foo/bar/*"))))

(test |/* at the end means the PATHNAME-TYPE is NIL|
  (is (null (pathname-type #P"/foo/bar/*"))))

(test |*.* at the end means the PATHNAME-TYPE is :WILD|
  (is (eql :wild (pathname-type #P"/foo/bar/*.*"))))

;; (test |* in a part of directory name|
;;   (is (pathname-match-p #P"/foo/sbcl-bar/baz" #P"/foo/*bar*/baz"))
;;   (is (pathname-match-p #P"/foo/sbcl-bar-1.4.2/baz" #P"/foo/*bar*/baz")))

;; (test |* in a part of file name|
;;   (is (pathname-match-p #P"/foo/bar/bazfeed.lisp" #P"/foo/bar/baz*.lisp")))

;; (test |* in a part of file type|
;;   (is (pathname-match-p #P"/foo/bar/baz.maybe-lispy" #P"/foo/bar/baz.*lisp*")))

;; #+(or sbcl clisp allegro)
;; (test |? in directory name|
;;   (is (pathname-match-p #P"/foo/bar1/baz" #P"/foo/bar?/baz"))
;;   (is (not (pathname-match-p #P"/foo/bar/baz" #P"/foo/bar?/baz"))))
