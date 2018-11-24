# wild-package-inferred-system

**This library is experimental and still in a pre-alpha stage.**

`wild-package-inferred-system` is an extension of ASDF `package-inferred-system` that interprets star `*` and globstar `**` in package names.

## Usage

```lisp
;; foo-wild.asd
(defsystem "foo-wild"
  :defsystem-depends-on (:wild-package-inferred-system)
  :class "wpis:wild-package-inferred-system"
  :depends-on ("foo-wild/interface" "foo-wild/bar/*" "foo-wild/baz/*"))
```

`*` matches one directory or (if in the end) all .lisp files in the directory. `**` matches zero or more subdirectories.

```
;; foo-wild/baz/hello.lisp
(defpackage :foo-wild/baz/hello
  (:use :cl :foo-wild/bar/**/* :foo-wild/qux/*)
  (:export #:hello))
```

## Installation
