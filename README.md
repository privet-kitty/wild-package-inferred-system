# wild-package-inferred-system

[![Build Status](https://api.travis-ci.org/privet-kitty/wild-package-inferred-system.svg?branch=master)](https://travis-ci.org/privet-kitty/wild-package-inferred-system/) **This library is experimental and still in a pre-alpha stage.**

`wild-package-inferred-system` is an extension of ASDF `package-inferred-system` that interprets star `*` and globstar `**` in package names.

If you are not sure about `package-inferred-system`, see [the section](https://common-lisp.net/project/asdf/asdf/The-package_002dinferred_002dsystem-extension.html) about it in the ASDF manual. In short, `wild-package-inferred-system` is aimed at elimininating the need for `foo/bar/all`-type subsystems, which are manually written only for `use-reexport`ing other .lisp files in the (sub)directories .

## Usage
You need to specify the `:defsystem-depends-on` and `class` options to use `wild-package-inferred-system` as follows:

```lisp
;; foo-wild/foo-wild.asd
(defsystem "foo-wild"
  :defsystem-depends-on ("wild-package-inferred-system")
  :class "wpis:wild-package-inferred-system"
  :depends-on ("foo-wild/baz/*"))
```

Each source file in the system `foo-wild` will begin with `defpackage` or `uiop:define-package` in the same way as `package-inferred-system`:

```lisp
;; foo-wild/baz/hello.lisp
(defpackage :foo-wild/baz/hello
  (:use :cl :foo-wild/qux/*)
  (:import-from :foo-wild/bar/**/*)
  (:export #:hello-world))
```

`*` matches one directory or (if in the end) all .lisp files in the directory. `**` matches zero or more subdirectories. In the above example, the package `:foo-wild/qux/*` corresponds to the UNIX path `foo-wild/qux/*.lisp` and `:foo-wild/bar/**/*` to `foo-wild/bar/**/*.lisp`. (The latter path matches all the recursively reachable .lisp files under `foo-wild/bar`.) 
<!--
You _can_ use any other combinations of wildcards, e.g. `foo/*/bar` (matching foo/sbcl/bar.lisp, foo/ccl/bar.lisp, ...) or `foo/**/interface/*`. However, I recommend that you think about if you really need such a designation.
-->

## Dependencies
`wild-package-inferred-system` is tested on the (usually latest vesions of the) following implementations:
- SBCL
- Clozure CL
- Allegro CL
- ABCL
- CLISP
- ECL

The only dependent system is [md5](https://github.com/pmai/md5), which can be installed with quicklisp.

## Installation
```
$ cd ~/common-lisp/ # , ~/quicklisp/local-projects/ or ~/.roswell/local-projects/ etc.
$ git clone https://github.com/privet-kitty/wild-package-inferred-system.git
```

## Mechanism

## FAQ
### How can I let a specific file be excluded from wildcard?
`wild-package-inferred-system` ignores the files of the type `.nosystem.lisp` and `.script.lisp` even if they match a given wild package.

## Copyright
Copyright (c) 2018 Hugo I.
