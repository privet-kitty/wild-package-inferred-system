# wild-package-inferred-system

[![Build Status](https://api.travis-ci.org/privet-kitty/wild-package-inferred-system.svg?branch=master)](https://travis-ci.org/privet-kitty/wild-package-inferred-system/) **This library is experimental and still in a pre-alpha stage.**

`wild-package-inferred-system` is an extension of ASDF `package-inferred-system` that interprets star `*` and globstar `**` in package (or system) names.

If you are not sure about `package-inferred-system`, see [the section](https://common-lisp.net/project/asdf/asdf/The-package_002dinferred_002dsystem-extension.html) about it in the ASDF manual. In short, `wild-package-inferred-system` is aimed at elimininating the need for `foo/bar/all`-type subsystems, which have been manually written only for `use-reexport`ing other .lisp files in the (sub)directories.

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
(uiop:define-package :foo-wild/baz/hello
  (:use :cl :foo-wild/qux/*)
  (:import-from :foo-wild/bar/**/* #:sym1 #:sym2)
  (:export #:hello-world))
```

`*` matches one directory or (if in the end) any .lisp files in the directory. `**` matches zero or more subdirectories. In the above example, the package `:foo-wild/qux/*` corresponds to the UNIX path `foo-wild/qux/*.lisp` and `:foo-wild/bar/**/*` to `foo-wild/bar/**/*.lisp`. (The latter path matches all the recursively reachable .lisp files under `foo-wild/bar/`.) 

Since a wild package is just a standard CL package, you can apply `find-package`, `use-package`, `in-package` etc. to it in the same way as other packages. Likewise you can apply `find-system`, `load-system` or other operations to the corresponding wild system.

<!--
You _can_ use any other combinations of wildcards, e.g. `foo/*/bar` (matching foo/sbcl/bar.lisp, foo/ccl/bar.lisp, ...) or `foo/**/interface/*`. However, I recommend that you think about if you really need such a designation.
-->

## Dependencies
You need **ASDF version 3.3.1** or later.

`wild-package-inferred-system` is tested on the (usually latest vesions of the) following implementations:
- SBCL
- Clozure CL
- Allegro CL
- ABCL
- CLISP
- ECL

The only dependent library is [md5](https://github.com/pmai/md5), which can be installed with quicklisp.

## Installation
```
$ cd ~/common-lisp/ # , ~/quicklisp/local-projects/ or ~/.roswell/local-projects/ etc.
$ git clone https://github.com/privet-kitty/wild-package-inferred-system.git
```

## Mechanism

## FAQ
### How can I let a specific file be excluded from wildcard?
`wild-package-inferred-system` ignores the files whose types are `.nosystem.lisp` or `.script.lisp` even if they match a given wild package.

### How can I make a wild package in REPL?
Just call `asdf:load-system`. Evaluating a form like `(asdf:load-system "foo/**/bar/*")` will make the wild package and register the corresponding system simultaneously (if the system `foo` is `wild-package-inferred-system`, of course).

## Copyright
Copyright (c) 2018 Hugo I.
