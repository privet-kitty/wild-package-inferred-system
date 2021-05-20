# wild-package-inferred-system

[![Build Status](https://github.com/privet-kitty/wild-package-inferred-system/workflows/CI/badge.svg)](https://github.com/privet-kitty/wild-package-inferred-system/actions) **This library is still in a alpha stage.**

`wild-package-inferred-system` is an extension of ASDF `package-inferred-system` that interprets star `*` and globstar `**` in package (or system) names.

If you are not sure about `package-inferred-system`, see [the section](https://common-lisp.net/project/asdf/asdf/The-package_002dinferred_002dsystem-extension.html) about it in the ASDF manual. In short, `wild-package-inferred-system` is aimed at elimininating the need for `foo/.../all`-type subsystems, which have been manually written only for `use-reexport`ing other .lisp files in the (sub)directories.

## Usage
You need to specify the `:defsystem-depends-on` and `:class` options to use `wild-package-inferred-system` as follows:

```lisp
;; foo-wild/foo-wild.asd
(defsystem "foo-wild"
  :defsystem-depends-on ("wild-package-inferred-system")
  :class "winfer:wild-package-inferred-system"
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

The only difference is that `wild-package-inferred-system` can interpret wildcards in a package name: `*` matches one directory or (if in the end) any .lisp files in the directory; `**` matches zero or more subdirectories. In the above example, the package `:foo-wild/qux/*` corresponds to the unix path `foo-wild/qux/*.lisp` and `:foo-wild/bar/**/*` to `foo-wild/bar/**/*.lisp`. (The latter path matches all the recursively reachable .lisp files under `foo-wild/bar/`.) 

Since a wild package is just a standard CL package, you can apply `find-package`, `use-package`, `in-package` etc. to it if once generated. Likewise you can apply `find-system`, `load-system` or other operations to the corresponding wild system.

You _can_ use any other combinations of wildcards, e.g. `foo/*/bar` or `foo/**/*/interface/*`(, though I recommend that you think about if you really need such a complicated desigination).

## Dependencies
**ASDF version 3.3** or later is required.

`wild-package-inferred-system` is tested on the (usually latest vesions of the) following implementations:
- SBCL
- Clozure CL
- Allegro CL
- ABCL

## Installation
`wild-package-inferred-system` will be automatically fetched and loaded via quicklisp. If you want to install it from this repository, just run:

```
$ cd ~/common-lisp/ # , ~/quicklisp/local-projects/, ~/.roswell/local-projects/ etc.
$ git clone https://github.com/privet-kitty/wild-package-inferred-system.git
```

## Mechanism
To be edited

## FAQ
### How can I let a specific file be excluded from wildcard?
`wild-package-inferred-system` ignores the files whose names begin with dot `.` or whose types are `.nosystem.lisp` or `.script.lisp` even if they match a given wild package.

### How can I make a wild package in REPL?
Just call `asdf:load-system`. Evaluating a form like `(asdf:load-system "foo/bar/**/*")` will make the wild package and register the corresponding system simultaneously (only if the system `foo` is `wild-package-inferred-system`, of course).

### Can I use the wildcard `*` as a part of file or directory name like `foo/bar-*-*/*baz`?
No. See [the issue](https://github.com/privet-kitty/wild-package-inferred-system/issues/1).

### How can I nickname a wild package?
Of course you may manually give arbitrary nicknames to a wild package by e.g. `(uiop:ensure-package :foo/bar/* :nicknames '(:foo/bar))`.

In addition, `wild-package-inferred-system` provides an option to nickname a wild package automatically. If `:add-non-wild-nickname t` is specified in `defsystem` form, a nickname is given to each wild package, which is the prefix containing no wildcards: e.g. the nickname of `:foo/bar/**/baz/*` is `:foo/bar`. Therefore you cannot make two packages with a common prefix (e.g. `:foo/bar/**/*` and `:foo/bar/*`) if you enable this option.

## Copyright
Copyright (c) 2018 Hugo I.
