# Lisp core libraries

![SBCL-Tests](https://github.com/qitab/ace.core/workflows/SBCL-Tests/badge.svg)
![CCL-Tests](https://github.com/qitab/ace.core/workflows/CCL-Tests/badge.svg)
[![Gitter](https://badges.gitter.im/qitab/community.svg)](https://gitter.im/qitab/community?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge)

This is a library containing utilities for working with Common Lisp.

## The ACE.CORE library contains the following modules:

### ace.core

A summary package that can be used as a namespace.

### macro

A set of utilities for writing macros.

### type

Utilities for dealing with types and declarations.

### symbol

Utilities dealing with symbols.

### collect

The functions: `With-collectors`, `with-collected-values`.

### etc

Various utilities that don't belong anywhere else.

### assert

The functions: `check`, `dcheck`, `expect`, `assert*`.

### defun*

A DEFUN like macro with easy interface and type declaration.

### thread

Utilities dealing with threads and concurrency.

### package

Helpers to defpackage, namespaces, and package aliases.

### number

Utilities related to numbers, parsing and writing C++/Java numbers.

### string

Uilities related to character strings.

### vector

Utilities for dealing with vectors.

### list

Utilities for dealing with lists.

### parse

Parsing of dynamic type values from C++/Java representation.

### fast-ops

A set of macros allowing to define optimized typed operators.

### fixnum-ops

The unholy set of fixnum optimized operations.

### index-ops

A set of operations acting on array indexes.

### switch

A family of `case`-like `switch` macros.


### Disclaimer: This is not an official Google product.
