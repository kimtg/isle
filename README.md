# Isle ISLISP
Isle ISLISP is an [ISLISP](https://en.wikipedia.org/wiki/ISLISP) Compiler. It is written in Common Lisp ([Steel Bank Common Lisp](https://sbcl.org/)). Because it is implemented in Common Lisp, the source code itself demonstrates the differences between ISLISP and Common Lisp.

# Motivation for implementing in Common Lisp
I was thinking of modifying SBCL to remove features not in ISLISP and add features as appropriate, but SBCL is constantly improving and it is difficult to keep up, so I implemented it as a Common Lisp program in SBCL. Because Isle ISLISP uses SBCL, a top-of-the-line Common Lisp implementation, execution speed is top-notch.

For reference, these days Racket is also implemented in Chez Scheme.

# Goals
* Learn Common Lisp
* Learn how to use macros
* Know the difference between ISLISP and Common Lisp

## Run
Run `isle.sh` or `isle.bat`.
```
Usage: isle [OPTIONS...] [FILE]

OPTIONS:
    -h  print this screen.
    -v  print version.
 If no FILE is specified, the REPL is run.
```

# How to build an executable of Isle ISLISP
Right after starting Isle ISLISP,

Unix-like:
```
(build-exe "isle")
```

Windows:
```
(build-exe "isle.exe")
```

# How to build an executable of your program
Define your `main` function and then save the core image.

Example:
```
(defun main () (format (standard-output) "hello~%"))
(build-exe "hello.exe")
```
See also: [Saving a Core Image - SBCL User Manual](http://www.sbcl.org/manual/#Saving-a-Core-Image)

# How to use features of Common Lisp
You can use Common Lisp symbols by adding the cl: prefix. ISLISP symbols are defined in islisp package.
Example: `(cl:evenp 2)`

# Extended functions
| Function          | Description                                       |
| ----------------- | ------------------------------------------------- |
| (eval *form*)     | evaluates *form*                                  |
| (load *filename*) | loads code from *filename* e.g. (load "foo.lisp") |

# Bugs
[BUGS file](BUGS.md)

# Major differences from Common Lisp
* There is a global lexical variable. (`defglobal`)
* Dynamic variable is explicit. (`dynamic`)
* Keywords are not self-evaluating.
* Destructuring is not supported in `defmacro`.

# ISLISP Resources
* [ISLISP Specification](http://www.islisp.org/ISLisp-spec.html)
* [ISLISP HyperDraft (HTML)](https://islisp-dev.github.io/ISLispHyperDraft/islisp-v23.html)
