# Isle ISLISP
Isle ISLISP is an [ISLISP](http://www.islisp.org/) Compiler. It is written in Common Lisp ([Steel Bank Common Lisp](https://sbcl.org/)). Because it is implemented in Common Lisp, the source code itself demonstrates the differences between ISLISP and Common Lisp.

# Motivation for implementing in Common Lisp
I was thinking of modifying SBCL to remove features not in ISLISP and add features as appropriate, but SBCL is constantly improving and it is difficult to keep up, so I implemented it as a Common Lisp program in SBCL. Because Isle ISLISP uses SBCL, a top-of-the-line Common Lisp implementation, execution speed is top-notch.

# Goals
* Learn Common Lisp
* Know the difference between ISLISP and Common Lisp

## Run
```
Usage: isle [OPTIONS...] [FILE]

OPTIONS:
    -h  print this screen.
    -v  print version.
 If no FILE is specified, the REPL is run.
```

# How to build an executable of Isle ISLISP
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

# ISLISP differs from Common Lisp mainly in the following points.
* There is a global lexical variable. (`defglobal`)
* Dynamic variable is explicit. (`dynamic`)
* Destructuring is not supported in defmacro.

# ISLISP Resources
* [ISLISP Specification](http://www.islisp.org/ISLisp-spec.html)
* [ISLISP HyperDraft](https://islisp-dev.github.io/ISLispHyperDraft/islisp-v23.html)
