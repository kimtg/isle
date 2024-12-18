# Isle ISLISP
Isle ISLISP is an [ISLISP](https://en.wikipedia.org/wiki/ISLISP) Compiler. It
is written in Common Lisp. Because it is implemented in Common Lisp, the
source code itself demonstrates the differences between ISLISP and Common
Lisp.

# Motivation for implementing in Common Lisp
I was thinking of modifying SBCL to remove features not in ISLISP and add
features as appropriate, but SBCL is constantly improving and it is difficult
to keep up, so I implemented it as a Common Lisp program. Because Isle ISLISP
uses SBCL by default, a top-of-the-line Common Lisp implementation, execution
speed is top-notch.

For reference, these days Racket is also implemented in Chez Scheme.

# Goals
* Learn Common Lisp
* Learn how to use macros
* Know the difference between ISLISP and Common Lisp

## Dependencies

* [ASDF](https://asdf.common-lisp.dev/): ASDF is the de facto standard build facility for Common Lisp. Your Lisp implementation probably contains a copy of ASDF, which you can load using (require "asdf").

## Run

Run `isle.sh` or `isle.bat`.
```
Usage: isle [OPTIONS...] [FILE]

OPTIONS:
    -h  print this screen.
    -v  print version.
 If no FILE is specified, the REPL is run.
```

The shell script uses SBCL by default, but other Common Lisp implementations
can be used by setting the `ISLE_IMPL` environment variable. Currently
supported CLs are:

* sbcl
* ccl
* clisp

```
ISLE_IMPL=ccl ./isle.sh
```

Other implementations can be used manually; consider enhancing the script with
support for them if you use something else.

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
| Function               | Description                                                  |
|------------------------|--------------------------------------------------------------|
| (eval *form*)          | evaluates *form*                                             |
| (load *filename*)      | loads code from *filename* e.g. (load "foo.lisp")            |
| (macroexpand *form*)   | repeatedly expands *form* until it is no longer a macro form |
| (macroexpand-1 *form*) | expands *form* once                                          |

# Bugs
`signal-condition` ignores `continuable` argument.

# Major differences from Common Lisp
* There is a global lexical variable. (`defglobal`)
* Dynamic variable is explicit. (`dynamic`)
* Keywords are not self-evaluating.
* Destructuring is not supported in `defmacro`.

# ISLISP Resources
* [ISLISP Specification](http://www.islisp.org/ISLisp-spec.html)
* [ISLISP HyperDraft (HTML)](https://islisp-dev.github.io/ISLispHyperDraft/islisp-v23.html)
