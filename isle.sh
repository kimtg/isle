#!/bin/sh

lisp=${ISLE_IMPL:-sbcl}
isle_path="$(dirname -- "$0")/isle.lisp"

if [ "$lisp" = sbcl ]; then
    sbcl --noinform --end-runtime-options --script "$isle_path" "$@"
elif [ "$lisp" = ccl ]; then
    ccl --no-init --load "$isle_path" -- "$@"
elif [ "$lisp" = clisp ]; then
    clisp -norc -ansi "$isle_path" -- "$@"
else
    printf "Uknown lisp: %s\n" "$lisp" >&2
    exit 1
fi
