# signal-condition Example

The example in the ISLISP Working Draft 23.0 '29.2.1. Operations relating to condition signaling'
```
(signal-condition (create (class <simple-error>)
                          'format-string "A ~A problem occurred."
                          'format-arguments '(bad))
                  nil)
```
doesn't work because of the structure of the error class in Common Lisp.

```
(signal-condition (create (class <simple-error>)
                          'format-control "A ~A problem occurred."
                          'format-arguments '(bad))
                  nil)
```
works.

For the above reasons, specification '29.3. Data associated with condition classes' could not be implemented.

For your information, OKI ISLisp, the reference implementation of ISLISP, cannot run the above example.
```
> ISLisp  Version 0.80 (1999/02/25)
>
ISLisp>(signal-condition (create (class <simple-error>)
                          'format-string "A ~A problem occurred."
                          'format-arguments '(bad))
                  nil)
> Error at CREATE
> Can't create instance for system defined class: #<SCLASS 00002052: <SIMPLE-ERROR>>
>>> ISLisp Debugger menu:
>>>            1    return to toplevel.
>>>           :h    show debugger command help.
Error:1>
```
