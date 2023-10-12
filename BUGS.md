The example in the ISLISP Working Draft 23.0 29.2.1. Operations relating to condition signaling
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
