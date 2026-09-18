(require "asdf")
(defpackage :islisp
  (:use :cl)
  (:shadow evenp oddp file-length the class / pi load eval defclass internal-time-units-per-second equal))
(in-package :islisp)

(defconstant *version* "0.12")
(defun print-version ()
  (format t "Isle ISLISP v~a~%" *version*))

;; ISLISP equal: Common Lisp and ISLISP differ primarily in vectors
(defun equal (x y)
  (cond
    ((and (consp x) (consp y))
     (and (equal (car x) (car y))
          (equal (cdr x) (cdr y))))
    ((and (vectorp x) (vectorp y))
     (and (= (length x) (length y))
          (every #'equal x y)))
    (t (cl:equal x y))))


;; 11. Classes and Types
(cl:defclass <object> (standard-object) ())

;; Register standard ISLISP classes in Common Lisp
(dolist (pair '((<basic-array> array)
                (<basic-array*> array)
                (<general-array*> array)
                (<basic-vector> simple-vector)
                (<general-vector> vector)
                (<string> string)
                (<built-in-class> built-in-class)
                (<character> character)
                (<class> class)
                (<standard-class> standard-class)
                (<cons> cons)
                (<function> function)
                (<generic-function> generic-function)
                (<standard-generic-function> standard-generic-function)
                (<null> null)
                (<list> list)
                (<number> number)
                (<float> float)
                (<integer> integer)
                (<standard-object> standard-object)
                (<symbol> symbol)
                (<stream> stream)
                (<condition> condition)
                (<serious-condition> serious-condition)
                (<error> error)
                (<simple-error> simple-error)
                (<arithmetic-error> arithmetic-error)
                (<division-by-zero> division-by-zero)
                (<floating-point-overflow> floating-point-overflow)
                (<floating-point-underflow> floating-point-underflow)
                (<control-error> control-error)
                (<parse-error> parse-error)
                (<program-error> program-error)
                (<domain-error> type-error)
                (<undefined-entity> cell-error)
                (<unbound-variable> unbound-variable)
                (<undefined-function> undefined-function)
                (<stream-error> stream-error)
                (<end-of-stream> end-of-file)
                (<storage-condition> storage-condition)))
  (let ((islisp-name (car pair))
        (cl-name (cadr pair)))
    (let ((c (find-class cl-name nil)))
      (when c
        (setf (find-class islisp-name) c)))))

(deftype <basic-array> () 'array)
(deftype <basic-array*> () 'array)
(deftype <general-array*> () 'array)
(deftype <basic-vector> () 'simple-vector)
(deftype <general-vector> () 'vector)
(deftype <string> () 'string)
(deftype <character> () 'character)
(deftype <cons> () 'cons)
(deftype <function> () 'function)
(deftype <null> () 'null)
(deftype <list> () 'list)
(deftype <number> () 'number)
(deftype <float> () 'float)
(deftype <integer> () 'integer)
(deftype <symbol> () 'symbol)
(deftype <stream> () 'stream)

(defmacro defunalias (new old) `(setf (fdefinition ',new) #',old))

(defmacro class (class-name)
  (if (symbolp class-name)
      `(find-class ',class-name)
      `(let ((c ,class-name))
         (if (symbolp c) (find-class c) c))))

(defun subclassp (c1 c2)
  (let ((cls1 (if (symbolp c1) (find-class c1 nil) c1))
        (cls2 (if (symbolp c2) (find-class c2 nil) c2)))
    (cond
      ((or (eq cls2 (find-class '<object> nil))
           (eq c2 '<object>)
           (eq c2 t))
       t)
      ((and cls1 cls2)
       (subtypep cls1 cls2))
      (t nil))))

(defun instancep (obj class)
  (let ((cls (if (symbolp class) (find-class class nil) class)))
    (cond
      ((or (eq cls (find-class '<object> nil))
           (eq class '<object>)
           (eq class t))
       t)
      ((null cls) nil)
      (t (typep obj cls)))))

(defmacro the (class-name form) `(cl:the ,class-name ,form))

(defmacro assure (class-name form)
  (let ((val (gensym "VAL"))
        (cls (gensym "CLASS")))
    `(let ((,val ,form)
           (,cls (class ,class-name)))
       (if (instancep ,val ,cls)
           ,val
           (error (create (class <domain-error>)
                          'expected-class ,cls
                          'object ,val))))))

;; 13. Variables and Binding
;; Global lexical variable by Kent M. Pitman https://groups.google.com/g/comp.lang.lisp/c/d5bsSR0o4Ps/m/6Hxj3TCvDHQJ
(defmacro defglobal (var init)
  `(progn (define-symbol-macro ,var (get ',var 'lexical))
	  (setf ,var ,init)
	  ',var))

(defun gboundp (var)
  (not (eq (get var 'lexical :unbound) :unbound)))

(defun earmuff (symbol)
  "symbol -> *symbol*"
  (intern (concatenate 'string "*" (string symbol) "*")))

(defmacro defdynamic (name form) `(defparameter ,(earmuff name) ,form))
(defmacro dynamic-let (bindings &rest forms)
  `(let ,(mapcar (lambda (binding) `(,(earmuff (car binding)) ,(second binding))) bindings)
     ,@(mapcar (lambda (binding) `(declare (special ,(earmuff (car binding))))) bindings)
     ,@forms))

(defmacro dynamic (a) `(symbol-value ',(earmuff a)))
(defmacro set-dynamic (form var)
  `(setf (dynamic ,var) ,form))

(defun dynamic-boundp (var)
  (boundp (earmuff var)))

;; 14. Control structure
(defmacro while (test-form &rest body-form*) `(loop while ,test-form do ,@body-form*))
(defmacro for (&rest r) `(do ,@r))

(defunalias set-file-position file-position)
(defun float-/ (&rest xs)
  (let ((r (apply #'cl:/ xs))) (if (integerp r) r (float r))))

(defun quotient (dividend divisor1 &rest divisors) (apply #'float-/ dividend divisor1 divisors))
(defun reciprocal (x) (float-/ x))
(defun div (z1 z2) (floor (cl:/ z1 z2)))
(defun atan2 (y x) (cl:atan y x))
(defconstant *pi* cl:pi)
(defconstant *most-positive-float* most-positive-double-float)
(defconstant *most-negative-float* most-negative-double-float)

(defun parse-number (string)
  (with-input-from-string (s string)
    (let ((*read-eval* nil))
      (let ((r (read s nil :eof)))
        (if (and (numberp r) (eq (read s nil :eof) :eof))
            r
            (error (create (class <parse-error>)
                           'format-string "~a is not a valid number."
                           'format-arguments (list string))))))))

;; 17. Characters and Strings
(defun create-string (i &optional (initial-character #\space))
  (make-string i :initial-element initial-character))

(defun char-index (char string &optional (start-position 0))
  (position char string :start start-position))

(defun string-index (substring string &optional (start-position 0))
  (search substring string :start2 start-position))

(defun string-append (&rest strings) (apply #'concatenate 'string strings))
(defun set-char (char string index) (setf (char string index) char))

;; 18. Symbols
(defun property (symbol property-name &optional default)
  (get symbol property-name default))

(defun (setf property) (new-value symbol property-name &optional default)
  (declare (ignore default))
  (setf (get symbol property-name) new-value))

(defun set-property (obj symbol property-name)
  (setf (property symbol property-name) obj))

(defun remove-property (symbol property-name)
  (let ((val (get symbol property-name)))
    (remprop symbol property-name)
    val))

;; 14. Control structure (case-using)
(defmacro case-using (predform keyform &rest cases)
  (let ((key (gensym "KEY"))
        (pred (gensym "PRED")))
    `(let ((,key ,keyform)
           (,pred ,predform))
       (cond
         ,@(mapcar
            (lambda (caseform)
              (let ((keylist (car caseform))
                    (body (cdr caseform)))
                (if (eq keylist 't)
                    `(t ,@body)
                    (let ((keys (if (listp keylist) keylist (list keylist))))
                      `((or ,@(mapcar (lambda (c) `(funcall ,pred ,key ',c)) keys))
                        ,@body)))))
            cases)))))

;; 15. Classes and Methods
(defmacro defclass (class-name (&rest superclasses) (&rest slot-specs) &rest options)
  (let ((boundp-methods nil)
        (cleaned-slots nil)
        (supers (if (null superclasses) '(<object>) superclasses)))
    (dolist (slot slot-specs)
      (if (atom slot)
          (push slot cleaned-slots)
          (let ((slot-name (car slot))
                (slot-opts (cdr slot))
                (cleaned-opts nil))
            (loop while slot-opts do
              (let ((key (car slot-opts))
                    (val (cadr slot-opts)))
                (case key
                  (:boundp
                   (push (list val class-name slot-name) boundp-methods)
                   (setf slot-opts (cddr slot-opts)))
                  (:initarg
                   (push :initarg cleaned-opts)
                   (push (intern (symbol-name val) :keyword) cleaned-opts)
                   (setf slot-opts (cddr slot-opts)))
                  (otherwise
                   (push key cleaned-opts)
                   (push val cleaned-opts)
                   (setf slot-opts (cddr slot-opts))))))
            (push (cons slot-name (nreverse cleaned-opts)) cleaned-slots))))
    (setf cleaned-slots (nreverse cleaned-slots))
    `(progn
       (cl:defclass ,class-name (,@supers)
         (,@cleaned-slots)
         ,@options)
       ,@(loop for (fn-name c-name s-name) in boundp-methods
               collect `(defmethod ,fn-name ((instance ,c-name))
                          (slot-boundp instance ',s-name)))
       ',class-name)))

;; 19. Lists and Conses
(defun set-car (obj cons) (setf (car cons) obj))
(defun set-cdr (obj cons) (setf (cdr cons) obj))
(defun create-list (i &optional initial-element) (make-list i :initial-element initial-element))
(defun set-elt (obj sequence z) (setf (elt sequence z) obj))

;; 20. Arrays and Vectors
(defunalias general-array*-p arrayp)
(defunalias basic-array-p arrayp)
(defunalias basic-array*-p arrayp)
(defunalias basic-vector-p simple-vector-p)
(defunalias general-vector-p vectorp)

(defun create-array (dimensions &optional initial-element)
  (make-array dimensions :initial-element initial-element))
(defunalias create-vector create-array)
(defunalias garef aref)
(defun set-garef (obj array &rest indices)
  (setf (apply #'aref array indices) obj))
(defun set-aref (obj array &rest indices)
  (setf (apply #'aref array indices) obj))

;; 28. Objects and Classes Operations
(defun create (class &rest bindings)
  (let ((plist (loop for (k v) on bindings by #'cddr
                     for key = (intern (symbol-name k) :keyword)
                     append (cond
                              ((eq key :format-string)
                               (list :format-control v :format-string v))
                              (t (list key v))))))
    (let ((c (if (symbolp class) (find-class class nil) class)))
      (if (and c (subtypep c 'condition))
          (apply #'make-condition class plist)
          (apply #'make-instance class plist)))))

(defunalias initialize-object initialize-instance)

;; 21. Conversion
(defun convert-object (obj classname)
  (let ((name (cond
                ((symbolp classname)
                 (let ((s (symbol-name classname)))
                   (if (and (>= (length s) 3)
                            (char= (char s 0) #\<)
                            (char= (char s (1- (length s))) #\>))
                       (intern (subseq s 1 (1- (length s))) :islisp)
                       classname)))
                ((typep classname 'cl:class)
                 (class-name classname))
                (t classname))))
    (case name
      ((character <character>)
       (typecase obj
         (character obj)
         (integer (code-char obj))
         (string (if (> (length obj) 0) (char obj 0) (error "Cannot convert empty string to character")))
         (t (character obj))))
      ((integer <integer>)
       (typecase obj
         (integer obj)
         (character (char-int obj))
         (string (parse-integer obj))
         (number (truncate obj))
         (t (error "Cannot convert ~s to integer" obj))))
      ((float <float>)
       (typecase obj
         (float obj)
         (number (float obj))
         (string (float (parse-number obj)))
         (t (error "Cannot convert ~s to float" obj))))
      ((symbol <symbol>)
       (typecase obj
         (symbol obj)
         (character (intern (string obj)))
         (string (intern obj))
         (t (intern (format nil "~a" obj)))))
      ((string <string>)
       (typecase obj
         (string obj)
         (integer (write-to-string obj))
         (float (format nil "~f" obj))
         (symbol (symbol-name obj))
         (character (string obj))
         (list (coerce obj 'string))
         (vector (coerce obj 'string))
         (t (format nil "~a" obj))))
      ((list <list> <cons>)
       (typecase obj
         (list obj)
         (vector (coerce obj 'list))
         (string (coerce obj 'list))
         (t (list obj))))
      ((general-vector basic-vector <general-vector> <basic-vector>)
       (typecase obj
         (vector obj)
         (list (coerce obj 'vector))
         (string (coerce obj 'vector))
         (t (vector obj))))
      (t
       (if (and (symbolp classname) (find-class classname nil))
           (coerce obj classname)
           (error (create (class <domain-error>)
                          'expected-class classname
                          'object obj)))))))

(defmacro convert (form class-name)
  `(convert-object ,form ',class-name))

;; 26. Streams and Files
(defun standard-input () *standard-input*)
(defun standard-output () *standard-output*)
(defun error-output () *error-output*)

(defmacro with-standard-input (stream-form &rest forms)
  `(with-open-stream (*standard-input* ,stream-form) ,@forms))

(defmacro with-standard-output (stream-form &rest forms)
  `(with-open-stream (*standard-output* ,stream-form) ,@forms))

(defmacro with-error-output (stream-form &rest forms)
  `(with-open-stream (*error-output* ,stream-form) ,@forms))

(defunalias create-string-input-stream make-string-input-stream)
(defunalias create-string-output-stream make-string-output-stream)

(defun element-class-to-cl-type (element-class)
  (cond
    ((null element-class) 'character)
    ((or (eq element-class '<character>)
         (eq element-class 'character)
         (eq element-class (find-class '<character> nil)))
     'character)
    ((or (eq element-class '<integer>)
         (eq element-class 'integer)
         (eq element-class (find-class '<integer> nil)))
     '(unsigned-byte 8))
    (t element-class)))

(defun file-length (filename element-class)
  (with-open-file (s filename
                     :direction :input
                     :element-type (element-class-to-cl-type element-class))
    (cl:file-length s)))

(defun open-input-file (filename &optional element-class)
  (open filename :direction :input
                 :element-type (element-class-to-cl-type element-class)))

(defun open-output-file (filename &optional element-class)
  (open filename :direction :output
                 :if-does-not-exist :create
                 :if-exists :supersede
                 :element-type (element-class-to-cl-type element-class)))

(defun open-io-file (filename &optional element-class)
  (open filename :direction :io
                 :if-does-not-exist :create
                 :element-type (element-class-to-cl-type element-class)))

(defmacro with-open-output-file ((name filename &optional element-class) &rest form*)
  `(with-open-file (,name ,filename
                          :direction :output
                          :if-does-not-exist :create
                          :if-exists :supersede
                          :element-type (element-class-to-cl-type ,element-class))
     ,@form*))

(defmacro with-open-io-file ((name filename &optional element-class) &rest form*)
  `(with-open-file (,name ,filename
                          :direction :io
                          :if-does-not-exist :create
                          :element-type (element-class-to-cl-type ,element-class))
     ,@form*))

(defmacro with-open-input-file ((name filename &optional element-class) &rest form*)
  `(with-open-file (,name ,filename
                          :direction :input
                          :element-type (element-class-to-cl-type ,element-class))
     ,@form*))

(defun format-char (output-stream char) (format output-stream "~c" char))
(defun format-float (output-stream float) (format output-stream "~g" float))
(defun format-fresh-line (output-stream) (format output-stream "~&"))
(defun format-integer (output-stream integer radix) (format output-stream "~vr" radix integer))
(defun format-object (output-stream obj escape-p)
  (format output-stream
	  (if escape-p "~s" "~a") obj))

(defun format-tab (output-stream column) (format output-stream "~vt" column))

(defun preview-char (&optional (stream *standard-input*) (eof-error-p t) eof-value)
  (peek-char nil stream eof-error-p eof-value))

(defun stream-ready-p (input-stream)
  (let ((c (read-char-no-hang input-stream)))
    (cond (c (unread-char c input-stream) t)
	  (t nil))))

;; 29. Condition system
(defun simple-error-format-string (c)
  (if (typep c 'simple-condition)
      (simple-condition-format-control c)
      (slot-value c 'format-string)))

(defun simple-error-format-arguments (c)
  (if (typep c 'simple-condition)
      (simple-condition-format-arguments c)
      (slot-value c 'format-arguments)))

(defun domain-error-object (c)
  (if (typep c 'type-error)
      (type-error-datum c)
      (slot-value c 'object)))

(defun domain-error-expected-class (c)
  (if (typep c 'type-error)
      (type-error-expected-type c)
      (slot-value c 'expected-class)))

(defun undefined-entity-name (c)
  (if (typep c 'cell-error)
      (cell-error-name c)
      nil))

(defun undefined-entity-namespace (c)
  (typecase c
    (unbound-variable 'variable)
    (undefined-function 'function)
    (t nil)))

(defvar *condition-continuable-table* (make-hash-table :test 'eq))

(defun condition-continuable (condition)
  (gethash condition *condition-continuable-table*))

(defun signal-condition (condition continuable)
  (when continuable
    (setf (gethash condition *condition-continuable-table*) continuable))
  (if continuable
      (restart-case
          (signal condition)
        (continue ()
          :report (lambda (stream)
                    (format stream "~a" (if (stringp continuable) continuable "Continue")))
          continuable)
        (use-value (val)
          :report "Use replacement value"
          val))
      (error condition)))

(defun report-condition (condition stream)
  (format stream "~a~%" condition))

(defun continue-condition (condition &optional value)
  (if value (use-value value condition)
    (continue condition)))

(defmacro with-handler (handler &rest form*)
  (let ((h (gensym "HANDLER")))
    `(let ((,h ,handler))
       (handler-bind ((condition (lambda (c) (funcall ,h c))))
         ,@form*))))

;; 30. Miscellaneous
(defun internal-time-units-per-second () cl:internal-time-units-per-second)

;; Utility
(defun build-exe (filename)
  (setq uiop:*image-entry-point* #'main)
  (uiop:dump-image filename :executable t)
  (uiop:quit 0 t))

;; Translator (il->cl)
(defun translate-lambda-list (expr)
  ":rest -> &rest"
  (if (listp expr)
      (mapcar (lambda (item)
                (cond
                  ((eq item :rest) '&rest)
                  (t item)))
              expr)
      expr))

(defun translate-defmethod (expr)
  (let ((name (second expr))
        (rest (cddr expr)))
    (if (and (rest rest) (keywordp (first rest)))
        `(defmethod ,name ,(first rest)
           ,(translate-lambda-list (second rest))
           ,@(mapcar #'il->cl (cddr rest)))
        `(defmethod ,name
           ,(translate-lambda-list (first rest))
           ,@(mapcar #'il->cl (cdr rest))))))

(defun il->cl (expr)
  "Translates ISLISP to Common Lisp"
  (cond
    ((listp expr)
     (cond
       ((null expr) nil)
       ((eq (car expr) 'quote)
        expr)
       ((eq (car expr) 'function)
        (if (and (consp (second expr)) (eq (car (second expr)) 'lambda))
            `(function ,(il->cl (second expr)))
            expr))
       ((eq (car expr) 'defmethod)
        (translate-defmethod expr))
       ((member (car expr) '(defun defgeneric defmacro))
        `(,(car expr) ,(second expr) ,(translate-lambda-list (third expr))
          ,@(mapcar #'il->cl (nthcdr 3 expr))))
       ((eq (car expr) 'lambda)
        `(lambda ,(translate-lambda-list (second expr))
           ,@(mapcar #'il->cl (cddr expr))))
       ((member (car expr) '(labels flet))
        `(,(car expr)
          ,(mapcar (lambda (binding)
                     `(,(first binding)
                       ,(translate-lambda-list (second binding))
                       ,@(mapcar #'il->cl (cddr binding))))
                   (second expr))
          ,@(mapcar #'il->cl (cddr expr))))
       ((member (car expr) '(let let*))
        `(,(car expr)
          ,(mapcar (lambda (binding)
                     (if (listp binding)
                         `(,(first binding) ,(il->cl (second binding)))
                         binding))
                   (second expr))
          ,@(mapcar #'il->cl (cddr expr))))
       (t
        (mapcar #'il->cl expr))))
    (t expr)))

;; Extended functions
(defun eval (expr) (cl:eval (il->cl expr)))

(defun load (filename)
  (handler-case
      (with-open-file (s filename :direction :input)
        (loop
          (let ((expr (read s nil :eof)))
            (when (eq expr :eof)
              (return t))
            (eval expr))))
    (error (e)
      (format *error-output* "Error loading ~a: ~a~%" filename e)
      nil)))

;; REPL
(defun repl ()
  (print-version)
  (loop
    (format t "> ")
    (finish-output)
    (handler-case
        (let ((expr (read *standard-input* nil :eof)))
          (when (eq expr :eof)
            (uiop:quit 0 t))
          (format t "~s~%" (eval expr)))
      #+sbcl (sb-sys:interactive-interrupt ()
               (format t "~%Interrupt~%")
               (finish-output))
      (end-of-file ()
        (uiop:quit 0 t))
      (error (e)
        (format t "Error: ~a~%" e)
        (finish-output)
        (ignore-errors (read-line *standard-input* nil nil))))))

(defun print-help ()
  (write-line
   "Usage: isle [OPTIONS...] [FILE]

OPTIONS:
    -h, --help       print this screen.
    -v, --version    print version.
    -e, --eval EXPR  evaluate EXPR and exit.

If no FILE or -e is specified, the REPL is run."))

;; Entry
(defun main ()
  (in-package :islisp)
  (let ((argv (uiop:command-line-arguments)))
    (cond
      ((null argv)
       (repl))
      ((or (string= (first argv) "-h")
           (string= (first argv) "--help"))
       (print-help)
       (uiop:quit 0 t))
      ((or (string= (first argv) "-v")
           (string= (first argv) "--version"))
       (print-version)
       (uiop:quit 0 t))
      ((or (string= (first argv) "-e")
           (string= (first argv) "--eval"))
       (if (second argv)
           (handler-case
               (let ((expr (read-from-string (second argv))))
                 (format t "~s~%" (eval expr))
                 (uiop:quit 0 t))
             (error (e)
               (format *error-output* "Error: ~a~%" e)
               (uiop:quit 1 t)))
           (progn
             (format *error-output* "Error: -e requires an expression.~%")
             (uiop:quit 1 t))))
      (t
       (let ((success (load (first argv))))
         (uiop:quit (if success 0 1) t))))))

(main)
