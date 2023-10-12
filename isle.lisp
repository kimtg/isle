(defpackage :islisp
  (:use :cl)
  (:shadow evenp oddp file-length the class / pi load eval defclass internal-time-units-per-second))
(in-package :islisp)

(defconstant *version* "0.1")
(defun print-version ()
  (format t "Isle ISLISP v~a~%" *version*))

;; initialize environment
;; Things you can do with macros and functions
;; Global lexical variable by Kent M. Pitman https://groups.google.com/g/comp.lang.lisp/c/d5bsSR0o4Ps/m/6Hxj3TCvDHQJ
(defmacro defglobal (var init)
  `(progn (define-symbol-macro ,var (get ',var 'lexical))
	  (setf ,var ,init)
	  ',var))

(defun earmuff (symbol)
  "symbol -> *symbol*"
  (intern (concatenate 'string "*" (string symbol) "*")))

(defmacro defdynamic (name form) `(defparameter ,(earmuff name) ,form))
(defmacro dynamic-let (bindings &rest forms)
  `(progn ,@(mapcar (lambda (binding) `(defvar ,(earmuff (car binding)))) bindings)
	  (let ,(mapcar (lambda (binding) `(,(earmuff (car binding)) ,(second binding))) bindings) ,@forms)))
  
(defmacro dynamic (a) (earmuff a))
(defmacro set-dynamic (form var) `(setf ,form ,var))
;; 14. Control structure
(defmacro while (test-form &rest body-form*) `(loop while ,test-form do ,@body-form*))
(defmacro for ((&rest iteration-spec*) (end-test &rest result*) &rest form*)
  `(loop 
    ,@(apply #'append (mapcar (lambda (iter-spec) `(with ,(car iter-spec) = ,(second iter-spec))) iteration-spec*))
    until ,end-test
    do ,@form*
    ,`(psetf
       ,@(apply #'append
		(remove nil
			(mapcar (lambda (iter-spec)
				  (if (>= (length iter-spec) 3) ; if step is there
				      `(,(car iter-spec) ,(third iter-spec))
				    nil)) ; no-op
				iteration-spec*))))
    finally (return (progn ,@result*))))

(defmacro the (class-name form) `(cl:the ,(cl:eval class-name) ,form))
(defmacro assure (&rest r) `(the ,@r))
(defmacro defunalias (new old) `(setf (fdefinition ',new) #',old))
(defunalias subclassp subtypep)
(defunalias class find-class)
(defunalias instancep typep)
(defunalias set-file-position file-position)
(defun / (&rest xs)
  (let ((r (apply #'cl:/ xs))) (if (integerp r) r (float r))))

(defun quotient (dividend divisor1 &rest divisors) (apply #'/ dividend divisor1 divisors))
(defun reciprocal (x) (/ x))
(defconstant *pi* cl:pi)
(defconstant *most-positive-float* most-positive-double-float)
(defconstant *most-negative-float* most-negative-double-float)
(defun file-length (filename element-class)
  (with-open-file (s filename) (* (cl:/ 8 element-class) (cl:file-length s))))

(defun standard-input () *standard-input*)
(defun standard-output () *standard-output*)
(defun error-output () *error-output*)

(defun set-elt (obj sequence z) (setf (elt sequence z) obj))

(defun create-string (i &optional (initial-character #\space))
  (make-string i :initial-element initial-character))

(defun char-index (char string &optional (start-position 0))
  (position char string :start start-position))

(defun string-index (substring string &optional (start-position 0))
  (search substring string :start2 start-position))

(defun string-append (&rest strings) (apply #'concatenate 'string strings))
(defmacro case-using (predform keyform &rest cases)
  (let ((key (gensym))
	(pred (gensym)))
    `(let ((,key ,keyform)
	   (,pred ,predform))
       (cond ,@(mapcar (lambda (caseform)
			(if (eql (car caseform) 't) `(t ,@(rest caseform))
			  `(
			   ,`(or ,@(mapcar (lambda (c) `(funcall ,pred ,key ,c)) (car caseform)))
			   ,@(rest caseform))))
		       cases)))))

(defmacro with-standard-input (stream-form &rest forms)
  `(with-open-stream (*standard-input* ,stream-form) ,@forms))

(defmacro with-standard-output (stream-form &rest forms)
  `(with-open-stream (*standard-output* ,stream-form) ,@forms))

(defmacro with-error-output (stream-form &rest forms)
  `(with-open-stream (*error-output* ,stream-form) ,@forms))

(defunalias create-string-input-stream make-string-input-stream)
(defunalias create-string-output-stream make-string-output-stream)
(defunalias format-fresh-line fresh-line)

(defun parse-number (string)
  (let ((*read-eval* nil)) (read (make-string-input-stream string))))  

(defun convert (obj classname)
  (case classname
	((character) (code-char obj))
	((integer)
	 (typecase obj
		   (character (char-int obj))
		   (string (parse-integer obj))
		   (t (coerce obj classname))))
	((float)
	 (typecase obj
		   (string (parse-number obj))
		   (t (coerce obj classname))))
	((symbol)
	 (typecase obj
		   (character (intern (string obj)))
		   (string (intern obj))))
	((string)
	 (typecase obj
		   (integer (write-to-string obj))
		   (float (write-to-string obj))
		   (symbol (write-to-string obj))))
	(t (coerce obj classname))))

(defun div (z1 z2) (floor (cl:/ z1 z2)))
(defun set-car (obj cons) (setf (car cons) obj))
(defun set-cdr (obj cons) (setf (cdr cons) obj))
(defun create-list (i &optional initial-element) (make-list i :initial-element initial-element))
(defunalias general-array*-p arrayp)
(defun create-array (dimensions &optional initial-element)
  (make-array dimensions :initial-element initial-element))
(defunalias garef aref)
(defunalias basic-vector-p simple-vector-p)
(defunalias general-vector-p vectorp)
(defunalias create-vector create-array)

(defmacro defclass (class-name (&rest sc-name*) (&rest slot-spec*) &rest class-opt*)
  (let* (boundp-funs (slot-spec*2
	 (loop for slot in slot-spec* collect
	       (if (listp slot)
		   (loop with s = slot while s collect (cond ((eql (second s) :boundp)
									  (push (list (third s) (car s)) boundp-funs)
									  (prog1 (car s) (setf s (cdddr s))))
									 (t (prog1 (car s) (setf s (cdr s))))))
		 slot))))
    `(progn
       (cl:defclass ,(cl:eval class-name) (,@sc-name*) (,@slot-spec*2) ,@class-opt*)
    ,@(loop for (f slot-name) in boundp-funs collect `(defmethod ,f ((instance ,(cl:eval class-name))) (slot-boundp instance ',slot-name))))))

;; 18.2. Symbol properties
(defmacro property (&rest r) `(get ,@r))
(defun set-property (obj symbol property-name) (setf (property symbol property-name) obj))
(defun remove-property (symbol property-name) (second (remprop symbol property-name)))

;; 26.1. Streams to files
(defun open-input-file (filename &optional element-class)
  (open filename :direction :input :element-type element-class))

(defun open-output-file (filename &optional element-class)
  (open filename :direction :output :if-does-not-exist :create :element-type element-class))

(defun open-io-file (filename &optional element-class)
  (open filename :direction :io :element-type element-class))

(defmacro with-open-output-file ((name filename &optional element-class) &rest form*)
  `(with-open-file (,name ,filename :direction :output :if-does-not-exist :create :element-type ,element-class) ,@form*))

(defmacro with-open-io-file ((name filename &optional element-class) &rest form*)
  `(with-open-file (,name ,filename :direction :io :element-type ,element-class) ,@form*))

(defmacro with-open-input-file ((name filename &optional element-class) &rest form*)
  `(with-open-file (,name ,filename :direction :input :element-type ,element-class) ,@form*))

(defun format-char (output-stream char) (format output-stream "~c" char))
(defun format-float (output-stream float) (format output-stream "~g" float))
(defun format-fresh-line (output-stream) (format output-stream "~&"))
(defun format-integer (output-stream integer radix) (format output-stream "~vr" radix integer))
(defun format-object (output-stream obj escape-p)
  (format output-stream
	  (if escape-p "~s" "~a") obj))

(defun format-tab (output-stream column) (format output-stream "~vt" column))

(defun preview-char (&rest r)
  (apply #'peek-char nil r))

(defun stream-ready-p (input-stream)
  (let ((c (read-char-no-hang input-stream)))
    (cond (c (unread-char c input-stream) t)
	  (t nil))))

(defunalias create make-instance)
(defunalias initialize-object initialize-instance)

;; 29. Condition system
(defun signal-condition (condition continuable)
  (signal condition))

(defun report-condition (condition stream)
  (format stream "~a~%" condition))

(defun continue-condition (condition &optional value)
  (if value (use-value value condition)
    (continue condition)))

(defmacro with-handler (handler &rest form*)
  `(handler-bind ((error handler)) ,@form*))

;; 30. Miscellaneous
(defun internal-time-units-per-second () cl:internal-time-units-per-second)

;; utility
(defun build-exe (filename)
  (sb-ext:save-lisp-and-die filename :toplevel #'main :executable t))

;; translator
(defun translate-lambda-list (expr)
  ":rest -> &rest"
  (if (listp expr)
      (mapcar (lambda (x) (case x (:rest '&rest) (otherwise x))) expr)
    expr))

(defun il->cl-simple (expr)
  (if (symbolp expr)
      (case expr
	    ((<general-vector>) ''simple-vector)
	    (otherwise
	     (let ((s (string expr)))
	       ;; <class> -> 'class
	       (cond ((and
		       (>= (length s) 3)
		       (char= (elt s 0) #\<)
		       (char= (elt s (1- (length s))) #\>))
		      `(quote ,(intern (subseq (string expr) 1 (- (length (string expr)) 1)))))
		     (t expr)))))
    expr))

(defun il->cl (expr)
  "Translates ISLISP to Common Lisp"
  (cond ((listp expr)
	 (cond ((>= (length expr) 2) (case (car expr)
					   (lambda `(lambda ,(translate-lambda-list (second expr)) ,@(cddr expr)))
					   ((defun defgeneric defmacro) `(,(car expr) ,(second expr) ,(translate-lambda-list (third expr)) ,@(nthcdr 3 expr)))
					   ((labels flet)
					    `(,(car expr)
					      ,(mapcar (lambda (binding) `(,(car binding) ,(translate-lambda-list (second binding)) ,@(cddr binding))) (second expr))
					      ,@(cddr expr)))
					   (otherwise (mapcar #'il->cl expr))))
	       (t (mapcar #'il->cl expr))))
	(t (il->cl-simple expr))))

(defun eval (expr) (cl:eval (il->cl expr)))

(defun load (filename)
  (with-open-file (s filename)
		  (loop
		   (handler-case
		       (let ((expr (read s)))
			 (handler-case (eval expr)
			   (error (e) (format t "Error: ~a in ~s~%" e expr) (return))))
		     (end-of-file () (return))))))

(defun repl ()
  (print-version)
  (loop
   (format t "> ")
   (finish-output)
   (handler-case
       (let ((expr (read)))
	 (handler-case (format t "~s~%" (eval expr))
	   (error (e) (format t "Error: ~a in ~s~%" e expr))))
     (end-of-file () (return)))))

(defun main ()
  ;;(format t "argv: ~a~%" sb-ext:*posix-argv*)
  (cond ((<= (length sb-ext:*posix-argv*) 1) (repl))
	((string= (second sb-ext:*posix-argv*) "-h")
	 (format t "Usage: isle [OPTIONS...] [FILE]~%")
	 (format t "~%")
	 (format t "OPTIONS:~%")
	 (format t "    -h	print this screen.~%")
	 (format t "    -v	print version.~%")
	 (format t " If no FILE is specified, the REPL is run.~%"))
	((string= (second sb-ext:*posix-argv*) "-v")
	 (print-version))
	;; FILE
	(t (load (second sb-ext:*posix-argv*)))))

(main)
