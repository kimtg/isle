(defpackage :islisp
  (:use :cl)
  (:shadow evenp oddp file-length the class / pi load))
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
(defmacro while (test-form &rest body-form) `(loop while ,test-form do ,@body-form))
(defmacro the (class-name form) `(cl:the ,(eval class-name) ,form))
(defmacro assure (&rest r) `(the ,@r))
(defmacro convert (obj class-name) `(coerce ,obj ',class-name))
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

;; 26.1. Streams to files
(defun open-input-file (filename &optional element-class)
  (open filename :direction :input :element-type element-class))

(defun open-output-file (filename &optional element-class)
  (open filename :direction :output :element-type element-class))

(defun open-io-file (filename &optional element-class)
  (open filename :direction :io :element-type element-class))

(defmacro with-open-output-file (name filename &optional element-class)
  `(with-open-file ,name ,filename :direction :output :element-type ,element-class))

(defmacro with-open-io-file (name filename &optional element-class)
  `(with-open-file ,name ,filename :direction :io :element-type ,element-class))

(defmacro with-open-input-file (name filename &optional element-class)
  `(with-open-file ,name ,filename :direction :input :element-type ,element-class))

(defun preview-char (&rest r)
  (apply #'peek-char nil r))

(defunalias create make-instance)
(defunalias initialize-object initialize-instance)

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
					   ((defun defgeneric) `(,(car expr) ,(second expr) ,(translate-lambda-list (third expr)) ,@(nthcdr 3 expr)))
					   ((labels flet)
					    `(,(car expr)
					      ,(mapcar (lambda (binding) `(,(car binding) ,(translate-lambda-list (second binding)) ,@(cddr binding))) (second expr))
					      ,@(cddr expr)))
					   (otherwise (mapcar #'il->cl expr))))
	       (t (mapcar #'il->cl expr))))
	(t (il->cl-simple expr))))

(defun repl ()
  (print-version)
  (loop
   (format t "> ")
   (finish-output)
   (handler-case (format t "~s~%" (eval (il->cl (read))))
     (end-of-file (e) (return))
     (error (e) (format t "Error: ~a~%" e)))))

(defun load (filename)
  (with-open-file (s filename)
		  (loop
		   (handler-case
		       (let ((expr (read s)))
			 (handler-case (eval (il->cl expr))
			   (error (e) (format t "~a in ~s~%" e expr) (return))))
		     (end-of-file (e) (return))))))

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
