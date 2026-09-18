;; Isle ISLISP Test Suite

(defglobal *tests-run* 0)
(defglobal *tests-failed* 0)

(defun report-test (name ok)
  (setf *tests-run* (+ *tests-run* 1))
  (if ok
      (format (standard-output) "  PASS: ~a~%" name)
      (progn
        (setf *tests-failed* (+ *tests-failed* 1))
        (format (standard-output) "  FAIL: ~a~%" name))))

(format (standard-output) "Running Isle ISLISP Test Suite...~%~%")

;; 1. Variables & Bindings
(defglobal g1 100)
(report-test "defglobal" (= g1 100))
(report-test "gboundp bound" (gboundp 'g1))
(report-test "gboundp unbound" (not (gboundp 'some-unbound-var)))

(defdynamic dyn1 20)
(report-test "dynamic" (= (dynamic dyn1) 20))
(dynamic-let ((dyn1 30))
  (report-test "dynamic-let" (= (dynamic dyn1) 30)))
(report-test "dynamic restored" (= (dynamic dyn1) 20))
(set-dynamic 40 dyn1)
(report-test "set-dynamic" (= (dynamic dyn1) 40))

;; 2. Control structures
(let ((i 0) (acc nil))
  (while (< i 5)
    (setf acc (cons i acc))
    (setf i (+ i 1)))
  (report-test "while" (equal acc '(4 3 2 1 0))))

(let ((res (for ((i 0 (+ i 1))
                 (sum 0 (+ sum i)))
                ((>= i 5) sum))))
  (report-test "for" (= res 10)))

(report-test "case-using"
  (= (case-using (function eql) 'b
       ((a) 1)
       ((b) 2)
       (t 3))
     2))

;; 3. Functions & Closures & AST Translator (il->cl)
(defun test-rest (a :rest r) (list a r))
(report-test "defun with :rest" (equal (test-rest 1 2 3) '(1 (2 3))))

(defun nested-rest (:rest outer)
  (lambda (:rest inner)
    (cons outer inner)))
(report-test "nested lambda with :rest in defun"
  (equal (funcall (nested-rest 1 2) 3 4) '((1 2) 3 4)))

(defmacro my-mac (x :rest xs)
  `(list ,x ',xs))
(report-test "defmacro with :rest"
  (equal (my-mac 10 20 30) '(10 (20 30))))

;; 4. Math & Numbers
(report-test "quotient" (= (quotient 7 2) 3.5))
(report-test "div" (= (div 7 2) 3))
(report-test "reciprocal" (= (reciprocal 2) 0.5))
(report-test "atan2" (= (atan2 0 1) 0.0))
(report-test "parse-number int" (= (parse-number "123") 123))
(report-test "parse-number float" (= (parse-number "123.5") 123.5))

;; 5. Strings and Sequences
(defglobal str (create-string 3 #\a))
(report-test "create-string" (equal str "aaa"))
(set-char #\b str 1)
(report-test "set-char" (equal str "aba"))
(report-test "string-append" (equal (string-append "foo" "bar") "foobar"))
(report-test "char-index" (= (char-index #\b str) 1))
(report-test "string-index" (= (string-index "ba" str) 1))

(defglobal c (cons 1 2))
(set-car 10 c)
(set-cdr 20 c)
(report-test "set-car & set-cdr" (equal c '(10 . 20)))

;; 6. Arrays & Vectors
(defglobal v (create-vector 3 0))
(set-garef 99 v 1)
(report-test "create-vector & set-garef" (= (garef v 1) 99))
(set-aref 88 v 2)
(report-test "set-aref" (= (aref v 2) 88))
(report-test "general-vector-p" (general-vector-p v))
(report-test "vector equal" (equal #(1 2 3) #(1 2 3)))
(report-test "nested vector in list equal" (equal (list #(1 2)) (list #(1 2))))
(report-test "different vector not equal" (not (equal #(1 2 3) #(1 2 4))))

;; 7. Symbols & Properties
(set-property 'val 'sym 'prop)
(report-test "property get" (eq (property 'sym 'prop) 'val))
(report-test "remove-property value" (eq (remove-property 'sym 'prop) 'val))
(report-test "property removed" (null (property 'sym 'prop)))

;; 8. Conversion
(report-test "convert int to char" (char= (convert 65 <character>) #\A))
(report-test "convert char to int" (= (convert #\A <integer>) 65))
(report-test "convert string to int" (= (convert "42" <integer>) 42))
(report-test "convert int to float" (= (convert 42 <float>) 42.0))
(report-test "convert list to vector" (equal (convert '(1 2 3) <general-vector>) #(1 2 3)))
(report-test "convert vector to list" (equal (convert #(1 2 3) <list>) '(1 2 3)))

;; 9. Object System & Classes
(defclass <vehicle> ()
  ((wheels :initarg :wheels :reader vehicle-wheels :boundp vehicle-wheels-boundp)))

(defclass <car> (<vehicle>)
  ((brand :initarg :brand :accessor car-brand :boundp car-brand-boundp)))

(defglobal car1 (create (class <car>) 'brand "Acme" 'wheels 4))
(report-test "class instance creation" (instancep car1 (class <car>)))
(report-test "instancep superclass" (instancep car1 (class <vehicle>)))
(report-test "instancep <object>" (instancep car1 (class <object>)))
(report-test "instancep number <object>" (instancep 123 (class <object>)))
(report-test "subclassp" (subclassp (class <car>) (class <vehicle>)))
(report-test "subclassp <object>" (subclassp (class <vehicle>) (class <object>)))
(report-test "subclassp <integer> <number>" (subclassp (class <integer>) (class <number>)))
(report-test "reader" (= (vehicle-wheels car1) 4))
(report-test "boundp reader" (vehicle-wheels-boundp car1))
(report-test "accessor reader" (equal (car-brand car1) "Acme"))
(setf (car-brand car1) "Turbo")
(report-test "accessor writer" (equal (car-brand car1) "Turbo"))

(defgeneric greet (x :rest r))
(defmethod greet ((c <car>) :rest r)
  (list 'car (car-brand c) r))
(report-test "defmethod with class specializer & :rest"
  (equal (greet car1 'extra) '(car "Turbo" (extra))))

(report-test "the" (= (the <integer> 50) 50))
(report-test "assure" (= (assure <integer> 50) 50))

;; 10. Condition System
(defglobal continuable-msg nil)
(defglobal handled-val nil)

(setf handled-val
      (with-handler
          (lambda (c)
            (setf continuable-msg (condition-continuable c))
            (continue-condition c 999))
        (signal-condition (create (class <simple-error>)
                                  'format-string "Something can be continued"
                                  'format-arguments nil)
                          "Continue execution")))

(report-test "signal-condition continuable handled" (= handled-val 999))
(report-test "condition-continuable string" (equal continuable-msg "Continue execution"))

;; Condition accessors test
(defglobal se (create (class <simple-error>) 'format-string "Err ~a" 'format-arguments '(val)))
(report-test "simple-error-format-string" (equal (simple-error-format-string se) "Err ~a"))
(report-test "simple-error-format-arguments" (equal (simple-error-format-arguments se) '(val)))

;; Test Summary
(format (standard-output) "~%Test Results: ~a run, ~a failed.~%" *tests-run* *tests-failed*)
(if (= *tests-failed* 0)
    (format (standard-output) "ALL TESTS PASSED!~%")
    (format (standard-output) "SOME TESTS FAILED!~%"))
