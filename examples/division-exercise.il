(setf cl:*random-state* (cl:make-random-state t))
(format (standard-output) "Division Exercise
Enter q to exit.~%")
(let ((score 0) a b c c2 line)
  (block while
	 (while t
	   (setf b (+ 1 (cl:random 10)))
	   (setf c (cl:random 100))
	   (setf a (* b c))
	   (format (standard-output) "~%Score: ~a~%" score)
	   (format (standard-output) "~a / ~a = " a b)
	   (finish-output (standard-output))
	   (setf line (read-line))
	   (when (equal line "q") (return-from nil nil))
	   (setf c2 (ignore-errors (parse-number line)))
	   (cond ((equal c c2)
		  (format (standard-output) "Correct!~%")
		  (setf score (+ 1 score)))
		 (t (format (standard-output) "Wrong! Correct answer is ~a.~%" c))))))
