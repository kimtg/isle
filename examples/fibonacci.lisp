(defun fib (n) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))

(loop
 (format (standard-output) "Fibonacci>")
 (finish-output (standard-output))
 (let ((n (read)))
   (format (standard-output) "~a~%" (fib n))))
