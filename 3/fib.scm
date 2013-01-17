(define fib (lambda (n)
	(cond 
		((= n 0) 0)
		((= n 1) 1)
		(else (+ 
			(fib (- n 1))
			(fib (- n 2))
		))
	)
))

(define fib-i (lambda (n)
	(define iter (lambda (a b count)
		(if (= count n)
			a
			(iter b (+ a b) (+ count 1))
		)
	))
	(iter 0 1 0)
))
