(define fact-iter (lambda (n)
	(define iter (lambda (prod count)
		(if (> count n)
			prod
			(iter (* prod count) (+ count 1))
		)
	))
	(iter 1 1)
))
