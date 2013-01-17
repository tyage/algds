(define product-iter (lambda (term a next b)
	(define iter (lambda(a result)
		(if (> a b)
			result
			(iter (next a) (* result (term a)))
		)
	))
	(iter a 1)
))

(define product-recur (lambda (term a next b)
	(if (> a b) 1
		(* (term a) (product-recur term (next a) next b))
	)
))

(define factorial (lambda (n)
	(product-iter 
		(lambda (n) n)
		1
		(lambda (n) (+ n 1))
		n
	)
))

(define pi (lambda (n)
	(define term (lambda (n) n))
	(define next (lambda (n) (+ n 2)))
	(/ (* 4
			(product-iter term 2 next (* 2 n))
			(product-iter term 4 next (* 2 (+ n 1)))
		) 
		(* (product-iter term 3 next (+ (* 2 n) 1))
			(product-iter term 3 next (+ (* 2 n) 1))
		)
	)
))

