(define square (lambda (x)
	(* x x)
))

(define inc (lambda (x)
	(+ x 1)
))

(define compose (lambda (f g)
	(lambda (x)
		(f (g x))
	)
))

