(define make-rat (lambda (n d)
	(define g (gcd n d))
	(cons (/ n g) (/ d g))
))

(define numer (lambda (x) (car x)))
(define denom (lambda (x) (cdr x)))

(define print-rat (lambda (x)
	(newline)
	(display (numer x))
	(display "/")
	(display (denom x))
))

(define add-rat (lambda (x y)
	(make-rat
		(+ 
			(* (numer x) (denom y))
			(* (numer y) (denom x))
		)
		(* (denom x) (denom y))
	)
))

(define sub-rat (lambda (x y)
	(make-rat
		(- 
			(* (numer x) (denom y))
			(* (numer y) (denom x))
		)
		(* (denom x) (denom y))
	)
))

(define mul-rat (lambda (x y)
	(make-rat
		(* (numer x) (numer y))
		(* (denom x) (denom y))
	)
))

(define div-rat (lambda (x y)
	(make-rat
		(* (numer x) (denom y))
		(* (denom x) (numer y))
	)
))


(define equal-rat? (lambda (x y)
	(= 
		(* (numer x) (denom y))
		(* (denom x) (numer y))
	)
))

(define to-inexact (lambda (x)
	(/ (numer x) (denom x))
))

