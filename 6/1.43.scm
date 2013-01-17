(load "1.42.scm")

(define repeated (lambda (f n)
	(define prod (lambda (nthf count)
		(if (<= count 1) nthf
			(prod (compose nthf f) (- count 1))
		)
	))
	(prod f n)
))

