(define kock-line (lambda (p0 q0 p1 q1 r i)
	(if (= i 0)
		(list (make-vect p0 q0) (make-vect p1 q1))
		(let* ((r1 (/ r 3.0))
			(x3 (/ (- p1 p0) 3.0))
			(y3 (/ (- q1 q0) 3.0))
			(xs (/ (+ (* 2.0 p0) p1) 3.0))
			(ys (/ (+ (* 2.0 q0) q1) 3.0))
			(xl (/ (+ p0 (* 2.0 p1)) 3.0))
			(yl (/ (+ q0 (* 2.0 q1)) 3.0))
			(xm (+ (* 0.5 x3) (* 0.866 y3) xs))
			(ym (+ (* 0.5 y3) (* -0.866 x3) ys)) )
			(append (kock-line p0 q0 xs ys r1 (- i 1))
				(kock-line xs ys xm ym r1 (- i 1))
				(kock-line xm ym xl yl r1 (- i 1))
				(kock-line xl yl p1 q1 r1 (- i 1)) )))))

(define vectors->segments (lambda (vectors)
	(define vector->segment (lambda (vector-list)
		(define a (car vector-list))
		(define b (cadr vector-list))
		(if (eq? nil b)
			'()
			(append (list (make-segment 
				(make-vect (car a) (cdr a))
				(make-vect (car b) (cdr b))))
				(vector->segment (cdr vector-list))) )))
	(vector->segment vectors) ))

(define kock (lambda (n)
	(let* ((h (/ 0.75 0.86))
		(p0 (/ (- 1.0 h) 2))
		(p1 (- 1.0 p0)) )
		(segments->painter
			(vectors->segments
				(append
					(kock-line p0 0.25 p1 0.25 1 n)
					(kock-line p1 0.25 0.5 1.0 1 n)
					(kock-line 0.5 1.0 p0 0.25 1 n) ))))))

; 以下は使用例
(load "init.lsp")
((kock 3) frm1)
