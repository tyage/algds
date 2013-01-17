(define (square z) (* z z))
(define (real-part z) (car z)) 
(define (imag-part z) (cdr z)) 
(define (magnitude z)
  (sqrt (+ (square (real-part z))
          (square (imag-part z)) )))
(define (angle z)
  (atan (imag-part z) (real-part z)))

(define (make-from-real-imag x y)
  (cond 
    ((and (number? x) (number? y))  (cons x y))
    ((number? x) (make-from-real-imag (- x (imag-part y)) (real-part y)))
    ((number? y) (make-from-real-imag (real-part x) (+ (imag-part x) y)))
    (else (make-from-real-imag (- (real-part x) (imag-part y)) (+ (imag-part x) (real-part y)))) ))

(define (add-complex z1 z2)
  (make-from-real-imag
    (+ (real-part z1) (real-part z2))
    (+ (imag-part z1) (imag-part z2)) ))
(define (sub-complex z1 z2)
  (make-from-real-imag
    (- (real-part z1) (real-part z2))
    (- (imag-part z1) (imag-part z2)) ))
(define (mul-complex z1 z2)
  (make-from-real-imag
    (- (* (real-part z1) (real-part z2)) (* (imag-part z1) (imag-part z2))) 
    (+ (* (imag-part z1) (real-part z2)) (* (real-part z1) (imag-part z2)))))
(define (div-complex z1 z2)
  (make-from-real-imag
    (/ (+ (* (real-part z1) (real-part z2)) (* (imag-part z1) (imag-part z2))) (+ (square (real-part z2)) (square (imag-part z2))))
    (/ (- (* (imag-part z1) (real-part z2)) (* (real-part z1) (imag-part z2))) (+ (square (real-part z2)) (square (imag-part z2)))) ))

(define stringify-complex (lambda (z)
  (cond
    ((= (imag-part z) 0) (number->string (real-part z)))
    ((= (real-part z) 0) 
      (cond 
        ((= (imag-part z) 1) "i")
        ((= (imag-part z) -1) "-i")
        (else (string-append (number->string (imag-part z)) "i")) ))
    ((> (imag-part z) 0) 
      (if (= (imag-part z) 1) 
        (string-append (number->string (real-part z)) "+" "i")
        (string-append (number->string (real-part z)) "+" (number->string (imag-part z)) "i") )) 
    (else 
      (if (= (imag-part z) -1)
        (string-append (number->string (real-part z)) "-" "i")
        (string-append (number->string (real-part z)) (number->string (imag-part z)) "i") )))))
