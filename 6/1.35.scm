(define tolerance 0.00001)

(define (fixed-point f first-gueses)
  (define (close-enough? a b)
    (> tolerance (abs (- b a))))
  (define (try gueses)
    (let ((next (f gueses)))
      (if (close-enough? gueses next)
          gueses
          (try next))))
  (try first-gueses))

