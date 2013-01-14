(define attach-tag (lambda (type-tag contents)
  (if (eq? type-tag 'scheme-number)
    contents
    (cons type-tag contents))))
(define type-tag (lambda (datum)
  (cond
    ((pair? datum) (car datum))
    ((number? datum) 'scheme-number)
    (else (error "Bad tagged datum -- TYPE-TAG" datum)))))
(define contents (lambda (datum)
  (cond
    ((pair? datum) (cdr datum))
    ((number? datum) datum)
    (else (error "Bad tagged datum -- CONTENTS" datum)))))

(define add (lambda (x y) (apply-generic 'add x y)))
(define sub (lambda (x y) (apply-generic 'sub x y)))
(define mul (lambda (x y) (apply-generic 'mul x y)))
(define div (lambda (x y) (apply-generic 'div x y)))
(define raise (lambda (x) (apply-generic 'raise x)))

;;scheme-number
(define (install-scheme-number-package)
  (define tag (lambda (x) (attach-tag 'scheme-number x)))
  (put 'add '(scheme-number scheme-number)
    (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
    (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
    (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
    (lambda (x y) (tag (/ x y))))
  (put 'make '(scheme-number)
    (lambda (x) (tag x)))
  (put 'equ? '(scheme-number scheme-number)
    (lambda (x y) (eq? x y)))
  (put '=zero? '(scheme-number)
    (lambda (x) (eq? x 0)))
  (put 'raise '(scheme-number)
    (lambda (x) (make-rational (contents x) 1)))
  'done
)


(define make-scheme-number (lambda (n)
  ((get 'make '(scheme-number)) n)))


;;rational
(define (install-rational-package)
  ;private
  (define numer (lambda (x) (car x)))
  (define denom (lambda (x) (cdr x)))
  (define make-rat (lambda (n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g)))))
  (define numer (lambda (x) (car x)))
  (define denom (lambda (x) (cdr x)))
  (define add-rat (lambda (x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y)))))
  (define sub-rat (lambda (x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y)))))
  (define mul-rat (lambda (x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y)))))
  (define div-rat (lambda (x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y)))))
  (define equ? (lambda (x y) (apply-generic 'equ? x y)))
  (define =zero? (lambda (x) (apply-generic '=zero? x)))

  ;others
  (define tag (lambda (x) (attach-tag 'rational x)))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mel-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'make '(rational)
       (lambda (n d) (tag (make-rat n d))))
  (put 'equ? '(rational rational)
    (lambda (x y) (eq? x y)))
  (put '=zero? '(rational)
    (lambda (x) (eq? (numer x) 0)))
  (put 'raise '(rational)
    (lambda (x) (make-real (/ (numer x) (denom x)))))
  'done
)

(define make-rational (lambda (n d)
  ((get 'make '(rational)) n d)))

;;real
(define (install-real-package)
  (define tag (lambda (x) (attach-tag 'real x)))
  (put 'add '(real real)
    (lambda (x y) (tag (+ x y))))
  (put 'sub '(real real)
    (lambda (x y) (tag (- x y))))
  (put 'mul '(real real)
    (lambda (x y) (tag (* x y))))
  (put 'div '(real real)
    (lambda (x y) (tag (/ x y))))
  (put 'make '(real)
    (lambda (x) (tag x)))
  (put 'equ? '(real real)
    (lambda (x y) (eq? x y)))
  (put '=zero? '(real)
    (lambda (x) (eq? x 0)))
  (put 'raise '(real)
    (lambda (x) (make-complex-from-real-imag (contents x) 0)))
  'done
)

(define make-real (lambda (n)
  ((get 'make '(real)) n)))


;;complex
(define (install-complex-package)
  ;from rectangular and polar
  (define make-from-real-imag (lambda (x y)
    ((get 'make-from-real-imag 'rectangular) x y)))
  (define make-from-mag-ang (lambda (r a)
    ((get 'make-from-mag-ang 'polar) r a)))
  
  ;private
  (define add-complex (lambda (z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2)))))
  (define sub-complex (lambda (z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
                         (- (imag-part z1) (imag-part z2)))))
  (define mul-complex (lambda (z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                       (+ (angle z1) (angle z2)))))
  (define div-complex (lambda (z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                       (- (angle z1) (angle z2)))))
  (define equ? (lambda (x y) (apply-generic 'equ? x y)))
  (define =zero? (lambda (x) (apply-generic '=zero? x)))


  ;others
  (define tag (lambda (z) (attach-tag 'complex z)))
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'make-complex-from-real-imag '(complex)
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-complex-from-mag-ang '(complex)
       (lambda (r a) (tag (make-from-mag-ang r a))))
  (put 'equ? '(complex complex)
    (lambda (x y) (eq? x y)))
  (put '=zero? '(complex)
    (lambda (x) (apply-generic '=zero? x)))
  (put 'add '(complex scheme-number)
    (lambda (z x) (tag (add-complex-to-schemenum z x))))
  'done
)

(define make-complex-from-real-imag (lambda (x y)
  ((get 'make-complex-from-real-imag '(complex)) x y)))
(define make-complex-from-mag-ang (lambda (r a)
  ((get 'make-complex-from-mag-ang '(complex)) r a)))

(define real-part (lambda (z) (apply-generic 'real-part z)))
(define imag-part (lambda (z) (apply-generic 'imag-part z)))
(define magnitude (lambda (z) (apply-generic 'magnitude z)))
(define angle (lambda (z) (apply-generic 'angle z)))

(define rectangular? (lambda (z)
  (eq? (type-tag z) 'rectangular)))
(define polar? (lambda (z)
  (eq? (type-tag z) 'polar)))

;rectangular
(define (install-rectangular-package)
  ;private
  (define real-part (lambda (z) (car z)))
  (define imag-part (lambda (z) (cdr z)))
  (define magnitude (lambda (z)
   (sqrt (+ (square (real-part z))
            (square (imag-part z))))))
  (define angle (lambda (z)
   (atan (imag-part z) (real-part z))))
  (define make-from-real-imag (lambda (x y) (cons x y)))
  (define make-from-mag-ang (lambda (r a)
    (cons (* r (cos a)) (* r (sin a)))))
  (define equ? (lambda (x y) (apply-generic 'equ? x y)))
  (define =zero? (lambda (x) (apply-generic '=zero? x)))

  ;others
  (define tag (lambda (x) (attach-tag '(rectangular) x)))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
    (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
    (lambda (r a) (tag (make-from-mag-ang r a))))
  (put 'equ? '()
    (lambda (x y) (eq? x y)))
  (put '=zero? '()
    (lambda (x) (eq? 0)))
  'done
)

;polar
(define (install-polar-package)
  ;private
  (define magnitude (lambda (z) (car z)))
  (define angle (lambda (z) (cdr z)))
  (define make-from-mag-ang (lambda (r a) (cons r a)))
  (define real-part (lambda (z)
    (* (magnitude z) (cos (angle z)))))
  (define imag-part (lambda (z)
    (* (magnitude z) (sin (angle z)))))
  (define make-from-real-imag (lambda (x y)
    (cons (sqrt (+ (square x) (square y)))
          (atan y x))  ))
  (define equ? (lambda (x y) (apply-generic 'equ? x y)))
  (define =zero? (lambda (x) (apply-generic '=zero? x)))

  ;others
  (define tag (lambda (x) (attach-tag 'polar x)))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
    (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
    (lambda (r a) (tag (make-from-mag-ang r a))))
  (put 'equ? '(polar polar)
    (lambda (x y) (eq? x y)))
  (put '=zero? '(polar)
    (lambda (x) (eq? (magnitude z) 0)))
  'done
)

(define apply-generic (lambda (op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
        (apply proc (map contents args))
        (if (= (length args) 2)
          (let* ((type1 (car type-tags))
                (type2 (cadr type-tags))
                (a1 (car args))
                (a2 (cadr args))
                (t1->t2 (get-coercion type1 type2))
                (t2->t1 (get-coercion type2 type1)))
            (cond (t1->t2
                        (appley-generic op (t1->t2 a1) a2))
                      (t2->t1
                        (appley-generic op a1 (t1->t2 a1)))
                      (else
                        (error
            "No method for these types -- APPLY-GENERIC"
                   (list op type-tags)))))
          (error
          "No method for these types -- APPLY-GENERIC"
                 (list op type-tags))))))))


;install
(install-scheme-number-package)
(install-rational-package)
(install-real-package)
(install-complex-package)
(install-polar-package)
(install-rectangular-package)
