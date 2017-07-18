(define-syntax stream-cons
    (syntax-rules ()
        ((_ x y) (cons x (delay y)))
    ))

(define-syntax stream-car
    (syntax-rules ()
        ((_ x) (car x ))
    ))

(define-syntax stream-cdr
    (syntax-rules ()
        ((_ x) (force (cdr x)))
    ))

(define numbers (lambda ()
  (letrec ((stream
              (lambda (n) (stream-cons n (stream (+ n 1))))
          ))
          (stream 2))))

(define head (lambda (n L)
  (if (<= n 0) '()
      (cons (stream-car L) (head (- n 1) (stream-cdr L)))
      )))

(define sieve (lambda (num L)
  (if (equal? (modulo (stream-car L) num) '0) (sieve num (stream-cdr L))
      (stream-cons (stream-car L) (sieve num (stream-cdr L)))
      )))

(define primes (lambda (n L)
  (if (<= n 0) '()
      (cons (stream-car L) (primes (- n 1) (sieve (stream-car L) L) ) )
      )))

(head 5 (numbers))
(head 10 (sieve 2 (numbers)))
(primes 50 (numbers))
