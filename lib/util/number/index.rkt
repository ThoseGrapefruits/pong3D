#lang typed/racket

(provide (all-defined-out))

(: get-number-place : Integer Integer Integer -> Integer)
(define (get-number-place n over under)
  (exact-floor (/ (exact->inexact (- (modulo n over) (modulo n under)))
                  (exact->inexact under))))

(: clamp : Flonum Flonum Flonum -> Flonum)
(define (clamp x low high)
  (cond
    [(< high low) (error "low must be less than high")]
    [else (max low (min high x))]))

(: digits : (->* (Nonnegative-Integer) (Nonnegative-Integer) (Listof Nonnegative-Integer)))
(define (digits n [radix 10])
  (define-values (q r) (quotient/remainder n radix))
  (cons r (if (= 0 q)
              null
              (digits q radix))))

(: random-0-1 : -> Flonum)
(define (random-0-1)
  (define result (/ (exact->inexact (random 4294967087)) 4294967086.0))
  (cond [(flonum? result) result]
        [else (error "random did not return flonum")]))

(: within? : Flonum Flonum Flonum -> Boolean)
(define (within? x low high)
  (cond
    [(< high low) (error "low must be less than high")]
    [else (and (<= x high)
               (>= x low))]))
