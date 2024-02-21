#lang typed/racket

(provide
 scale--1-1
 wrap-within)

; Scale a [-1,1] value up to a range of the given width.
(: scale--1-1 : Flonum Integer -> Flonum)
(define (scale--1-1 n width)
  (define half-width (/ (exact->inexact width) 2.0))
  (* (+ n 1.0) half-width))

; wrap-within
(: wrap-within : Flonum Flonum -> Flonum)
(define (wrap-within n width)
  (cond [(negative? width) (error "negative width")]
        [(or (> n width) (< n (- width))) (error "n outside screen bounds")]
        [(negative? n) (+ width n)]
        [else n]))
