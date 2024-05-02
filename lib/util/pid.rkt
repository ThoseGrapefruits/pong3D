#lang typed/racket/base

(provide pid)

; A Proportional-integral-derivative controller
; https://en.wikipedia.org/wiki/Proportional–integral–derivative_controller


(: pid : Flonum Flonum Flonum -> (-> Flonum Flonum Flonum))
(define (pid tuning-p tuning-i tuning-d)
  (define err-integral (box 0.0))
  (define err-last     (box 0.0))

  (λ (err dt)
    (define err-p err)
    (define err-i (+ (unbox err-integral)
                     (* err dt)))
    (define err-d (/ (- err (unbox err-last)) dt))

    (set-box! err-integral err-i)
    (set-box! err-last err)
    (+ (* tuning-p err-p)
       (* tuning-i err-i)
       (* tuning-d err-d))))