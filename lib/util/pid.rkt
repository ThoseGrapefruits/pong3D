#lang typed/racket/base

(provide PID make-pid pid-step! pid-reset!)

; A Proportional-integral-derivative controller
; https://en.wikipedia.org/wiki/Proportional–integral–derivative_controller

(define-struct PID
  ([tuning-p     : Flonum]
   [tuning-i     : Flonum]
   [tuning-d     : Flonum]
   [err-integral : (Boxof Flonum)]
   [err-last     : (Boxof Flonum)]))

(: make-pid :
   [#:tuning-p Flonum]
   [#:tuning-i Flonum]
   [#:tuning-d Flonum] -> PID)
(define (make-pid #:tuning-p [tuning-p 0.0]
                  #:tuning-i [tuning-i 0.0]
                  #:tuning-d [tuning-d 0.0])
  (PID tuning-p tuning-i tuning-d (box 0.0) (box 0.0)))

(: pid-reset! : PID -> Void)
(define (pid-reset! pid)
  (set-box! (PID-err-integral pid) 0.0)
  (set-box! (PID-err-last pid) 0.0))

(: pid-step! : PID Flonum Flonum -> Flonum)
(define (pid-step! pid err dt)
  (define err-p err)
  (define err-i (+ (unbox (PID-err-integral pid))
                   (* err dt)))
  (define err-d (/ (- err (unbox (PID-err-last pid))) dt))

  (set-box! (PID-err-integral pid) err-i)
  (set-box! (PID-err-last pid) err)
  (+ (* (PID-tuning-p pid) err-p)
     (* (PID-tuning-i pid) err-i)
     (* (PID-tuning-d pid) err-d)))
