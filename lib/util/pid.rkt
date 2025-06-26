#lang typed/racket/base

(provide PID
         make-pid
         pid-step!
         pid-reset!)

; A proportional-integral-derivative controller
; https://en.wikipedia.org/wiki/Proportional–integral–derivative_controller

(define-struct PID
  ([tuning-p     : Flonum]
   [tuning-i     : Flonum]
   [tuning-d     : Flonum]
   [err-integral : (Boxof Flonum)]
   [err-last     : (Boxof Flonum)])
  #:transparent)

(: make-pid :
   [#:tuning-p Flonum]
   [#:tuning-i Flonum]
   [#:tuning-d Flonum] -> PID)
(define (make-pid #:tuning-p [tuning-p #i0]
                  #:tuning-i [tuning-i #i0]
                  #:tuning-d [tuning-d #i0])
  (PID tuning-p tuning-i tuning-d (box #i0) (box #i0)))

(: pid-reset! : PID -> Void)
(define (pid-reset! pid)
  (define err-integral (PID-err-integral pid))
  (define err-last     (PID-err-last pid))
  (box-cas! err-integral (unbox err-integral) #i0)
  (box-cas! err-last     (unbox err-last)     #i0)
  (void))

(: pid-step! : PID Flonum Flonum -> Flonum)
(define (pid-step! pid err dt)
  (define err-p err)
  (define err-i (+ (unbox (PID-err-integral pid))
                   (* err dt)))
  (define err-d (/ (- err (unbox (PID-err-last pid))) dt))

  (define err-integral (PID-err-integral pid))
  (define err-last     (PID-err-last pid))
  (box-cas! err-integral (unbox err-integral) err-i)
  (box-cas! err-last     (unbox err-last)     err)

  (+ (* (PID-tuning-p pid) err-p)
     (* (PID-tuning-i pid) err-i)
     (* (PID-tuning-d pid) err-d)))
