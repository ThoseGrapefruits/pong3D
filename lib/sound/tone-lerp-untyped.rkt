#lang racket/base
(require
  rsound
  "../util/number.rkt")

(provide tone-lerp)

(define now-millis current-inexact-monotonic-milliseconds)

;;; tone-lerp : Flonum Flonum -> (values RSignal SimpleProcedure) ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; initial-target : The initial target frequency in Hz.
;;; transition-time : The time in milliseconds it takes to transition to the new target frequency.
;;; Returns a tuple containing a signal and a procedure to update the target frequency.
(define (tone-lerp initial-target transition-time)
  (define last-box (box initial-target))
  (define last-lerp-box (box initial-target))
  (define last-time-box (box (now-millis)))
  (define target-box (box initial-target))
  (define siggy (network ()
    [now = (now-millis)]
    [last-time = (unbox last-time-box)]
    [delta = (- now last-time)]
    [target = (unbox target-box)]
    [last = (unbox last-box)]
    [time-ratio = (clamp (/ delta transition-time) 0.0 1.0)]
    [lerp = (lerp last target time-ratio)]
    [_ = (set-box! last-lerp-box lerp)]
    [sin <= sine-wave lerp]
    [out = (* 0.1 sin)]))
  (signal-play siggy)
  (values siggy
        (lambda (target)
          (set-box! last-box (unbox last-lerp-box))
          (set-box! target-box target)
          (set-box! last-time-box (now-millis)))))

;;; (define-values (siggy update) (tone-lerp 500.0 1500.0))
;;; (sleep 1)
;;; (update 400.0)
;;; (sleep 1)
;;; (update 300.0)
;;; (sleep 1)
;;; (stop)
