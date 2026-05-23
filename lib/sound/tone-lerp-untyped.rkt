#lang racket/base
(require
  rsound
  (only-in racket/math exact-floor)
  "../util/number.rkt")

(provide tone-lerp tone-lerp-double)

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
    [lerped-freq = (lerp last target time-ratio)]
    [_ = (set-box! last-lerp-box lerped-freq)]
    [sin <= sine-wave lerped-freq]
    [out = (* 0.07 sin)]))
  (values siggy
          (λ (target)
            (define targetrounded (round target))
            (when (not (= targetrounded (unbox target-box)))
              (set-box! last-box (unbox last-lerp-box))
              (set-box! target-box targetrounded)
              (set-box! last-time-box (now-millis))))))

(define (tone-lerp-double initial-target-one initial-target-two transition-time)
  (define last-box-one (box initial-target-one))
  (define last-box-two (box initial-target-two))
  (define last-lerp-box-one (box initial-target-one))
  (define last-lerp-box-two (box initial-target-two))
  (define last-time-box-one (box (now-millis)))
  (define last-time-box-two (box (now-millis)))
  (define target-box-one (box initial-target-one))
  (define target-box-two (box initial-target-two))
  (define siggy (network ()
    [now = (now-millis)]
    [last-time-one = (unbox last-time-box-one)]
    [last-time-two = (unbox last-time-box-two)]
    [delta-one = (- now last-time-one)]
    [delta-two = (- now last-time-two)]
    [target-one = (unbox target-box-one)]
    [target-two = (unbox target-box-two)]
    [last-one = (unbox last-box-one)]
    [last-two = (unbox last-box-two)]
    [time-ratio-one = (clamp (/ delta-one transition-time) 0.0 1.0)]
    [time-ratio-two = (clamp (/ delta-two transition-time) 0.0 1.0)]
    [lerped-freq-one = (lerp last-one target-one time-ratio-one)]
    [lerped-freq-two = (lerp last-two target-two time-ratio-two)]
    [_ = (set-box! last-lerp-box-one lerped-freq-one)]
    [_ = (set-box! last-lerp-box-two lerped-freq-two)]
    [sin-one <= sine-wave lerped-freq-one]
    [sin-two <= sine-wave lerped-freq-two]
    [out-one = (* 0.05 sin-one)]
    [out-two = (* 0.05 sin-two)]
    [out = (+ out-one out-two)]))
  (values siggy
          (λ (target-one target-two)
            (define targetrounded-one (round target-one))
            (define targetrounded-two (round target-two))
            (when (not (= targetrounded-one (unbox target-box-one)))
              (set-box! last-box-one (unbox last-lerp-box-one))
              (set-box! target-box-one targetrounded-one)
              (set-box! last-time-box-one (now-millis)))
            (when (not (= targetrounded-two (unbox target-box-two)))
              (set-box! last-box-two (unbox last-lerp-box-two))
              (set-box! target-box-two targetrounded-two)
              (set-box! last-time-box-two (now-millis))))))

;;; Example usage:
;;; (define-values (siggy update) (tone-lerp 500.0 1500.0))
;;; (define-values (siggy2 update2) (tone-lerp 200.0 1500.0))
;;; (signal-play siggy)
;;; (signal-play siggy2)
;;; (sleep 1)
;;; (update 400.0)
;;; (update2 400.0)
;;; (sleep 1)
;;; (update 300.0)
;;; (update2 600.0)
;;; (sleep 1)
;;; (update2 300.0)
;;; (sleep 2)
;;; (stop)
