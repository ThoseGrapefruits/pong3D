#lang typed/racket/base

(require
  typed-compose
  "../state/state.rkt"
  "../state/updaters.rkt"
  "./on-frame-paused.rkt"
  "./on-frame-play.rkt")

(provide on-frame)

(: on-frame : State Natural Flonum -> State)
(define (on-frame s n t)
  ((compose-n
    (λ ([s : State]) (State-update-counters s n t))
    on-frame-paused
    on-frame-play)
   s))
