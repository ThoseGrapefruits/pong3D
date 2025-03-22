#lang typed/racket/base

(require
  (only-in racket/match match-define)
  (only-in typed-compose compose-n)
  "../state/state.rkt"
  "../state/updaters.rkt"
  "./on-frame-pause-menu.rkt"
  "./on-frame-play.rkt")

(provide on-frame)

(: on-frame : State-Any Natural Flonum -> State-Any)
(define (on-frame s n t)
  (match-define (cons mouse-x mouse-y) (State-mouse-pos-last s))
  ((compose-n ; bottom-to-top
    on-frame-pause-menu
    on-frame-play
    (λ ([s : State-Any]) : State-Any
      (State-update-trace-mouse s mouse-x mouse-y))
    (λ ([s : State-Any]) : State-Any
      (State-update-counters s n t)))
   s))
