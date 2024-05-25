#lang typed/racket/base

(require
  racket/match
  typed-compose
  "../state/state.rkt"
  "../state/updaters.rkt"
  "./on-frame-pause-menu.rkt"
  "./on-frame-play.rkt")

(provide on-frame)

(: on-frame : State Natural Flonum -> State)
(define (on-frame s n t)
  (match-define (cons mouse-x mouse-y) (State-mouse-pos-last s))
  ((compose-n ; bottom-to-top
    on-frame-pause-menu
    on-frame-play
    (λ ([s : State]) (State-update-mouse-trace s mouse-x mouse-y))
    (λ ([s : State]) (State-update-counters s n t)))
   s))
