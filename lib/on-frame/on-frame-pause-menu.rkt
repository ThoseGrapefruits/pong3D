#lang typed/racket/base

(require "../state/state.rkt")

(provide (all-defined-out))

(: on-frame-pause-menu : State -> State)
(define (on-frame-pause-menu s)
  (cond [(State-Pause-Menu? s) s]
        [else s]))