#lang typed/racket/base

(require "../state/state.rkt")

(provide (all-defined-out))

(: on-frame-paused : State -> State)
(define (on-frame-paused s)
  (cond [(State-Paused? s) s]
        [else s]))