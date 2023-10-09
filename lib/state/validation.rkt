#lang typed/racket/base

(require "./state.rkt")

(provide (all-defined-out))

(: valid-state? : State Natural Flonum -> Boolean)
(define (valid-state? s n t)
  (or (State-Game-Over? s)
      (State-Main-Menu? s)
      (State-Paused? s)
      (State-Play? s)))