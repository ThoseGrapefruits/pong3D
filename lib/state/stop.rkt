#lang typed/racket/base

(require "./state.rkt")

(provide stop-state?)

(: stop-state? : State Natural Flonum -> Boolean)
(define (stop-state? s n t)
  (State-Stop? s))
