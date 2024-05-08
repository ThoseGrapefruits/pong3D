#lang typed/racket/base

(require
  "./state.rkt"
  "./syntax.rkt")

(provide (all-defined-out))

(: State-update-counters : State Natural Flonum -> State)
(define (State-update-counters s n t)
  (State-update-parent
   s
   [dt #:parent State (- t (State-t s)) ]
   [n #:parent State n]
   [t #:parent State t]))
