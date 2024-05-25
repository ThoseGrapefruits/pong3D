#lang typed/racket/base

(require "./state.rkt")

(provide (all-defined-out))

(: valid-state? : State Natural Flonum -> Boolean)
(define (valid-state? s n t)
  (define is-init (zero? n))
  (cond [is-init (valid-state-init? s n t)]
        [else    (valid-state-run? s n t)]))

(: valid-state-init? : State Natural Flonum -> Boolean)
(define (valid-state-init? s n t)
  (or (State-Game-Over? s)
      (State-Main-Menu? s)
      (State-Pause-Menu? s)
      (State-Play? s)))

(: valid-state-run? : State Natural Flonum -> Boolean)
(define (valid-state-run? s n t)
  (or (State-Game-Over? s)
      (State-Main-Menu? s)
      (and (State-Pause-Menu? s)
           ; required if paused
           (State-Pause-Menu-resume-state s)
           #t)
      (State-Play? s)))
