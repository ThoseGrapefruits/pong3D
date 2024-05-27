#lang typed/racket/base

(require "./state.rkt")

(provide (all-defined-out))

(: valid-state? : State Natural Flonum -> Boolean)
(define (valid-state? s n t)
  (cond [(zero? n) (valid-state-init? s n t)]
        [else      (valid-state-run?  s n t)]))

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
      (and (State-Play? s)
           (or (not (State-Play-pause-state s))
               (not (State-Pause-Menu-resume-state
                     (State-Play-pause-state s)))))))