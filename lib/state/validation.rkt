#lang typed/racket/base

(require "./state.rkt")

(provide valid-state?)

(: valid-state? : State Natural Flonum -> Boolean)
(define (valid-state? s n t)
  (cond [(zero? n) (valid-state-init? s n t)]
        [else      (valid-state-run?  s n t)]))

(: valid-state-init? : State Natural Flonum -> Boolean)
(define (valid-state-init? s n t)
  (State-Any? s))

(: valid-state-run? : State Natural Flonum -> Boolean)
(define (valid-state-run? s n t)
  (or (and (State-Play? s)
           (or (not (State-Play-pause-state s))
               (not (State-Pause-Menu-resume-state
                     (State-Play-pause-state s)))))
      (and (State-Game-Over? s))
      (and (State-Main-Menu? s))
      (and (State-Pause-Menu? s)
           ; required if paused
           (State-Pause-Menu-resume-state s)
           #t)
      (and (State-Play? s)
           (or (not (State-Play-pause-state s))
               (not (State-Pause-Menu-resume-state
                     (State-Play-pause-state s)))))
      (and (State-Stop? s))))