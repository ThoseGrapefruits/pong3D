#lang typed/racket/base

(require
  "../state/menu.rkt"
  "../state/state-base.rkt")

(provide ANIMATION-TIME
         Menu-Item-scale)

(: SCALE-MAX : Flonum)
(define SCALE-MAX 0.05)

(: ANIMATION-TIME : Flonum)
(define ANIMATION-TIME 140.0)

(: Menu-Item-scale : State Menu Menu-Item -> Flonum)
(define (Menu-Item-scale s menu menu-item)
  (define active-start (unbox (Menu-Item-active-start menu-item)))
  (define active-end   (unbox (Menu-Item-active-end   menu-item)))
  (define t (State-t s))

  (: time-since : Flonum)
  (define time-since
    (cond [active-end
          ; If Menu-Item stopped being active, we scale back to 1.0.
           (max 0.0            (- ANIMATION-TIME (- t active-end)))]
          [active-start
           (min ANIMATION-TIME (- t active-start))]
          [else 0.0]))

  (+ 1.0 (* SCALE-MAX (/ time-since ANIMATION-TIME))))
