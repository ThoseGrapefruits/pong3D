#lang typed/racket/base

(require
  pict3d
  "./state.rkt")

(provide (all-defined-out))

; Get the y-offset between the center a bumper and a ball
(: State-get-contact-offset-y : State-Play Flonum -> Flonum)
(define (State-get-contact-offset-y s y)
  (- (pos-y (Ball-pos (State-Play-ball s))) y))
