#lang typed/racket/base

(require pict3d
         "../state/state.rkt"
         "./number/index.rkt")

(provide within-bounds?)

(: within-bounds? : Pos Bounds -> Boolean)
(define (within-bounds? p bounds)
  (define bound1 (car bounds))
  (define bound2 (cdr bounds))
  (and bound1
       bound2
       (within? (pos-x p) (pos-x bound1) (pos-x bound2))
       (within? (pos-y p) (pos-y bound1) (pos-y bound2))
       (within? (pos-z p) (pos-z bound1) (pos-z bound2))))