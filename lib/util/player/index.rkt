#lang typed/racket/base

(require
  math/flonum
  pict3d
  "../../config.rkt"
  "../../state/state.rkt"
  "../number/index.rkt")

(provide (all-defined-out))

(: clamp-bumper-y : Flonum -> Flonum)
(define (clamp-bumper-y y)
  (clamp y
         (+ (- WALL-Y) (dir-dy BUMPER-SCALE))
         (- WALL-Y         (dir-dy BUMPER-SCALE))))

(: get-new-player-score : Player Nonnegative-Integer -> Nonnegative-Integer)
(define (get-new-player-score player n)
  (+ (Player-score player)
     (max 0 (fl->exact-integer (ceiling (* (exact->inexact n)
                                 (Player-score-multiplier player)))))))