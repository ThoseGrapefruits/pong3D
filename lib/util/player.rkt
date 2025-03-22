#lang typed/racket/base

(require
  (only-in math/flonum fl->exact-integer)
  (only-in pict3d dir-dy)
  "../config.rkt"
  "../state/state.rkt"
  "./number.rkt")

(provide (all-defined-out))

(: clamp-bumper-y : Flonum -> Flonum)
(define (clamp-bumper-y y)
  (clamp y
         (+ (- WALL-Y) (dir-dy PADDLE-SCALE))
         (- WALL-Y     (dir-dy PADDLE-SCALE))))

(: get-new-player-score : Player Nonnegative-Integer -> Nonnegative-Integer)
(define (get-new-player-score player n)
  (+ (Player-score player)
     (max 0 (fl->exact-integer (ceiling (* (exact->inexact n)
                                 (Player-score-multiplier player)))))))
