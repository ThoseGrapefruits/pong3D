#lang typed/racket/base

(require
  pict3d
  "./config.rkt"
  "./state/setters.rkt"
  "./state/state.rkt")

(provide (all-defined-out))

(: on-mouse : State Natural Flonum Integer Integer String -> State)
(define (on-mouse s n t x y e)
  (cond [(State-Play? s) (on-mouse-game-play s n t x y e)]
        [else s]))

(: on-mouse-game-play : State-Play Natural Flonum Integer Integer String -> State)
(define (on-mouse-game-play s n t x y e)
  (cond [(string=? e "move")
         (State-set-player-position
          s
          (- (* 1.6 (/ (exact->inexact x) SCREEN-WIDTH)) 0.8))]
        [else s]))
