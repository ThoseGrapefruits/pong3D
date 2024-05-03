#lang typed/racket/base

(require "./config.rkt"
         "./state/state.rkt")

(provide (all-defined-out))

(: on-mouse : State Natural Flonum Integer Integer String -> State)
(define (on-mouse s n t x y e)
  (cond [(State-Play? s) (on-mouse-game-play s n t x y e)]
        [else s]))

(: on-mouse-game-play : State-Play Natural Flonum Integer Integer String -> State)
(define (on-mouse-game-play s n t x y e)
  (cond [(string=? e "move")
         (struct-copy
          State-Play s
          [player (struct-copy
                   Player (State-Play-player s)
                   [y-desired (- (* 1.6 (/ (exact->inexact x) SCREEN-WIDTH)) 0.8)])])]
        [else s]))
